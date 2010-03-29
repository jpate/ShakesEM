/**
* ShakesEM is a package for running EM for Probabilistic Context Free Grammars,
* implemented in Scala using the Actors library to allow parallel processing
* over multiple machines and multiple cores. This version implements both
* vanilla EM and the modification to EM in Pereira and Schabes (1992) for
* partially bracketed corpora
*
* Copyright 2010 John K Pate
* Distributed under the GNU General Public License
* 
*
* This program is free software: you can redistribute it and/or modify it under
* the terms of the GNU General Public License as published by the Free Software
* Foundation, either version 3 of the License, or (at your option) any later
* version. This program is distributed in the hope that it will be useful, but
* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
* details. You should have received a copy of the GNU General Public License along
* with this program. If not, see <http://www.gnu.org/licenses/>.
*
*
* @version 0.20_remote
* @author John K Pate
*/
package ShakesEM {
  import scala.actors.Actor
  import scala.actors.Actor._
  import collection.immutable.{HashMap => IHashMap,HashSet => IHashSet}
  import collection.mutable.{HashMap => MHashMap, HashSet => MHashSet}

  /**
  * <code>ShakesPCNF</code> defines a Probabilistic Chomsky Normal Form grammar
  * for use with the ShakesEM library
  */
  @serializable class ShakesPCNF {
    import math._


    /**
    * Used in re-estimation of the PCFG. f Sums estimated counts for binary
    * branching nodes, g for unary branching nodes, and h for any non-terminal
    * node.
    * See Manning &amp; Schutze p. 396.
    * The defaults just make the incrementation code cleaner.
    */
    val f = new MHashMap[
      String,   
      MHashMap[
        String,
        MHashMap[String,Double]
      ]
    ] {
      override def default(lhs:String) = {
        this += Pair( lhs,
                      new MHashMap[String, MHashMap[String,Double]] {
            override def default(left:String) = {
              this += Pair( left,
                            new MHashMap[String,Double] {
                   override def default(right:String) = 0
                }
              )
              this(left)
            }
          }
        )
        this(lhs)
      }
    }
    val g = new MHashMap[(String,String),Double] {
      override def default(key:(String,String)) = 0
    }
    val h = new MHashMap[String,Double] {
      override def default(key:String) = 0
    }


    //var reestimateCounter = 0

    /**
    * Re-estimates a PCFG based on the counts in f, g, and h. All the action of
    * this function lies in its side-effects.
    */
    def reestimateRules {
      //reestimateCounter = reestimateCounter + 1
      //println("REESTIMATION NUMBER" + reestimateCounter)
      f.keysIterator.foreach{ lhs =>
        f (lhs) .keysIterator.foreach{ left =>
          f (lhs)(left) .keysIterator.foreach{ right =>
            phrases (lhs)(left)(right) =
              100000 * f (lhs)(left)(right) /
              h (lhs)
            if( phrases(lhs)(left)(right) == 0.0 ) {
              phrases(lhs)(left) -= right
              println("Removed a rule" + (lhs,left,right))
            }
          }
          if( phrases(lhs)(left).isEmpty )
            phrases(lhs) -= left
        }
        if( phrases(lhs).isEmpty )
          phrases -= lhs
      }



      g.keysIterator.foreach{ k =>
        lexicon (k._1) (k._2) =
           100000 * g(k) /
           h(k._1)
      }
      lexicon.keysIterator.foreach{ pos =>
        lexicon(pos).keysIterator.foreach{ word =>
          if( lexicon(pos)(word) == 0.0 ) {
            lexicon(pos) -= word
            println("Removed a rule" + (pos,word))
          }
        }
        if( lexicon(pos).isEmpty )
          lexicon -= pos
      }



      f.clear
      g.clear
      h.clear
      normalize
      preCalcExps
    }


    def normalize {
      phrases.keysIterator.foreach{ lhs =>
        var lhsTotal = 0D
        phrases(lhs).keysIterator.foreach( left =>
          phrases(lhs)(left).keysIterator.foreach( right =>
              lhsTotal = lhsTotal + phrases(lhs)(left)(right)
          )
        )
        phrases(lhs).keysIterator.foreach( left =>
          phrases(lhs)(left).keysIterator.foreach( right =>
            phrases(lhs)(left)(right) = phrases(lhs)(left)(right) / lhsTotal
          )
        )
      }

      lexicon.keysIterator.foreach{ pos =>
        var posTotal = 0D
        lexicon( pos ).keysIterator.foreach( word =>
          posTotal = posTotal + lexicon(pos)(word)
        )
        lexicon(pos).keysIterator.foreach( word =>
          lexicon(pos)(word) = lexicon(pos)(word) / posTotal
        )
      }
    }


    def randomizeGrammar( nonTermCount:Int, termSymbols:List[String],
                          randSeed:Int, centeredOn:Int) {
      import scala.util.Random

      val nonTermSymbols = "S" :: (
         ( (1 to nonTermCount - 1) toList ) map ( "N" + _ )
      )

      val r = new Random( randSeed )

      val allSymbols:List[String] = nonTermSymbols ::: termSymbols

      for( lhs <- nonTermSymbols ) 
        for( left <- allSymbols )
          for( right <- allSymbols )
            phrases(lhs)(left)(right) = r.nextDouble + centeredOn


      for( pos <- nonTermSymbols )
        for( word <- termSymbols )
          lexicon( pos ) ( word ) = r.nextDouble + centeredOn

      normalize
      preCalcExps
    }


    /**
    * <p>Deletes re-write rules which have a probablity less than some threshold.
    * Could be useful for speeding up estimation and saving memory in later
    * iterations.</p>
    * 
    * @param cutoff The minimum probability for a re-write to remain.
    */
    def pruneRules(cutoff:Double) {
      phrases.keysIterator.foreach{ lhs =>
        phrases (lhs) .keysIterator.foreach{ left =>
          phrases (lhs) (left) .keysIterator.foreach{ right =>
            if( phrases(lhs)(left)(right) < cutoff )
              phrases(lhs)(left) -= right
          }
          if( phrases(lhs)(left).size == 0 )
            phrases(lhs) -= left
        }
        if( phrases (lhs) .size == 0 )
          phrases -= lhs
      }
      lexicon.keysIterator.foreach{ pos =>
        lexicon (pos) .keysIterator.foreach{ word =>
          if( lexicon(pos)(word) < cutoff )
            lexicon(pos) -= word
        }
        if( lexicon (pos) .size == 0 )
          lexicon -= pos
      }
      preCalcExps
    }



    /**
    * <p>lexicon, lexExps, phrases, and phrExps contain the actual grammatical
    * information. lexExps contains the same information as lexicon in a
    * different format to facilitate rule look-up. Similarly, phrExps contains
    * the same information as phrases in a different format to facilitate rule
    * look-up. The default values just help make incrementation easier.<p>
    */
    var lexicon = new MHashMap[
      String,       //  POS
      MHashMap[
        String,     //  WORD
        Double      //  PROB
      ]
    ] {
      override def default(pos:String) = {
        this += Pair( pos,
              new MHashMap[String,Double] {
            override def default(word:String) = {
              this += Pair( word, 0.0 )
              this(word)
            }
          }
        )
        this(pos)
      }
    }
    var lexExps = new MHashMap[
      String,       //  WORD
      List[(
        String,     //  POS
        Double      //  PROB
      )]
    ] {
      override def default(word:String) = {
        this += Pair( word, Nil )
        this(word)
      }
    }
    
    var phrases = new MHashMap[
      String,       //  LHS
      MHashMap[
        String,     //  LEFT
        MHashMap[
          String,   //  RIGHT
          Double    //  PROB
        ]
      ]
    ] {
      override def default(lhs:String) = {
        this += Pair( lhs,
                      new MHashMap[String, MHashMap[String,Double]] {
            override def default(left:String) = {
              this += Pair( left, 
                            new MHashMap[String,Double] {
                  override def default(right:String) = {
                    this += Pair( right, 0.0 )
                    this(right)
                  }
                }
              )
              this(left)
            }
          }
        )
        this(lhs)
      }
    }
    var phrExps = new MHashMap[
      String,       //  LEFT
      MHashMap[
        String,     //  RIGHT
        List[(
          String,   //  LHS
          Double    //  PROB
        )]
      ]
    ] {
      override def default(left:String) = {
        this += Pair( left,
                      new MHashMap[String, List[(String,Double)]] {
            override def default(right:String) = {
              this += Pair( right, Nil )
              this(right)
            }
          }
        )
        this(left)
      }
    }


    /**
    * Reads a grammar from a file. Grammar should be in the format:
    * <br/><br/>
    * <code>parent left-child right-child probability</code>
    * <br/><br/>
    * and contain only binary rules.
    * 
    * @param gramPath File path to the grammar (relative to current working
    * directory)
    */
    def readGrammar(gramPath:String) {
      import scala.io.Source._
      val lines = fromPath(gramPath).getLines("\n")
      lines.foreach( line =>
        if( line.length > 1 ) {
          val fields = line.split(' ')

          phrases (fields(0)) (fields(1)) (fields(2)) += fields(3).toDouble
          phrExps (fields(1)) (fields(2)) = (fields(0), fields(3).toDouble) ::
                                            phrExps (fields(1)) (fields(2))
        }
      )
    }

    /**
    * Reads a lexicon from a file. Lexicon should be in the format:
    * <br/><br/>
    * <code>part-of-speech word probability</code>
    * <br/><br/>
    * and contain only unary rules.
    * 
    * @param gramPath File path to the grammar (relative to current working
    * directory)
    */
    def readLexicon(lexPath:String) {
      import scala.io.Source._
      val lines = fromPath(lexPath).getLines("\n")
      lines.foreach( line =>
        if( line.length > 1 ) {
          val fields = line.split(' ')
          lexicon (fields(0)) (fields(1)) += fields(2).toDouble
          lexExps (fields(1)) = (fields(0), fields(2).toDouble) ::
                                lexExps(fields(1))

        }
      )
    }

    /**
    * Receives a probability in log space, takes it out of log space, and
    * increments the probability of a binary rule by the probability.
    *
    * @param lhs The left hand side of the rule
    * @param left The left child of the rule
    * @param right The right child of the rule
    * @param scoreIncrem The amount to increment the probability by (in log
    * space)
    */
    def incPhraseScore(lhs:String,left:String,right:String,scoreIncrem:Double) {
      phrases (lhs) (left) (right) += exp(scoreIncrem)
    }

    /**
    * Receives a probability and increments the probability of a binary rule by
    * the probability.
    *
    * @param lhs The left hand side of the rule
    * @param left The left child of the rule
    * @param right The right child of the rule
    * @param probIncrem The amount to increment the probability by
    */
    def incPhraseProb(lhs:String,left:String,right:String,probIncrem:Double) {
      phrases(lhs) (left) (right) += probIncrem
    }
    
    /**
    * Receives a probability in log space, takes it out of log space, and
    * increments the probability of a unary rule by the probability.
    *
    * @param pos The part of speech of the rule
    * @param word The terminal production of the rule
    * @param scoreIncrem The amount to increment the probability by (in log
    * space)
    */
    def incLexScore(pos:String,word:String,scoreIncrem:Double) {
      lexicon(pos) (word) += exp(scoreIncrem)
    }

    /**
    * Receives a probability and increments the probability of a unary rule by
    * the probability.
    *
    * @param pos The part of speech of the rule
    * @param word The terminal production of the rule
    * @param probIncrem The amount to increment the probability by 
    */
    def incLexProb(pos:String,word:String,probIncrem:Double) {
      lexicon(pos) (word) += probIncrem
    }
    
    /**
    * Computes the value of phrExps on the basis of phrases and the value of
    * lexExps on the basis of lexicon. Basically, keep these values in two forms
    * to save computation time at the expense of space.
    */
    def preCalcExps {
      phrExps.clear
      lexExps.clear

      phrExps.keysIterator.foreach{ phrExps(_).clear }
      phrases.keysIterator.foreach( lhs =>
        phrases (lhs) .keysIterator.foreach( left =>
          phrases (lhs) (left).keysIterator.foreach ( right =>
            phrExps (left) (right) = (lhs, phrases(lhs)(left)(right)) ::
                                      phrExps (left) (right)
          )
        )
      )

      lexicon.keysIterator.foreach( pos =>
        lexicon (pos) .keysIterator.foreach ( word =>
          lexExps(word) = (pos, lexicon(pos)(word)) ::
                          lexExps(word)
        )
      )
    }

    /**
    * Returns a ShakesPCNF which has the same lexicon, phrases, lexExps, and
    * phrExps but zero counts
    *
    * @return A ShakesPCNF with the same lexicon, phrases, lexExps, and phrExps
    * as the ShakesPCNF calling the function, but with all zero counts.
    */
    def countlessCopy:ShakesPCNF = {
      val copy = new ShakesPCNF
      phrases.keysIterator.foreach( lhs =>
        phrases (lhs) .keysIterator.foreach( left =>
          phrases (lhs) (left) .keysIterator.foreach( right =>
            copy.phrases(lhs)(left)(right) +=  0.0
          )
        )
      )
      lexicon.keysIterator.foreach( pos =>
        lexicon (pos) .keysIterator.foreach( word =>
          copy.lexicon(pos)(word) += 0.0
        )
      )
      copy.preCalcExps
      copy
    }
    
    def copy:ShakesPCNF = {
      val copy = new ShakesPCNF
      copy.phrases = phrases
      copy.lexicon = lexicon
      copy.phrExps = phrExps
      copy.lexExps = lexExps
      copy
    }

    /**
    * Clears out all counts from lexicon, phrases, lexExps and phrExps.
    */
    def resetGrammar {
      phrExps.clear
      lexExps.clear
      phrases.keysIterator.foreach( lhs =>
        phrases(lhs).keysIterator.foreach( left =>
          phrases(lhs)(left).keysIterator.foreach{ right =>
            phrases(lhs)(left).clear
            phrases(lhs)(left)(right) += 0.0
          }
        )
      )
      lexicon.keysIterator.foreach( pos =>
        lexicon(pos).keysIterator.foreach{ word =>
          lexicon(pos).clear
          lexicon(pos)(word) += 0.0
        }
      )
      preCalcExps
    }

    /**
    * Produce a readable stringification of lexicon and phrases in the same
    * format that readGrammar and readLexicon expect.
    * @return A string representation of lexicon and phrases.
    */
    override def toString =
      phrases.keysIterator.map{ lhs =>
        phrases (lhs) .keysIterator.map{ left =>
          phrases (lhs)(left) .keysIterator.map{ right =>
            lhs + " " + left + " " + right + " " + 
            ("%1.30f" format phrases(lhs)(left)(right) )
          }.mkString("\n","\n","")
        }.mkString("","","")
      }.mkString("","","\n") +
      lexicon.keysIterator.map{ pos =>
        lexicon (pos) .keysIterator.map{ word =>
          pos + " " + word + " " +
          ("%1.30f" format lexicon(pos)(word) )
        }.mkString("\n","\n","")
      }.mkString("","","")

    /**
    * Produce another readable stringification of lexicon and phrases. This is
    * not readable by readGrammar or readLexicon, but I think it is a little bit
    * easier for human eyes so I'm keeping it.
    * @return A string representation of lexicon and phrases.
    */
    def prettyPrint =
      phrases.keysIterator.map{ lhs =>
        phrases (lhs) .keysIterator.map{ left =>
          phrases (lhs)(left) .keysIterator.map{ right =>
            lhs + " " + left + " " + right + " " + phrases(lhs)(left)(right)
          }.mkString("\t","\n\t","\n")
        }.mkString("","","")
      }.mkString("Phrases:\n\n","","\n") +
      lexicon.keysIterator.map{ pos =>
        lexicon (pos) .keysIterator.map{ word =>
          pos + " " + word + " " + lexicon(pos)(word)
        }.mkString("\t","\n\t","")
      }.mkString("Lexicon:\n\n","\n","")
  }

  // Use this to terminate parsers when we run out of sentences to give them.
  case object Stop
  case class RightHandSide(leftChild:String,rightChild:String)
  case class Bracketing(leftSpanPoint:Int,rightSpanPoint:Int)
  abstract class ToParse {
    def size:Int
  }
  case class BracketedToParse(s:String,b:MHashSet[Bracketing]) extends ToParse {
    def words = s.split(' ')
    def size = words.size
  }
  case class StringToParse(s:String) extends ToParse {
    def words = s.split(' ')
    def size = words.size
  }


  /**
  * This defines what a parser must have, without giving an explicit definition
  * for certain functions we might want to change (such as re-estimation
  * functions). This must be extended by providing at least the actual parsing
  * functions synFill, lexFill, and populateChart. Other functions can of course
  * be provided as well, for example computeOPWithEstimates to provide outside
  * probability estimates.
  */
  trait ShakesParser {
    import math._
      //import collection.mutable.{HashMap => MHashMap}
    //import collection.immutable.{HashMap,HashSet}

    var parserID:ParserID
    var g:ShakesPCNF
    def firstStart:Unit //  What happens when we first start?
                          //  Remote actors register with the node

    abstract class Entry(label:String) {
      import collection.mutable.ArrayBuffer
      import math._

      /**
      * Stored as a probability (not log-likelihoods) to reduce rounding error
      * from exp and log over and over. Underflow is handled by scaling the
      * probability of each word.
      */
      var ip:Double = 1.0 // Initialization
      /**
      * Stored as a probability (not log-likelihoods) to reduce rounding error
      * from exp and log over and over. Underflow is handled by scaling the
      * probability of each word.
      */
      var op:Double = 1.0 // initialization

      var ipSetYet = false
      var opSetYet = false

      /**
      * Keeps track of a node's children for the outside pass.
      */
      var backMatcher = new ArrayBuffer[ArrayBuffer[RightHandSide]]
      0 to (length-1) foreach( i => backMatcher += new ArrayBuffer[RightHandSide])

      def size = backMatcher.size

      var start = 0 // Init to something ridiculous for simplicity
      var end = 0 // Init to something ridiculous for simplicity

      def length = end - start + 1
      val l = label

      /**
      * Get inside probability as a log-likelihood
      * @return The inside probability of the node.
      */
      def inScore = log(ip)
      /**
      * Get outside probability as a log-likelihood
      * @return The outside probability of the node.
      */
      def outScore = log(op)

      /**
      * Easily set the outside probability by providing a probability in
      * log-space.
      * @param x The amount (in log-space) to set the outside probability to.
      */
      def setOPScore(x:Double)
        { opSetYet = true; op = exp(x) }
      /**
      * Easily increment the outside probability by providing a probability in
      * log-space.
      * @param x The amount (in log-space) to increment the outside probability by.
      */
      def incOPScore(x:Double) {
        if(!opSetYet)
          setOPScore(x)
        else
          op = exp(x) + op
      }
      /**
      * Easily set the outside probability by providing a probability not in
      * log-space.
      * @param x The amount to set the outside probability to.
      */
      def setOPProb(x:Double)
        { opSetYet = true; op = x }
      /**
      * Easily increment the outside probability by providing a probability not
      * in log-space.
      * @param x The amount to increment the outside probability by.
      */
      def incOPProb(x:Double) {
        if(!opSetYet)
          setOPProb(x)
        else
          op = x + op
      }

      /**
      * Easily set the inside probability by providing a probability in
      * log-space.
      * @param x The amount (in log-space) to set the inside probability to.
      */
      def setIPScore(x:Double)
        { ipSetYet = true; ip = exp(x) }
      /**
      * Easily increment the inside probability by providing a probability in
      * log-space.
      * @param x The amount (in log-space) to increment the inside probability by.
      */
      def incIPScore(x:Double) {
        if(!ipSetYet)
          setIPScore(x)
        else
          ip = exp(x) + ip
      }
      /**
      * Easily increment the inside probability by providing a probability not
      * in log-space.
      * @param x The amount to increment the inside probability by.
      */
      def setIPProb(x:Double)
        { ipSetYet = true; ip = x }
      /**
      * Easily increment the inside probability by providing a probability not in
      * log-space.
      * @param x The amount to increment the inside probability by.
      */
      def incIPProb(x:Double) {
        if(! ipSetYet )
          setIPProb(x)
        else
          ip = x + ip
      }

      /**
      * Add a new expansion to the chart for Viterbi parsing. This is actually
      * kind of hacky when adding a unary (terminal) expansion. This is absurdly
      * inefficient, but good enough for now (and fast compared to the rest of
      * the system)
      *
      * @param left The left child.
      * @param right The right child.
      * @param s The left index of the expansion span
      * @param split The index of the split of the span
      * @param e The right index of the expansion span
      * @param prob The probability of the span
      */
      def newVitExpansion(left:String,right:String,s:Int,split:Int,e:Int,prob:Double) {
        if(end == 0) {
          start = s
          end = e
          backMatcher = new ArrayBuffer[ArrayBuffer[RightHandSide]]
          0 to (length-1) foreach( i => 
            backMatcher += new ArrayBuffer[RightHandSide]
          )
        }


        if( !ipSetYet | prob > ip ) {
          backMatcher .clear
          backMatcher = new ArrayBuffer[ArrayBuffer[RightHandSide]]
          0 to (length-1) foreach( i =>
            backMatcher += new ArrayBuffer[RightHandSide]
          )
          backMatcher( split - start ) += RightHandSide(left,right)

          //println( "\t\t"+backMatcher(split-start).size)

          setIPProb( prob )
        }
      }

      /**
      * Add a new expansion to the chart. This is actually kind of hacky when
      * adding a unary (terminal) expansion.
      *
      * @param left The left child.
      * @param right The right child.
      * @param s The left index of the expansion span
      * @param split The index of the split of the span
      * @param e The right index of the expansion span
      * @param prob The probability of the span
      */
      def newExpansion(left:String,right:String,s:Int,split:Int,e:Int,prob:Double) {
        if(end == 0) {
          start = s
          end = e
          backMatcher = new ArrayBuffer[ArrayBuffer[RightHandSide]]
          0 to (length-1) foreach( i =>
            backMatcher += new ArrayBuffer[RightHandSide]
          )
        }

        backMatcher(split - start) += RightHandSide(left,right)
        incIPProb( prob )
      }

      /**
      * Return an entry to its initial state.
      */
      def reset = {
        ip = 0.0
        op = 0.0
        ipSetYet = false
        opSetYet = false
        0 to (length - 1) foreach( i => backMatcher += 
          new ArrayBuffer[RightHandSide])
      }
      
      def viterbiString:String
    }

    /**
    * Binary-branching non-terminal entries.
    * @param label The label of the entry.
    */
    case class SynEntry(label:String)
      extends Entry(label) {
      def viterbiString:String = {
        import collection.mutable.ArrayBuffer
        val spanInfo =
          backMatcher.toArray.zipWithIndex.find( ! _._1.isEmpty ).toList.head
        
        val spans:ArrayBuffer[RightHandSide] = spanInfo._1
        val k = spanInfo._2
        val RightHandSide(left:String,right:String) = spans(0)

        "(" + label + " " +
        chart(start)(start+k)(left) .viterbiString +
        chart(start+k)(end)(right) .viterbiString +
        ")"
      }
    }

    /**
    * Unary-branching terminal entries.
    * @param label The prt of spedch of the entry.
    */
    case class LexEntry(label:String)
      extends Entry(label) {
      def viterbiString:String = {
          backMatcher.toArray.zipWithIndex.find( !
            _._1.isEmpty
          ).toList.head._1(0) match {
            case RightHandSide(word,_) => "(" + label + " " + word + ")"
          }

      }
    }




    /**
    * This is the chart itself.
    */
    object chart {
      //var triangMatrix = Array(Array(MHashMap[String,Entry]))
      

      /**
      * This is the data structure that stores the entries.
      */
      var matrix = Array.fill( 0,0) (new collection.mutable.HashMap[String,Entry]{
          override def default(s:String) = {
            this += Pair( s, new SynEntry(s) )
            this(s)
          }
     } )


      /**
      * Chart can be resized, which allows the same parser to parse sentences of
      * different lengths. This is necessary so the same parser can parse
      * multiple sentences without assuming a fixed upper bound on sentence
      * length.
      * @param chartSize The size of the chart.
      */
      def resize(chartSize:Int) {
        matrix = Array.fill( chartSize, chartSize )(
          new collection.mutable.HashMap[String,Entry] {
            override def default(s:String) = {
              this += Pair( s, new SynEntry(s) )
              this(s)
            }
          }
        )
      }

      /**
      * Access a hashmap of entries from the Chart
      * 
      * @param n1 Start index of the span.
      * @param n2 End index of the span.
      * @return A hashmap of Entries over the span.
      */
      def apply(n1:Int)(n2:Int) =
        matrix(n1)(n2)


      /**
      * @treturn The size of the chart.
      */
      def size = matrix.size


      /**
      * Stringify the chart.
      * @return A human-readable(ish) string representation of the chart.
      */
      override def toString =
        matrix.map( i =>
          Array.fill(matrix.size - i.length)( "<\t>\t").mkString("","","") +
          i.map( j =>
            "<" + {
              if(j.isEmpty)
                "\t"
              else
                j.keySet.map( k =>
                  k + ":" + String.format("%1.5f",
                  double2Double(j(k).ip)) +
                  "," + String.format("%1.5f",
                  double2Double(j(k).op))
                ).mkString("",",","")
              } + ">"
          ).mkString("","\t","\n")
        ).mkString("","","\n")
    }

    /**
    * @return The size of the chart
    */
    def size = chart.size

    /**
    * @return The root cell of the chart
    */
    def root = chart(0)( size - 1)

    /**
    *  Resize the chart.
    *  @param sentLen The new size for the chart
    */
    def resize(sentLen:Int) = chart.resize(sentLen)


    /**
    * Used in the CYK parser. Fills in one (non-terminal) span's worth of the
    * chart.
    * @param start The start index of the span.
    * @param end The end index of the span.
    */
    def synFill( start:Int, end:Int):Unit

    var wordScale:Int// = wS
    /**
    * Used in the CYK parser. Fills in the parts-of-speech of a word.
    * @param w The word.
    * @param index The index of the word.
    */
    def lexFill(w:String,index:Int):Unit


    /**
    * This is the CYK parsing algorithm. Same time complexity as Earley for
    * completely ambiguous grammars, so (since we're doing full grammar
    * induction) just use CYK. There's also a paper (Li and Alagappan something)
    * suggesting it has better average-case space complexity for completely
    * ambiguous grammars.
    * @param s The input sentence (an array of terminals)
    * @return A parse chart with labels and inside and outside probabilities.
    */
    def populateChart(s:Array[String]):Unit

    /**
    * @return The inside probability of the sentence, before de-scaling, and not
    * in log-space.
    */
    def scaledStringProb = chart(0)(size-1)("S").ip

    /**
    * @return The inside probability of the sentence not in log-space.
    */
    def stringProb = scaledStringProb / pow( wordScale , size - 1 )

    /**
    * @return The inside probability of the sentence in log-space.
    */
    def stringScore = log( stringProb )


    /**
    * Start at the root node and visit other nodes in a complete parse, making
    * sure to visit a node only if all parent nodes are already visited, and
    * apply a function to the entries of each node. Used for outside pass and
    * gathering estimated counts. By allowing arbitrary functions of entries, we
    * can change how we estimate a count easily; just call chartDescent but use
    * a different estimation function.
    * @param p The function to be applied
    */
    def chartDescent( p: ( Entry => Unit ) ) = {
      //import collection.mutable.{HashMap,HashSet}

      if( chart(0)(chart.size - 1).contains("S") ) {
        val toCompute = new MHashMap[(Int,Int),MHashSet[String]] {
          override def default(key:(Int,Int)) = {
            this += Pair(key, new MHashSet[String])
            this(key)
          }
        }

        //println("Beginning to descend into a parsed sentence")

        toCompute(0, size - 1) += "S"

        while( !toCompute.isEmpty) {
          val(start,end) = 
            toCompute.keysIterator.foldLeft[(Int,Int)](0,0)( (a,b) =>
              if( (a._2 - a._1) > (b._2 - b._1) ) a else b
          )
          val labels = toCompute( (start,end) )
          toCompute -= Tuple2(start,end)

          //println("Looking at span "+(start,end))

          labels.foreach{ l =>
              //println("looking at label " + l )
              val rootCell = chart(start)(end)(l)
              p( chart(start)(end)(l) )

              //println("Computed outside probability")

              (0 to (rootCell.backMatcher.length-1)).foreach{ split =>
                //println("When determining toCompute, looking at split " + split)
                val splitPoint = rootCell.start + split
                rootCell.backMatcher(split).foreach{ children =>
                  children match {
                    case RightHandSide( left, right ) => {
                      if( splitPoint - rootCell.start > 1 ) {
                        //println("adding "+(rootCell.start,splitPoint,left))
                        toCompute( (rootCell.start, splitPoint)) += left
                        }

                      if( rootCell.end - splitPoint > 1) {
                        //println("adding " + (splitPoint,rootCell.end,right))
                        toCompute ( (splitPoint, rootCell.end) ) += right
                      }

                    }
                    case _ =>
                  }
                  //println("New children added" )
                }
                //println("Done looking at split " + split + " when determining toCompute")
              }
          }
          //println("toCompute is " + toCompute)
        }
      }
    }
  }//end ShakesParser

  @serializable case class F_Key(
    start:Int,
    end:Int,
    lhs:String,
    left:String,
    right:String
  )
  @serializable case class G_Key(
    index:Int,
    pos:String,
    word:String
  )
  @serializable case class H_Key(
    start:Int,
    end:Int,
    label:String
  )
  trait EveryOneHundred {
    var quietude = 100
  }
  trait EveryFive {
    var quietude = 5
  }
  trait CountingDefinitions extends ShakesParser {
    //import collection.immutable.HashMap

    var quietude:Int

    /**
    * Compute the outside probability for one entry. Assumes that all referenced
    * values are already computed.
    * @param ent The entry to be scored.
    */
    def computeOP(ent:Entry) {
      import math._

      List.range(0,ent.backMatcher.size - 1).foreach{ split =>
        ent.backMatcher(split).foreach{ matches =>
          matches match {
            case RightHandSide( left, right ) => {
              val splitPoint  = split + ent.start

              val ruleProb =  g.phrases (ent.l)(left)(right)
              
              val leftEnt = chart( ent.start )( splitPoint )( left )
              val rightEnt = chart( splitPoint )( ent.end )( right )

              val toAddLeft = ent.op * ruleProb * rightEnt.ip
              leftEnt.incOPProb( toAddLeft )

              val toAddRight = ent.op * ruleProb * leftEnt.ip
              rightEnt.incOPProb( toAddRight )
            }
          }
        }
      }
    }

    /**
    * Compute the outside probability for one entry and gather estimated counts
    * based on the entry. Assumes that all referenced values are already
    * computed.
    * @param ent The entry to be scored and counted.
    */
    def computeOPWithEstimates(ent:Entry) {
      //import collection.mutable.HashMap
      import math._

      val h_Summand = ent.op * ent.ip
      val h_Key = H_Key(ent.start,ent.end,ent.l)
      h_i = h_i.updated( h_Key, h_Summand )

      val f_toAdd = new MHashMap[ (String,String), Double] {
        override def default(key:(String,String)) = 0D
      }


      (0 to (ent.backMatcher.size-1) ) foreach{ split =>
        ent.backMatcher(split).foreach{ matches =>
          matches match {
            case RightHandSide( left, right ) => {
              val ruleProb = g.phrases (ent.l) (left) (right)

              val splitPoint = split + ent.start

              val leftEnt = chart (ent.start) (splitPoint) (left)
              val rightEnt = chart (splitPoint) (ent.end) (right)

              val toAddLeft = ent.op * ruleProb * rightEnt.ip
              leftEnt.incOPProb( toAddLeft )

              val toAddRight = ent.op * ruleProb * leftEnt.ip
              rightEnt.incOPProb( toAddRight )

              leftEnt match{
                case LexEntry(_) => {
                  val g_Summand = leftEnt.ip * leftEnt.op

                  val RightHandSide( word, _ ) = leftEnt.backMatcher(0)(0)
                  val g_Key = G_Key(leftEnt.start, leftEnt.l, word )
                  g_i = g_i.updated( g_Key, g_Summand )

                  val h_Summand = leftEnt.ip * leftEnt.op
                  val h_Key = H_Key( leftEnt.start, leftEnt.end, leftEnt.l)
                  h_i = h_i.updated( h_Key, h_Summand )
                }
                case _ =>
              }
              rightEnt match {
                case LexEntry(_) => {
                  val g_Summand = rightEnt.ip * rightEnt.op

                  val RightHandSide( word, _ ) = rightEnt.backMatcher(0)(0)
                  val g_Key = G_Key(rightEnt.start, rightEnt.l, word )
                  g_i = g_i.updated( g_Key, g_Summand )

                  val h_Summand = leftEnt.ip * leftEnt.op
                  val h_Key = H_Key( rightEnt.start, rightEnt.end, rightEnt.l)
                  h_i = h_i.updated( h_Key, h_Summand )
                }
                case _ => 
              }

              val f_Summand =
                ent.op * ruleProb * leftEnt.ip * rightEnt.ip
              f_toAdd( (left,right) ) =
                  f_toAdd( (left,right) )+ f_Summand
            }
            case _ =>
          }
        }
      }

      f_toAdd .keysIterator.foreach{ k =>
        val(left,right) = k

        val summand:Double = f_i( F_Key(ent.start,ent.end,ent.l,left,right) ) +
        (f_toAdd(k))

        f_i = f_i.updated( F_Key(ent.start,ent.end,ent.l,left,right),summand)
        
      }
    }

    /**
    * This stores intermediate counts of binary-branching nodes for this sentence.
    */
    @serializable var f_i = new collection.immutable.HashMap[F_Key,Double] withDefaultValue(0D)
    /**
    * This stores intermediate counts of unary-branching nodes for this sentence.
    */
    //var g_i = new IHashMap[(Int,String,String), Double] {
    @serializable var g_i = new IHashMap[G_Key, Double] withDefaultValue(0D)

    /**
    * This stores intermediate counts of non-terminal nodes for this sentence.
    */
    @serializable var h_i = new IHashMap[H_Key, Double] withDefaultValue(0D)
  }

  /**
  * This provides all parsing functions. If we receive a bracketed sentence, use
  * the brackets, otherwise do vanilla EM
  */
  trait EstimationParser extends CountingDefinitions {
    var g:ShakesPCNF

    /**
    * Fills in the parts-of-speech of a word.
    * @param w The word.
    * @param index The index of the word.
    */
    def lexFill(w:String,index:Int) {
      g.lexExps(w).foreach{ pos =>
        val l = new LexEntry(pos._1)
        l.newExpansion(
          w,w,
          index,index,index+1,
          wordScale * pos._2
        )
        chart(index)(index+1) += Pair( pos._1, l)
      }
    }

    /**
    * Fills in one (non-terminal) span's worth of the chart.
    * @param start The start index of the span.
    * @param end The end index of the span.
    */
    def synFill( start:Int, end:Int) {
      start+1 to (end-1) foreach{ k =>
        chart(start)(k).keysIterator.foreach{ left =>
          chart(k)(end).keysIterator.foreach{ right =>
            g.phrExps (left)(right) .foreach{ phrase =>
              val thisprob = phrase._2 * 
                              chart(start)(k)(left).ip *
                              chart(k)(end)(right).ip
                chart(start)(end)(phrase._1).newExpansion(
                  left, right,
                  start,k,end,
                  thisprob
                )
            }
          }
        }
      }
    }

    /*
    * This is the CYK parsing algorithm. Same time complexity as Earley for
    * completely ambiguous grammars, so (since we're doing full grammar
    * induction) just use CYK. There's also a paper (Li and Alagappan something)
    * suggesting it has better average-case space complexity for completely
    * ambiguous grammars.
    * @param s The input sentence (an array of terminals)
    * @return A parse chart with labels and inside and outside probabilities.
    */
    def populateChart(s:Array[String]) = {
      1 to s.size foreach{ j =>
        lexFill( s(j-1), j-1)
        
        if( j > 1 )
          ((0 to (j-2)) reverse) foreach{ i =>
            synFill(i, j)
          }
      }

      //println("Inside pass complete with scaled string score " + stringScore)

      chartDescent( computeOPWithEstimates )
      //println("Outside pass complete")
    }

    var bracketing:MHashSet[Bracketing] = new MHashSet // Initialize to empty set
    def isCompatible(start:Int, end:Int):Boolean =
      ! bracketing.exists{ span =>
        val Bracketing( left, right ) = span
        
        (       // Crosses a given left span
          start < left &
          end > left &
          end < right
        ) | (   // Crosses a given right span
          start > left &
          start < right &
          end > right
        )
      }

    /**
    * This is the CYK parsing algorithm, modified to fill in cells iff the
    * relevant span is compatible with the given bracketing. Same time
    * complexity as Earley for completely ambiguous grammars, so (since we're
    * doing full grammar induction) just use CYK. There's also a paper (Li and
    * Alagappan something) suggesting it has better average-case space
    * complexity for completely ambiguous grammars.
    * @param s The input sentence (an array of terminals)
    * @return A parse chart with labels and inside and outside probabilities.
    */
    def bracketedChartPopulation(s:Array[String]) = {
      //println("Inside pass beginning")
      1 to s.size foreach{ j =>
        lexFill( s(j-1), j-1)
        
        if( j > 1 )
          ((0 to (j-2)) reverse) foreach{ i =>
            if( isCompatible( i, j ) )
              synFill(i, j)
          }
      }
      //println("Inside pass complete")
      chartDescent( computeOPWithEstimates )
    }

    /**
    * inverse of verbosity
    */
    var stringCount = 0

    /**
    * Use this as an actor
    */
    def act() {
      import math._
      firstStart
      println("Parser " + parserID + " started")
      loop {
        react {
          case itemList:List[ToParse] => {

            val numSentences = itemList.size
            val numTerminals = itemList.foldLeft( 0 ) ( (a,b) => a + b.size )

            if( ceil( stringCount / quietude ) != ceil( (stringCount +
            numSentences) / quietude ) )
              println( "Received " + numSentences + " sentences with " +
                numTerminals + " total terminals")
          

            itemList foreach ( item => 
              item match {
                case StringToParse( s:String ) => {
                  //if( stringCount % quietude == 0 )
                  //  println( parserID + " about to process string " + s )



                  f_i = new IHashMap[F_Key,Double] withDefaultValue(0D)
                  g_i = new IHashMap[G_Key, Double] withDefaultValue(0D)
                  h_i = new IHashMap[H_Key, Double] withDefaultValue (0D)

                  //if( stringCount % quietude == 0 )
                  //  println( parserID + " resizing chart...")
                  val words = s.split(' ')
                  resize( words.size+1 )

                  //if( stringCount % quietude == 0 )
                  //  println( parserID + " parsing sentence...")
                  populateChart(words)

                  if( !root.contains("S") ) {
                    println("WARNING: SENTENCE DID NOT PARSE")
                    println( s )
                  }

                  val scaledBy = pow( wordScale, size - 1 )


                    //if(stringCount >= quietude )
                    //  stringCount = 0
                    //else
                  stringCount += 1

                    //reply( ParsingResult(parserID,
                    //              scaledStringProb,
                    //              f_i.toMap,
                    //              g_i.toMap,
                    //              h_i.toMap,
                    //              scaledBy) )

                  sender ! FResult( f_i.toMap, scaledStringProb )
                  sender ! GResult( g_i.toMap, scaledStringProb )
                  sender ! HResult( h_i.toMap, scaledStringProb )
                  sender ! StringProbResult( scaledStringProb, scaledBy )

                    //estimatesReply = ParsingResult(parserID,
                    //              scaledStringProb,
                    //              f_i.toMap,
                    //              g_i.toMap,
                    //              h_i.toMap,
                    //              scaledBy)::estimatesReply
                }

                case BracketedToParse( s:String, b:MHashSet[Bracketing] ) => {
                  f_i = new IHashMap[F_Key,Double] withDefaultValue(0D)
                  g_i = new IHashMap[G_Key, Double] withDefaultValue(0D)
                  h_i = new IHashMap[H_Key, Double] withDefaultValue (0D)
                  bracketing = b


                  val words = s.split(' ')
                  resize(words.size + 1)


                  //if( stringCount % quietude == 0 ){
                  //  println( parserID + " received sentence " + s)
                  //  println( parserID + " received bracketing " + b)
                  //}

                  //if( stringCount % quietude == 0 )
                  //  println( parserID + " parsing sentence...")
                  bracketedChartPopulation(words)

                  if( !root.contains("S") ) {
                    println("WARNING: SENTENCE DID NOT PARSE")
                    println( s )
                  }

                  //if( stringCount % quietude == 0 )
                  //  println( parserID + " replying with estimates...")

                  val scaledBy = pow( wordScale , size - 1 )

                  //if(stringCount >= quietude )
                  //  stringCount = 0
                  //else
                  stringCount += 1

                  //reply( ParsingResult(parserID,
                  //              scaledStringProb,
                  //              f_i.toMap,
                  //              g_i.toMap,
                  //              h_i.toMap,
                  //              scaledBy) )
                  reply( FResult( f_i.toMap, scaledStringProb ) )
                  reply( GResult( g_i.toMap, scaledStringProb ) )
                  reply( HResult( h_i.toMap, scaledStringProb ) )
                  reply( StringProbResult( scaledStringProb, scaledBy ) )

                }
              }
            )
              //if( stringCount % quietude == 0 )
              //  println( parserID + " replying with estimates...")
              //reply(estimatesReply)
            //if( stringCount % quietude == 0 )
            //  println( parserID + " asking for more...")
            sender ! parserID
          }
          case StringToParse(s:String) => {
            //if( stringCount % quietude == 0 )
            //  println( parserID + " received string " + s )



            f_i = new IHashMap[F_Key,Double] withDefaultValue(0D)
            g_i = new IHashMap[G_Key, Double] withDefaultValue(0D)
            h_i = new IHashMap[H_Key, Double] withDefaultValue (0D)

            //if( stringCount % quietude == 0 )
            //  println( parserID + " resizing chart...")
            val words = s.split(' ')
            resize( words.size+1 )

            //if( stringCount % quietude == 0 )
            //  println( parserID + " parsing sentence...")
            populateChart(words)

            if( !root.contains("S") ) {
              println("WARNING: SENTENCE DID NOT PARSE")
              println( s )
            }

            val scaledBy = pow( wordScale, size - 1 )

            //if( stringCount % quietude == 0 )
            //  println( parserID + " replying with estimates...")

            if(stringCount >= quietude )
              stringCount = 0
            else
              stringCount += 1
      
            sender ! ParsingResult(parserID,scaledStringProb,f_i.toMap,g_i.toMap,h_i.toMap,scaledBy)
            //if( stringCount % quietude == 0 )
            //  println( parserID + " asking for more...")
            sender ! parserID
          }
          case BracketedToParse(s:String,b:MHashSet[Bracketing]) => {  
            f_i = new IHashMap[F_Key, Double] withDefaultValue(0D)
            g_i = new IHashMap[G_Key, Double] withDefaultValue(0D)
            h_i = new IHashMap[H_Key, Double] withDefaultValue (0D)
            bracketing = b


            val words = s.split(' ')
            resize(words.size + 1)


            //if( stringCount % quietude == 0 ){
            //  println( parserID + " received sentence " + s)
            //  println( parserID + " received bracketing " + b)
            //}

            //if( stringCount % quietude == 0 )
            //  println( parserID + " parsing sentence...")
            bracketedChartPopulation(words)

            if( !root.contains("S") ) {
              println("WARNING: SENTENCE DID NOT PARSE")
              println( s )
            }

            //if( stringCount % quietude == 0 )
            //  println( parserID + " replying with estimates...")

            val scaledBy = pow( wordScale , size - 1 )

            stringCount += 1

            sender ! ParsingResult(parserID,scaledStringProb,f_i.toMap,g_i.toMap,h_i.toMap,scaledBy)

            //if( stringCount % quietude == 0 )
            //  println( parserID + " asking for more...")
            sender ! parserID
          }
          case StillAlive => reply( StillAlive )
          case Stop => {      // If we get the stop signal, then shut down.
            println( parserID + " stopping")
            exit()
          }
          case trainedGram:ShakesPCNF => {
            g = trainedGram
            println( "Received a new grammar" )
            reply( StillAlive )
          }
          case RemoteParserID(id:Int) => {
            parserID = RemoteParserID(id)
            println( "Parser now known as " + parserID )
          }
          case what:Any => {
            println("got something else: "  + what)
          }
        }
      }
    }
  }

  @serializable case object StillAlive

  @serializable case class FResult(
    f_i:scala.collection.immutable.Map[F_Key,Double],
    scaledBy:Double
  )
  @serializable case class GResult(
    g_i:scala.collection.immutable.Map[G_Key,Double],
    scaledBy:Double
  )
  @serializable case class HResult(
    h_i:scala.collection.immutable.Map[H_Key,Double],
    scaledBy:Double
  )
  @serializable case class StringProbResult(
    scaledStringProb:Double,
    scaledBy:Double
  )

  @serializable case class ParsingResult(
    parserID:ParserID,
    scaledStringProb:Double,
    f_i:scala.collection.immutable.Map[F_Key,Double],
    g_i:scala.collection.immutable.Map[G_Key,Double],
    h_i:scala.collection.immutable.Map[H_Key,Double],
    scaledBy:Double
  )

  @serializable abstract class ParserID(id:Int)
  @serializable case class RemoteParserID(id:Int) extends ParserID(id)
  @serializable case class LocalParserID(id:Int) extends ParserID(id)

  case class Evaluation( prefix:String, trainedGram:ShakesPCNF )

  trait EvaluatingManager {
    val testSentences:List[String]
    def finalCleanup(trainedGram:ShakesPCNF) = {
      VitActor ! Evaluation("Convergence",trainedGram)
      testSentences.foreach( sent => VitActor ! sent )
      VitActor ! Stop
    }

    object VitActor extends ViterbiDefinitions {
      var parserID:ParserID = LocalParserID(-1)
      var g = new ShakesPCNF
      var wordScale = 10000
    }

    VitActor.start

    def useGrammar( trainedGram:ShakesPCNF, iterNum:Int ) {
      if( iterNum % 2 == 0 ) {
        VitActor ! Evaluation("Iter"+iterNum,trainedGram)
        testSentences.foreach( sent => VitActor ! sent )
      }
    }
  }

  /**
  * This provides chart filling definitions for viterbi parsing
  */
  trait ViterbiDefinitions extends Actor with LocalDefinitions with ShakesParser {
    /**
    * This is the CYK parsing algorithm. Same time complexity as Earley for
    * completely ambiguous grammars, so (since we're doing full grammar
    * induction) just use CYK. There's also a paper (Li and Alagappan something)
    * suggesting it has better average-case space complexity for completely
    * ambiguous grammars.
    * @param s The input sentence (an array of terminals)
    * @return A parse chart with labels and inside and outside probabilities.
    */
    def populateChart(s:Array[String]) = {
      1 to s.size foreach{ j =>
        lexFill( s(j-1), j-1)
        
        (0 to (j-2) reverse) foreach{ i =>
          synFill(i, j)
        }
      }
    }
    def lexFill( w:String, index:Int) {
      val mlePOS = g.lexExps(w).foldLeft(g.lexExps(w)(0))( (l1, l2) =>
        if( l1._2 > l2._2 )
          l1 else l2
      )
      val l = new LexEntry( mlePOS._1 )
      l.newExpansion(
        w,w,
        index, index, index+1,
        wordScale * mlePOS._2
      )
      chart(index)(index+1) += Pair( mlePOS._1, l)
    }
    def synFill( start:Int, end:Int) {
      start+1 to (end-1) foreach{ k =>
        chart(start)(k).keysIterator.foreach{ left =>
          chart(k)(end).keysIterator.foreach{ right =>
            g.phrExps (left)(right) .foreach{ phrase =>
              val thisprob = phrase._2 * 
                              chart(start)(k)(left).ip *
                              chart(k)(end)(right).ip
                chart(start)(end)(phrase._1).newVitExpansion(
                  left, right,
                  start,k,end,
                  thisprob
                )
            }
          }
        }
      }
    }

    var prefix= ""

    def act() {
      loop {
        react {
          case Evaluation(pre:String, trainedGram:ShakesPCNF) => {
            prefix = pre
            g = trainedGram
            println( "Received grammar for " + prefix + " evaluation")
          }

          case Stop => {      // If we get the stop signal, then shut down.
            println("Parser " +parserID + " stopping")
            exit()
          }

          case s:String => {
            val words = s.split(' ')
            resize(words.size + 1)

            populateChart(words)

            if( root.contains("S") ) {
              println( prefix + ": " + root("S").viterbiString )
            } else {
              println("WARNING: SENTENCE DID NOT PARSE")
              println( s )
            }

          }
        }
      }
    }

    def parseString:String = chart(0)(chart.size - 1)("S").viterbiString
  }

  trait LocalDefinitions {
    def firstStart = ()
  }

  trait RemoteDefinitions {
    import scala.actors.Actor._
    import scala.actors.remote.RemoteActor._
    val host:String
    val port:Int
    var g = new ShakesPCNF
    def firstStart = {
      alive( port )
      register( 'parser, self )
    }
  }

  //case class RemoteSpec(ip:String,port:Int)

  trait ShakesParserManager {
    import scala.actors.AbstractActor
    import math._

    var g1:ShakesPCNF
    var g2:ShakesPCNF

    def stoppingCondition( numIters:Int, deltaLogProb:Double ):Boolean

    /**
    * This method provides access to the slave parsers at the beginning of each
    * iteration.. Implementations for remote actors should send the grammar to
    * the remote actor. Implementations for local actors should probably just
    * create a new actor with the new grammar (and let garbage collection take
    * care of the old actor)
    *
    * @param grammar  Grammar to be used for this iteration.
    */
    def remoteParserConstructor( grammar:ShakesPCNF ):List[AbstractActor]
    def localParserConstructor( grammar:ShakesPCNF ):List[ShakesParser with Actor]
    //def iterationCleanup( parsers:List[AbstractActor] ):Unit

    def useGrammar( trainedG:ShakesPCNF, curIterCount:Int ):Unit
    def finalCleanup( trainedGram:ShakesPCNF ):Unit


    //val hostsList:List[Array[String]]
    val deadHosts = new MHashSet[RemoteParserID]

    val trainingCorpus:ShakesTrainCorpus
    var quietude:Int

    val timeout:Int

    def act() {
      var iterationNum = 0
      var deltaLogProb = 1.0
      var lastCorpusLogProb = 0.0
      var corpusLogProb = 0.0

      while( ! stoppingCondition( iterationNum, deltaLogProb ) ) {
        val localParsers = localParserConstructor( g1 )
        val remoteParsers = remoteParserConstructor( g1 )

        var thisIterTrain = trainingCorpus.toList.sortWith(
          (a,b) => a.size > b.size 
        )

        println("Beginning to parse iteration " + iterationNum + "...\n\n")

        var sentenceNumber = 0
        var numFinishedParsers = 0

        val maxTerminalsPerPackageLocal = 200//100
        val maxTerminalsPerPackageRemote = 1000//100

        println( "Distributing to remote parsers" )
        (0 to (remoteParsers.size-1)) filter ( index =>  
          ! (deadHosts.contains(RemoteParserID( index )))
          ) foreach{ id =>
          println( "Checking parser " + RemoteParserID(id))
          remoteParsers(id) !?(timeout, StillAlive) match {
            case Some(_) => {// println("this remote parser still alive" )
              var prefixLength = 0
              val prefix = thisIterTrain.takeWhile( nextSent =>
                {
                  prefixLength += nextSent.size
                  prefixLength <= maxTerminalsPerPackageRemote
                }
              )

              val numberToSend = prefix.size
              val numTerminals = prefix.foldLeft( 0 ) ( (a,b) => a + b.size )

              if( ceil( sentenceNumber / quietude ) != ceil( (sentenceNumber +
                  numberToSend) / quietude ) )
                  println( "Sending " + numberToSend + " sentences with " +
                    numTerminals + " total terminals to a remote parser" )


              if( numberToSend > 0 ) {
                thisIterTrain = thisIterTrain.slice( numberToSend, thisIterTrain.size )
                sentenceNumber += numberToSend
                remoteParsers(id) ! prefix
              } else {
                numFinishedParsers += 1
              }
            }
            case None => { 
              deadHosts += RemoteParserID(id)
              println( RemoteParserID(id) + " timed out" )
            }
          }
        }


        println( "Distributing to local parsers" )
        localParsers foreach{ localParser =>

          var prefixLength = 0
          val prefix = thisIterTrain.takeWhile( nextSent =>
            {
              prefixLength += nextSent.size
              prefixLength <= maxTerminalsPerPackageLocal
            }
          )

          val numberToSend = prefix.size
          val numTerminals = prefix.foldLeft( 0 ) ( (a,b) => a + b.size )
          println( "Sending " + numberToSend + " sentences with " +
            numTerminals + " total terminals to a local parser" )


          if( numberToSend > 0 ) {
            thisIterTrain = thisIterTrain.slice( numberToSend, thisIterTrain.size )
            sentenceNumber += numberToSend
            localParser ! prefix
          } else {
            numFinishedParsers += 1
          }
        }

        println("All sentences sent")

        def totalParserCount = remoteParsers.size + localParsers.size -
        deadHosts.size


        println( numFinishedParsers +
          " finished parsers at the beginning of the iteration" )
        println( deadHosts.size +
          " dead hosts at the beginning of the iteration")

        var parsedSentences = 0

        while( numFinishedParsers < totalParserCount ) {
          receive {
            case s:String => println( s )

            case FResult(f_i,scaledStringProb) => {
              f_i.keysIterator.foreach{ summandKey =>
                val F_Key( _,_,lhs,left,right ) = summandKey
                g2.f (lhs)(left)(right) =
                  g2.f (lhs)(left)(right) +
                    (
                      f_i (summandKey) /
                        scaledStringProb
                    )
              }
            }

            case GResult(g_i,scaledStringProb) => {
              g_i.keysIterator.foreach{ summandKey =>
                val G_Key( _, pos, word ) = summandKey
                g2.g( (pos,word) ) = 
                g2.g( (pos,word) ) +
                  (
                    g_i( summandKey ) /
                    scaledStringProb
                  )
              }
            }


            case HResult(h_i,scaledStringProb) =>
              h_i.keysIterator.foreach{ summandKey =>
                val H_Key(start,end,label) = summandKey
                g2.h(label) =
                  g2.h(label) + 
                  (
                    h_i(summandKey) /
                    scaledStringProb
                  )
              }

            case StringProbResult(scaledStringProb,scaledBy) =>
              corpusLogProb = corpusLogProb + log( scaledStringProb ) -
                log( scaledBy )


            case RemoteParserID(id:Int) => {

              remoteParsers(id) !?(timeout, StillAlive) match {
                case None => {
                  println( RemoteParserID(id) + " timed out " +
                  (numFinishedParsers,totalParserCount))
                  deadHosts += RemoteParserID(id)
                }
                case Some(_) => {

                  if( thisIterTrain.size > 0 ) {
                    var prefixLength = 0
                    val prefix = thisIterTrain.takeWhile( nextSent =>
                      {
                        prefixLength += nextSent.size
                        prefixLength <= maxTerminalsPerPackageRemote
                      }
                    )

                    val numberToSend = prefix.size
                    val numTerminals = prefix.foldLeft( 0 ) ( (a,b) => a + b.size )
                    

                    thisIterTrain = thisIterTrain.slice( numberToSend, thisIterTrain.size )


                    sentenceNumber += numberToSend

                    if( ceil((sentenceNumber-numberToSend)/ quietude ) !=
                      ceil( (sentenceNumber/ quietude ) ) ) {
                      println( "Sending " + numberToSend +
                      " sentences to parser " + RemoteParserID(id) +
                      ". Up to sentence number " +
                      sentenceNumber )
                    }


                    //if( sentenceNumber % quietude < 10 )
                    //  println( "Sending " + numberToSend + 
                    //  " sentences to parser " + RemoteParserID(id) +
                    //  ". Up to sentence number " +
                    //  sentenceNumber + ".")

                    reply( prefix )
                  } else {
                    numFinishedParsers += 1
                    println( RemoteParserID(id) + " stopping")
                    //+ (parsedSentences,trainingCorpus.size) )
                  }
                }
              }

            }

            case LocalParserID(id:Int) => {
              if( thisIterTrain.size > 0 ) {
                var prefixLength = 0
                val prefix = thisIterTrain.takeWhile( nextSent =>
                  {
                    prefixLength += nextSent.size
                    prefixLength <= maxTerminalsPerPackageLocal
                  }
                )

                val numberToSend = prefix.size
                val numTerminals = prefix.foldLeft( 0 ) ( (a,b) => a + b.size )
                

                thisIterTrain = thisIterTrain.slice( numberToSend, thisIterTrain.size )

                sentenceNumber += numberToSend

                if( ceil((sentenceNumber-numberToSend)/ quietude ) !=
                  ceil( (sentenceNumber/ quietude ) ) ) {
                  println( "Sending " + numberToSend +
                    " sentences to parser " + RemoteParserID(id) +
                    ". Up to sentence number " +
                    sentenceNumber )
                }


                reply( prefix )

              
              } else {
                numFinishedParsers += 1
                println( LocalParserID(id) + " stopping" )
                //+ (parsedSentences,trainingCorpus.size) )
              }
            }

            case ParsingResult(
              parserID:ParserID,
              scaledStringProb:Double,
              f_i:Map[F_Key,Double],
              g_i:Map[G_Key,Double],
              h_i:Map[H_Key,Double],
              scaledBy:Double ) => {

              parsedSentences += 1


              if( sentenceNumber % quietude == 0 )
                println( "Estimates received from " + parserID )
              //println( "f_i: \n" + f_i + "\n\n\n" )
              //println( "g_i: \n" + g_i + "\n\n\n" )
              //println( "h_i: \n" + h_i + "\n\n\n" )


              corpusLogProb = corpusLogProb + log( scaledStringProb ) -
                log( scaledBy )


              f_i.keysIterator.foreach{ summandKey =>
                val F_Key( _,_,lhs,left,right ) = summandKey
                g2.f (lhs)(left)(right) =
                  g2.f (lhs)(left)(right) +
                    (
                      f_i (summandKey) /
                        scaledStringProb
                    )
              }

              g_i.keysIterator.foreach{ summandKey =>
                val G_Key( _, pos, word ) = summandKey
                g2.g( (pos,word) ) = 
                g2.g( (pos,word) ) +
                  (
                    g_i( summandKey ) /
                    scaledStringProb
                  )
              }

              h_i.keysIterator.foreach{ summandKey =>
                val H_Key(start,end,label) = summandKey
                g2.h(label) =
                  g2.h(label) + 
                  (
                    h_i(summandKey) /
                    scaledStringProb
                  )
              }


                //if( thisIterTrain isEmpty /*sentenceNumber >= trainingCorpus.size*/ ) {
                //  println( parserID + " finished")
                //  numFinishedParsers += 1
                //} else {
                //  parserID match {
                //    case RemoteParserID(_) => {
                //      val nextLongOne = thisIterTrain.head
                //        if( nextLongOne.size > 10 ) {
                //          sentenceNumber = sentenceNumber + 1
                //          thisIterTrain = thisIterTrain.tail

                //          if( sentenceNumber % quietude == 0 )
                //            println( "Sending sentence number " + sentenceNumber +
                //              " to parser " + parserID )

                //          reply( nextLongOne )
                //        } else {
                //          println( parserID + " finished")
                //          numFinishedParsers += 1
                //        }
                //    }
                //    case LocalParserID(_) => {
                //      sentenceNumber = sentenceNumber + 1

                //      if( sentenceNumber % quietude == 0 )
                //        println( "Sending sentence number " + sentenceNumber +
                //          " to parser " + parserID )

                //      val nextShortOne = thisIterTrain.last
                //      thisIterTrain = thisIterTrain.init

                //      reply( nextShortOne )
                //    }
                //  }
                //}
            }

            case what:Any =>
              println("ShakesParserManager got something else: " + what)

          }
        }
          //println( (numFinishedParsers,totalParserCount))

        //parsers.foreach( _ ! Stop )

        g2.reestimateRules

        g1 = g2
        g2 = g1.countlessCopy

        deltaLogProb = (lastCorpusLogProb - corpusLogProb ) /
          abs(corpusLogProb)

        println("corpusLogProb.Iter"+iterationNum + ": "+ corpusLogProb)
        println("deltaLogProb.Iter"+iterationNum + ": "+ deltaLogProb)

        useGrammar( g1 , iterationNum)

        lastCorpusLogProb = corpusLogProb
        corpusLogProb = 0.0

        iterationNum = iterationNum + 1

        localParsers.foreach( _ ! Stop )
        //iterationCleanup(parsers)
      }
      finalCleanup(g1)
      exit()
    }
  }



  /**
  * Training corpus classes. Apply should return whatever counts as one sentence
  * (whether it's a string or a tuple of a string and a bracketing), size should
  * give the number of sentences, and readCorpus should read in the corpus from
  * file(s)
  */
  trait ShakesTrainCorpus {
    def apply(n:Int):ToParse
    def size:Int
    def readCorpus(pathSpecifier:String)
    def toList:List[ToParse]
  }

  /**
  * Reads, stores, and makes useful a partially bracketed corpus.
  */
  class BracketedCorpus extends ShakesTrainCorpus {
    //import collection.mutable.HashSet

    var bracketings:Array[MHashSet[Bracketing]] = Array()
    var strings:Array[String] = Array()

    /**
    * Reads in strings of terminals from <code>stringsPath</code> and the
    * brackets from <code>bracketsPath</code>. stringsPath should point to a
    * file with one sentence per line. bracketsPath should point to a file that
    * specifies the partial bracketing with each span specified in the following
    * format:
    * <br /><br />
    * <code>sentence-id span-start-index span-end-index</code>
    * <br /><br />
    * Where <code>sentence-id</code> is the zero-based index of the sentence in
    * the stringsPath. Basically, I think it is just easier to get a perl script
    * to take a Penn-Treebank style corpus and produce these files than to do it
    * in Scala.
    *
    * @param stringsPath File path to the strings we're trying to induce a
    * grammar from.
    * @param bracketsPath File path to the corresponding partial bracketing.
    */
    def readCorpus(corporaPrefix:String) {
      import scala.io.Source._

      val stringsPath = corporaPrefix+"-strings.txt"
      val bracketsPath = corporaPrefix+"-brackets.txt"


      strings = fromPath(stringsPath).getLines("\n").toList.filter(_.length > 0).map(_.replace("\n","")).toArray

      val corpusSize = strings.size

      bracketings = Array.fill(corpusSize + 1 )(
        new MHashSet[Bracketing]
      )

      val lines = fromPath(bracketsPath).getLines("\n")
      lines.foreach( line =>
        if( line.length > 1 ) {
          val fields = line.replace("\n","").split(' ')
          
          val (start,end) = ( fields(1).toInt, fields(2).toInt )
          if( end - start > 1 )
            bracketings( fields(0).toInt ) +=
              Bracketing( fields(1).toInt, fields(2).toInt )
        }
      )
    }
    def apply(n:Int) = BracketedToParse(strings(n),bracketings(n))
    def size = strings.size
    override def toString = strings mkString("","\n","")
    def toList = ((0 to (strings.size-1)) map{ index =>
      BracketedToParse( strings( index ), bracketings( index ) )
    }).toList
  }

  /**
  * Reads, stores, and makes useful a corpus with only strings
  */
  class StringsOnlyCorpus extends ShakesTrainCorpus {
    var strings:Array[String] = Array()
    def apply(n:Int) = StringToParse( strings( n ) )
    def size = strings.size

    def readCorpus( stringsPath:String ) {
      import scala.io.Source._
      strings = fromPath(stringsPath).getLines("\n").toList.filter(_.length > 0).map(_.replace("\n","")).toArray
    }
    override def toString = strings mkString("","\n","")

    def toList = ((0 to (strings.size-1)) map{ index =>
      StringToParse( strings( index ) )
    }).toList
  }
} // END PACKAGE





