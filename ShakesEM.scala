/**
* ShakesEM is a package for running EM for Probabilistic Context Free Grammars,
* implemented in Scala using the Actors library to allow parallel processing
* over multiple cores. This is a branch of the project to use Akka actors to try
* to clusterfy my program.
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
* @version 0.20_akkaActors
* @author John K Pate
*/
package ShakesEM {
  //import scala.actors.Actor
  //import scala.actors.Actor._
  import se.scalablesolutions.akka.actor.Actor
  import se.scalablesolutions.akka.actor.Actor._
  /**
  * <code>ShakesPCNF</code> defines a Probabilistic Chomsky Normal Form grammar
  * for use with the ShakesEM library
  */
  class ShakesPCNF {
    import Math._
    import collection.mutable.{HashMap,HashSet}


    /**
    * Used in re-estimation of the PCFG. f Sums estimated counts for binary
    * branching nodes, g for unary branching nodes, and h for any non-terminal
    * node.
    * See Manning &amp; Schutze p. 396.
    * The defaults just make the incrementation code cleaner.
    */
    val f = new HashMap[
      String,   
      HashMap[
        String,
        HashMap[String,Double]
      ]
    ] {
      override def default(lhs:String) = {
        this += Pair( lhs,
                      new HashMap[String, HashMap[String,Double]] {
            override def default(left:String) = {
              this += Pair( left,
                            new HashMap[String,Double] {
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
    val g = new HashMap[(String,String),Double] {
      override def default(key:(String,String)) = 0
    }
    val h = new HashMap[String,Double] {
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
      f.keys.foreach{ lhs =>
        f (lhs) .keys.foreach{ left =>
          f (lhs)(left) .keys.foreach{ right =>
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



      g.keys.foreach{ k =>
        lexicon (k._1) (k._2) =
           100000 * g(k) /
           h(k._1)
      }
      lexicon.keys.foreach{ pos =>
        lexicon(pos).keys.foreach{ word =>
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
      phrases.keys.foreach{ lhs =>
        var lhsTotal = 0D
        phrases(lhs).keys.foreach( left =>
          phrases(lhs)(left).keys.foreach( right =>
              lhsTotal = lhsTotal + phrases(lhs)(left)(right)
          )
        )
        phrases(lhs).keys.foreach( left =>
          phrases(lhs)(left).keys.foreach( right =>
            phrases(lhs)(left)(right) = phrases(lhs)(left)(right) / lhsTotal
          )
        )
      }

      lexicon.keys.foreach{ pos =>
        var posTotal = 0D
        lexicon( pos ).keys.foreach( word =>
          posTotal = posTotal + lexicon(pos)(word)
        )
        lexicon(pos).keys.foreach( word =>
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
      phrases.keys.foreach{ lhs =>
        phrases (lhs) .keys.foreach{ left =>
          phrases (lhs) (left) .keys.foreach{ right =>
            if( phrases(lhs)(left)(right) < cutoff )
              phrases(lhs)(left) -= right
          }
          if( phrases(lhs)(left).size == 0 )
            phrases(lhs) -= left
        }
        if( phrases (lhs) .size == 0 )
          phrases -= lhs
      }
      lexicon.keys.foreach{ pos =>
        lexicon (pos) .keys.foreach{ word =>
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
    var lexicon = new HashMap[
      String,       //  POS
      HashMap[
        String,     //  WORD
        Double      //  PROB
      ]
    ] {
      override def default(pos:String) = {
        this += Pair( pos,
              new HashMap[String,Double] {
            override def default(word:String) = {
              this += Pair( word, 0.0 )
              this(word)
            }
          }
        )
        this(pos)
      }
    }
    var lexExps = new HashMap[
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
    
    var phrases = new HashMap[
      String,       //  LHS
      HashMap[
        String,     //  LEFT
        HashMap[
          String,   //  RIGHT
          Double    //  PROB
        ]
      ]
    ] {
      override def default(lhs:String) = {
        this += Pair( lhs,
                      new HashMap[String, HashMap[String,Double]] {
            override def default(left:String) = {
              this += Pair( left, 
                            new HashMap[String,Double] {
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
    var phrExps = new HashMap[
      String,       //  LEFT
      HashMap[
        String,     //  RIGHT
        List[(
          String,   //  LHS
          Double    //  PROB
        )]
      ]
    ] {
      override def default(left:String) = {
        this += Pair( left,
                      new HashMap[String, List[(String,Double)]] {
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
    * Reads output from a Shakes*Manager from a file. Grammar should contain
    * both unary and binary rules.
    * 
    * @param gramPath File path to the grammar (relative to current working
    * directory)
    */
    def readShakesGram(gramPath:String) {
      import scala.io.Source._
      val lines = fromFile(gramPath).getLines.toList.drop(5)
      lines.foreach( line =>
        if( line.length > 1 ) {
          val fields = line.split(' ')

          if( fields.length == 4) {
            phrases (fields(0)) (fields(1)) (fields(2)) += fields(3).toDouble
            phrExps (fields(1)) (fields(2)) = (fields(0), fields(3).toDouble) ::
                                              phrExps (fields(1)) (fields(2))
          } else if( fields.length == 3) {
            lexicon (fields(0)) (fields(1)) += fields(2).toDouble
            lexExps (fields(1)) = (fields(0), fields(2).toDouble) ::
                                  lexExps(fields(1))
          }
        }
      )
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
      val lines = fromFile(gramPath).getLines
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
      val lines = fromFile(lexPath).getLines
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

      phrExps.keys.foreach{ phrExps(_).clear }
      phrases.keys.foreach( lhs =>
        phrases (lhs) .keys.foreach( left =>
          phrases (lhs) (left).keys.foreach ( right =>
            phrExps (left) (right) = (lhs, phrases(lhs)(left)(right)) ::
                                      phrExps (left) (right)
          )
        )
      )

      lexicon.keys.foreach( pos =>
        lexicon (pos) .keys.foreach ( word =>
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
      phrases.keys.foreach( lhs =>
        phrases (lhs) .keys.foreach( left =>
          phrases (lhs) (left) .keys.foreach( right =>
            copy.phrases(lhs)(left)(right) +=  0.0
          )
        )
      )
      lexicon.keys.foreach( pos =>
        lexicon (pos) .keys.foreach( word =>
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
      phrases.keys.foreach( lhs =>
        phrases(lhs).keys.foreach( left =>
          phrases(lhs)(left).keys.foreach{ right =>
            phrases(lhs)(left).clear
            phrases(lhs)(left)(right) += 0.0
          }
        )
      )
      lexicon.keys.foreach( pos =>
        lexicon(pos).keys.foreach{ word =>
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
      phrases.keys.map{ lhs =>
        phrases (lhs) .keys.map{ left =>
          phrases (lhs)(left) .keys.map{ right =>
            lhs + " " + left + " " + right + " " + 
            ("%1.30f" format phrases(lhs)(left)(right) )
          }.mkString("\n","\n","")
        }.mkString("","","")
      }.mkString("","","\n") +
      lexicon.keys.map{ pos =>
        lexicon (pos) .keys.map{ word =>
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
      phrases.keys.map{ lhs =>
        phrases (lhs) .keys.map{ left =>
          phrases (lhs)(left) .keys.map{ right =>
            lhs + " " + left + " " + right + " " + phrases(lhs)(left)(right)
          }.mkString("\t","\n\t","\n")
        }.mkString("","","")
      }.mkString("Phrases:\n\n","","\n") +
      lexicon.keys.map{ pos =>
        lexicon (pos) .keys.map{ word =>
          pos + " " + word + " " + lexicon(pos)(word)
        }.mkString("\t","\n\t","")
      }.mkString("Lexicon:\n\n","\n","")
  }

  // Use this to terminate parsers when we run out of sentences to give them.
  case object Stop

  /**
  * This defines what a parser must have, without giving an explicit definition
  * for certain functions we might want to change (such as re-estimation
  * functions). This must be extended by providing at least the actual parsing
  * functions synFill, lexFill, and populateChart. Other functions can of course
  * be provided as well, for example computeOPWithEstimates to provide outside
  * probability estimates.
  */
  trait ShakesParser {
    import Math._
    import collection.mutable.{HashMap,HashSet}


    var g:ShakesPCNF

    abstract class Entry(label:String) {
      import collection.mutable.ArrayBuffer
      import Math._

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
      var backMatcher = new ArrayBuffer[ArrayBuffer[(String,String)]]
      0 to (length-1) foreach( i => backMatcher += new ArrayBuffer[(String,String)])

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
          backMatcher = new ArrayBuffer[ArrayBuffer[(String,String)]]
          0 to (length-1) foreach( i => backMatcher += new ArrayBuffer[(String,String)])
        }


        if( !ipSetYet | prob > ip ) {
          backMatcher .clear
          backMatcher = new ArrayBuffer[ArrayBuffer[(String,String)]]
          0 to (length-1) foreach( i => backMatcher += new ArrayBuffer[(String,String)])
          backMatcher( split - start ) += (left,right)

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
          backMatcher = new ArrayBuffer[ArrayBuffer[(String,String)]]
          0 to (length-1) foreach( i => backMatcher += new ArrayBuffer[(String,String)])
        }

        backMatcher(split - start) += (left,right)
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
          new ArrayBuffer[(String,String)])
      }
      
      /**
      * Make a readable stringification for Entry.
      * @return A readable stringification for Entry.
      */
      override def toString = 
        "ip: "+ip+"; op: "+op;

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
        
        val (spans:ArrayBuffer[(String,String)],k) = spanInfo
        val (left:String,right:String) = spans(0)

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
        val word =
          backMatcher.toArray.zipWithIndex.find( !
            _._1.isEmpty
          ).toList.head._1(0)._1
        "(" + label + " " + word + ")"
      }
    }




    /**
    * This is the chart itself.
    */
    object chart {
      /**
      * This is the data structure that stores the entries.
      */
      var triangMatrix = Array.fromFunction( x =>
        Array.fromFunction( y =>
          new HashMap[String,Entry] {
            override def default(s:String) = {
              this += Pair( s, new SynEntry(s) )
              this(s)
            }
          }
        )(0) // just initialization
      )(0) 


      /**
      * Chart can be resized, which allows the same parser to parse sentences of
      * different lengths. This is necessary so the same parser can parse
      * multiple sentences without assuming a fixed upper bound on sentence
      * length.
      * @param chartSize The size of the chart.
      */
      def resize(chartSize:Int) {
        var rowCount = 0
        triangMatrix = Array.fromFunction( x =>
          {
            rowCount = rowCount + 1
            Array.fromFunction( y =>
              new HashMap[String,Entry] {
                override def default(s:String) = {
                  this += Pair( s, new SynEntry(s) )
                  this(s)
                }
              }
            )(chartSize - (rowCount - 1))
          }
        )(chartSize)
      }

      /**
      * Access a hashmap of entries from the Chart
      * 
      * @param n1 Start index of the span.
      * @param n2 End index of the span.
      * @return A hashmap of Entries over the span.
      */
      def apply(n1:Int)(n2:Int) =
        triangMatrix(n1)(n2-n1)

      /**
      * Clear out all entries from the chart.
      */
      def clear {
        triangMatrix.foreach( i =>
          i.foreach( j =>
            j.foreach( _._2.reset)
          )
        )
      }

      /**
      * @treturn The size of the chart.
      */
      def size = triangMatrix.size

      /**
      * A map function for the chart.
      * @param p The function to be applied.
      * @return The function applied to the rows of the chart.
      */
      def map( p: Array[HashMap[String,Entry]] => Any ) = triangMatrix.map{ p(_) }

      /**
      * Stringify the chart.
      * @return A human-readable(ish) string representation of the chart.
      */
      override def toString =
        triangMatrix.map( i =>
          Array.make(triangMatrix.size - i.length, "<\t>\t").mkString("","","") +
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
    * Clear out the entries of the chart.
    */
    def clear = chart.clear

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
    def chartDescent( p: ( Entry => {} ) ) = {
      import collection.mutable.{HashMap,HashSet}

      if( chart(0)(chart.size - 1).contains("S") ) {
        val toCompute = new HashMap[(Int,Int),HashSet[String]] {
          override def default(key:(Int,Int)) = {
            this += Pair(key, new HashSet[String])
            this(key)
          }
        }

        toCompute(0, size - 1) += "S"

        while( !toCompute.isEmpty) {
          val(start,end) = 
            toCompute.keys.foldLeft[(Int,Int)](0,0)( (a,b) =>
              if( (a._2 - a._1) > (b._2 - b._1) ) a else b
          )
          val labels = toCompute( (start,end) )
          toCompute -= Tuple2(start,end)

          labels.foreach{ l =>
              val rootCell = chart(start)(end)(l)
              p( chart(start)(end)(l) )

              List.range(0, rootCell.backMatcher.length).foreach{ split =>
                val splitPoint = rootCell.start + split
                rootCell.backMatcher(split).foreach{ children =>
                  val left = children._1
                  val right = children._2

                  if( splitPoint - rootCell.start > 1 )
                    toCompute( (rootCell.start, splitPoint)) += left

                  if( rootCell.end - splitPoint > 1)
                    toCompute ( (splitPoint, rootCell.end) ) += right

                }
              }
          }
        }
      }
    }
  }//end ShakesParser

  /**
  * This provides chart filling definitions for CYK parsing
  */
  trait ShakesFullCYKParser extends ShakesParser {
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
        chart(start)(k).keys.foreach{ left =>
          chart(k)(end).keys.foreach{ right =>
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

  }

  abstract class ShakesDistributedParser extends ShakesFullCYKParser with Actor

  /**
  * This provides a real parser that can provide a parse chart with partial
  * estimates all filled in.
  *
  * @param id Parser id so parser can identify itself to parser managers, etc.
  * @param grammar The grammar the parser should use
  * @param ws The factor by which to scale words while parsing (to avoid
  * underflow)
  */
  class ShakesEstimatingParser(id:Int,grammar:ShakesPCNF,ws:Int)
    extends ShakesDistributedParser {//with ShakesFullCYKParser with Actor { 
    import Math._
    import collection.mutable.HashMap

    var g = grammar
    var wordScale = ws
    
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
        
        (0 to (j-2) reverse) foreach{ i =>
          synFill(i, j)
        }
      }

      /*
      if( !root.contains("S") )
        println("SENTENCE DID NOT PARSE") 
      else
        println("SENTENCE " + s.mkString(""," ","") + " PARSED")
      */

      chartDescent( computeOPWithEstimates )
    }

    /**
    * Compute the outside probability for one entry and gather estimated counts
    * based on the entry. Assumes that all referenced values are already
    * computed.
    * @param ent The entry to be scored and counted.
    */
    def computeOPWithEstimates(ent:Entry) {
      import collection.mutable.HashMap
      import Math._

      val h_Summand = ent.op * ent.ip
      val h_Key = Tuple3(ent.start,ent.end,ent.l)
      h_i( h_Key ) = h_Summand

      val f_toAdd = new HashMap[
          String,
          HashMap[String,Double]
        ]{
          override def default(left:String) = {
          this += Pair( left,
                        new HashMap[String,Double] {
              override def default(right:String) = 0
            }
          )
          this(left)
        }
      }


      0 to (ent.backMatcher.size-1) foreach{ split =>
        ent.backMatcher(split).foreach{ matches =>
          val left = matches._1
          val right = matches._2
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

              val g_Key = (leftEnt.start, leftEnt.l, leftEnt.backMatcher(0)(0)._1)
              g_i( g_Key ) = g_Summand

              val h_Summand = leftEnt.ip * leftEnt.op
              val h_Key = Tuple3( leftEnt.start, leftEnt.end, leftEnt.l)
              h_i( h_Key ) = h_Summand
            }
            case _ =>
          }
          rightEnt match {
            case LexEntry(_) => {
              val g_Summand = rightEnt.ip * rightEnt.op

              val g_Key = (rightEnt.start, rightEnt.l, rightEnt.backMatcher(0)(0)._1)
              g_i( g_Key ) = g_Summand

              val h_Summand = leftEnt.ip * leftEnt.op
              val h_Key = Tuple3( rightEnt.start, rightEnt.end, rightEnt.l)
              h_i( h_Key ) = h_Summand
            }
            case _ =>
          }

          val f_Summand =
            ent.op * ruleProb * leftEnt.ip * rightEnt.ip
          f_toAdd (left) (right) =
              f_toAdd (left) (right) + f_Summand
        }
      }



      f_toAdd .keys.foreach( left =>
        f_toAdd (left) .keys.foreach{ right =>
          f_i ((ent.start,ent.end,ent.l))(left)(right) =
              f_i ((ent.start,ent.end,ent.l))(left)(right) +
              f_toAdd (left)(right)
        }
      )
    }


    /**
    * Compute the outside probability for one entry. Assumes that all referenced
    * values are already computed.
    * @param ent The entry to be scored.
    */
    def computeOP(ent:Entry) {
      import Math._

      List.range(0,ent.backMatcher.size - 1).foreach{ split =>
        ent.backMatcher(split).foreach{ matches =>
          val splitPoint  = split + ent.start

          val left = matches._1
          val right = matches._2
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

    /**
    * This stores intermediate counts of binary-branching nodes for this sentence.
    */
    val f_i = new HashMap[
      (Int,Int,String),
      HashMap[
        String,
        HashMap[String,Double]
      ]
    ] {
      override def default(lhs:(Int,Int,String)) = {
        this += Pair( lhs,
                      new HashMap[String, HashMap[String,Double]] {
            override def default(left:String) = {
              this += Pair( left,
                            new HashMap[String,Double] {
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

    /**
    * This stores intermediate counts of unary-branching nodes for this sentence.
    */
    val g_i = new HashMap[(Int,String,String), Double] {
      override def default(key:(Int,String,String)) = 0
    }

    /**
    * This stores intermediate counts of non-terminal nodes for this sentence.
    */
    val h_i = new HashMap[(Int,Int,String), Double] {
      override def default(key:(Int,Int,String)) = 0
    }


    /**
    * Use this as an actor
    */
    def receive = {
      case s:String => {  // If we get a sentence, then parse it and send the
                          // counts back
        f_i.clear
        g_i.clear
        h_i.clear

        val words = s.split(' ')
        resize( words.size+1 )

        populateChart(words)

        if( !root.contains("S") ) {
          println("WARNING: SENTENCE DID NOT PARSE")
          println( s )
        }

        val scaledBy = pow( wordScale, size - 1 )


        //sender ! (id,scaledStringProb,f_i,g_i,h_i,scaledBy)
        reply (id,scaledStringProb,f_i,g_i,h_i,scaledBy)
      }
      case Stop => {      // If we get the stop signal, then shut down.
        println("Parser " + id + " stopping")
        exit
      }
    }
  }

  //case class ToParse(s:String)


    /**
    * Create many estimating parsers and estimate a grammar. Use this by
    * implementing useGrammar (this receives the trained grammar after every
    * iteration), stoppingCondition (iterations continue until this returns true),
    * and parserConstructor (builds an array of parsers)
    * @param initGram The initial random grammar
    * @param trainingCorpus The training corpus the parsers will use
    */
    abstract class ShakesRemoteParserManager(initGram:ShakesPCNF) extends Actor {
      import Math._
      import collection.mutable.{HashMap,ArrayBuffer}

      def stoppingCondition( iterNum:Int, deltaLogProb:Double ):Boolean
      def parserConstructor:ArrayBuffer[Actor]
      def useGrammar(g:ShakesPCNF,iterNum:Int):Unit
      def cleanup:Unit

      var g1 = initGram
      var g2 = g1.countlessCopy

      val trainCorpus:ShakesTrainCorpus

      var iterNum = 0
      var deltaLogProb = 1.0
      var lastCorpusLogProb = 0.0
      var corpusLogProb = 0.0

      var stringID = 0

      var parsers = parserConstructor
      var numFinishedParsers = 0



      parsers.foreach( p =>
        {
          //p.start
          println("Sending " + trainCorpus( stringID ) + " to parser " +
          stringID )
          p ! trainCorpus( stringID )
          stringID += 1
        }
      )

      def receive = {
        case (
          id:Int,
          scaledStringProb:Double,
          f_i:HashMap[(Int,Int,String),HashMap[String,HashMap[String,Double]]],
          g_i:HashMap[(Int,String,String),Double],
          h_i:HashMap[(Int,Int,String),Double],
          scaledBy:Double
        ) => {

          corpusLogProb = corpusLogProb + Math.log( scaledStringProb ) -
            Math.log( scaledBy )

          f_i.keys.foreach( lhs =>
            f_i (lhs) .keys.foreach( left =>
              f_i (lhs)(left) .keys.foreach{ right =>
                g2.f (lhs._3)(left)(right) =
                  g2.f (lhs._3)(left)(right) +
                  (
                    f_i (lhs)(left)(right) /
                    scaledStringProb
                  )
              }
            )
          )
          g_i.keys.foreach{ k =>
            val index = k._1
            val pos = k._2
            val word = k._3
            g2.g( (pos,word) ) = 
              g2.g( (pos,word) ) +
              (
                g_i( k ) /
                scaledStringProb
              )
          }
          h_i.keys.foreach{ k =>
            val start = k._1
            val end = k._2
            val label = k._3
            g2.h(label) =
              g2.h(label) + 
              (
                h_i(k) /
                scaledStringProb
              )
          }

          if( stringID < trainCorpus.size ) {

            if( stringID % 100 == 0 )
              println("Sending " + trainCorpus( stringID ) + " to parser " + id )

            parsers(id) !  trainCorpus( stringID )
            stringID += 1
          } else {
            //parsers(id) ! Stop
            //parsers(id).stop
            numFinishedParsers += 1

            if( numFinishedParsers >= parsers.size ) {  // we have results from
                                                        // every sentence
              g2.reestimateRules
              g1 = g2.copy
              g2 = g1.countlessCopy

              deltaLogProb = (lastCorpusLogProb - corpusLogProb) /
                abs(corpusLogProb)

              useGrammar( g1, iterNum )

              println("corpusLogProb.Iter"+iterNum + ": "+ corpusLogProb)
              println("deltaLogProb.Iter"+iterNum + ": "+ deltaLogProb)


              
              if( ! stoppingCondition( iterNum, deltaLogProb ) ) {
                iterNum += 1
                stringID = 0
                numFinishedParsers = 0
                lastCorpusLogProb = corpusLogProb
                corpusLogProb = 0.0
                parsers = parserConstructor

                parsers foreach( p =>
                  {
                    //p.start
                    println("Sending " + trainCorpus( stringID ) + " to parser " +
                    stringID )
                    p ! trainCorpus( stringID )
                    stringID += 1
                  }
                )
              } else {
                cleanup
                exit
              }
            }
          }

      }

    }
  }

  
    /**
    * Create many estimating parsers and estimate a grammar. Use this by
    * implementing useGrammar (this receives the trained grammar after every
    * iteration), stoppingCondition (iterations continue until this returns true),
    * and parserConstructor (builds an array of parsers)
    * @param initGram The initial random grammar
    * @param trainingCorpus The training corpus the parsers will use
    */
    abstract class ShakesParserManager(initGram:ShakesPCNF) extends Actor {
      import Math._
      import collection.mutable.{HashMap,ArrayBuffer}

      def stoppingCondition( iterNum:Int, deltaLogProb:Double ):Boolean
      def parserConstructor:ArrayBuffer[ShakesDistributedParser]
      def useGrammar(g:ShakesPCNF,iterNum:Int):Unit
      def cleanup:Unit

      var g1 = initGram
      var g2 = g1.countlessCopy

      val trainCorpus:ShakesTrainCorpus

      var iterNum = 0
      var deltaLogProb = 1.0
      var lastCorpusLogProb = 0.0
      var corpusLogProb = 0.0

      var stringID = 0

      var parsers = parserConstructor
      var numFinishedParsers = 0



      parsers.foreach( p =>
        {
          p.start
          println("Sending " + trainCorpus( stringID ) + " to parser " +
          stringID )
          p ! trainCorpus( stringID )
          stringID += 1
        }
      )

      def receive = {
        case (
          id:Int,
          scaledStringProb:Double,
          f_i:HashMap[(Int,Int,String),HashMap[String,HashMap[String,Double]]],
          g_i:HashMap[(Int,String,String),Double],
          h_i:HashMap[(Int,Int,String),Double],
          scaledBy:Double
        ) => {

          corpusLogProb = corpusLogProb + Math.log( scaledStringProb ) -
            Math.log( scaledBy )

          f_i.keys.foreach( lhs =>
            f_i (lhs) .keys.foreach( left =>
              f_i (lhs)(left) .keys.foreach{ right =>
                g2.f (lhs._3)(left)(right) =
                  g2.f (lhs._3)(left)(right) +
                  (
                    f_i (lhs)(left)(right) /
                    scaledStringProb
                  )
              }
            )
          )
          g_i.keys.foreach{ k =>
            val index = k._1
            val pos = k._2
            val word = k._3
            g2.g( (pos,word) ) = 
              g2.g( (pos,word) ) +
              (
                g_i( k ) /
                scaledStringProb
              )
          }
          h_i.keys.foreach{ k =>
            val start = k._1
            val end = k._2
            val label = k._3
            g2.h(label) =
              g2.h(label) + 
              (
                h_i(k) /
                scaledStringProb
              )
          }

          if( stringID < trainCorpus.size ) {

            if( stringID % 100 == 0 )
              println("Sending " + trainCorpus( stringID ) + " to parser " + id )

            parsers(id) !  trainCorpus( stringID )
            stringID += 1
          } else {
            //parsers(id) ! Stop
            parsers(id).stop
            numFinishedParsers += 1

            if( numFinishedParsers >= parsers.size ) {  // we have results from
                                                        // every sentence
              g2.reestimateRules
              g1 = g2.copy
              g2 = g1.countlessCopy

              deltaLogProb = (lastCorpusLogProb - corpusLogProb) /
                abs(corpusLogProb)

              useGrammar( g1, iterNum )

              println("corpusLogProb.Iter"+iterNum + ": "+ corpusLogProb)
              println("deltaLogProb.Iter"+iterNum + ": "+ deltaLogProb)


              
              if( ! stoppingCondition( iterNum, deltaLogProb ) ) {
                iterNum += 1
                stringID = 0
                numFinishedParsers = 0
                lastCorpusLogProb = corpusLogProb
                corpusLogProb = 0.0
                parsers = parserConstructor

                parsers foreach( p =>
                  {
                    p.start
                    println("Sending " + trainCorpus( stringID ) + " to parser " +
                    stringID )
                    p ! trainCorpus( stringID )
                    stringID += 1
                  }
                )
              } else {
                cleanup
                exit
              }
            }
          }

      }

    }
  }

  //  /**
  //  * Create many estimating parsers and estimate a grammar. Use this by
  //  * implementing useGrammar (this receives the trained grammar after every
  //  * iteration), stoppingCondition (iterations continue until this returns true),
  //  * and parserConstructor (builds an array of parsers)
  //  * @param initGram The initial random grammar
  //  * @param trainingCorpus The training corpus the parsers will use
  //  */
  //  abstract class ShakesParserManager(
  //    initGram:ShakesPCNF,
  //    trainingCorpus:ShakesTrainCorpus ) extends Actor {
  //    import scala.collection.mutable.{ArrayBuffer,HashMap}
  //    import Math._
  //
  //
  //    //var trainingCorpus:ShakesTrainCorpus
  //    var g1 = initGram
  //    var g2:ShakesPCNF = g1.countlessCopy
  //    //g2.reestimateCounter = g1.reestimateCounter
  //
  //    //var parsers:Array[ShakesDistributedParser]
  //
  //    def stoppingCondition(n:Int,x:Double):Boolean
  //
  //    def parserConstructor:ArrayBuffer[ShakesDistributedParser]
  //
  //    def cleanup:{}
  //
  //    def useGrammar(g:ShakesPCNF,iterNum:Int):Unit
  //    var iterationNum = 0
  //    var deltaLogProb = 1.0
  //    var lastCorpusLogProb = 0.0
  //    var corpusLogProb = 0.0
  //
  //
  //    def receive {
  //      //trapExit = true
  //      iterationNum = 0
  //      deltaLogProb = 1.0
  //      lastCorpusLogProb = 0.0
  //      corpusLogProb = 0.0
  //
  //      while( ! stoppingCondition( iterationNum, deltaLogProb ) ) {
  //
  //        val parsers = parserConstructor
  //
  //
  //        parsers.foreach( _.start )
  //
  //        println("Beginning to parse iteration " + iterationNum + "...\n\n")
  //        List.range(0, parsers.size).foreach{ id =>
  //          println( "Sending sentence number " + id + " to parser " + id )
  //          parsers(id) ! trainingCorpus(id)
  //        }
  //
  //        var sentenceNumber = parsers.size - 1
  //        var numFinishedParsers = 0
  //
  //        while( numFinishedParsers < parsers.size ) {
  //          receive {
  //            case (
  //                    id:Int,
  //                    scaledStringProb:Double,
  //                    f_i:HashMap[(Int,Int,String),HashMap[String,HashMap[String,Double]]],
  //                    g_i:HashMap[(Int,String,String),Double],
  //                    h_i:HashMap[(Int,Int,String),Double],
  //                    scaledBy:Double
  //            ) => {
  //
  //              corpusLogProb = corpusLogProb + log( scaledStringProb ) -
  //                log( scaledBy )
  //
  //              f_i.keys.foreach( lhs =>
  //                f_i (lhs) .keys.foreach( left =>
  //                  f_i (lhs)(left) .keys.foreach{ right =>
  //                    g2.f (lhs._3)(left)(right) =
  //                      g2.f (lhs._3)(left)(right) +
  //                      (
  //                        f_i (lhs)(left)(right) /
  //                        scaledStringProb
  //                      )
  //                  }
  //                )
  //              )
  //              g_i.keys.foreach{ k =>
  //                val index = k._1
  //                val pos = k._2
  //                val word = k._3
  //                g2.g( (pos,word) ) = 
  //                  g2.g( (pos,word) ) +
  //                  (
  //                    g_i( k ) /
  //                    scaledStringProb
  //                  )
  //              }
  //              h_i.keys.foreach{ k =>
  //                val start = k._1
  //                val end = k._2
  //                val label = k._3
  //                g2.h(label) =
  //                  g2.h(label) + 
  //                  (
  //                    h_i(k) /
  //                    scaledStringProb
  //                  )
  //              }
  //
  //
  //              sentenceNumber = sentenceNumber + 1
  //
  //              if( sentenceNumber >= trainingCorpus.size) {
  //                numFinishedParsers = numFinishedParsers + 1
  //              } else {
  //                if( sentenceNumber % 100 == 0 )
  //                  println(
  //                    "Starting sentence number " + sentenceNumber  + " with parser " +
  //                    id
  //                  )
  //                parsers(id) ! trainingCorpus(sentenceNumber)
  //              }
  //
  //            }
  //          }
  //        }
  //
  //
  //        parsers.foreach( _ ! Stop )
  //
  //        g2.reestimateRules
  //
  //        g1 = g2
  //        g2 = g1.countlessCopy
  //        //g2.reestimateCounter = g1.reestimateCounter
  //
  //
  //        deltaLogProb = (lastCorpusLogProb - corpusLogProb) / abs(corpusLogProb)
  //
  //        println("corpusLogProb.Iter"+iterationNum + ": "+ corpusLogProb)
  //        println("deltaLogProb.Iter"+iterationNum + ": "+ deltaLogProb)
  //
  //        useGrammar( g1 , iterationNum)
  //
  //        lastCorpusLogProb = corpusLogProb
  //        corpusLogProb = 0.0
  //
  //        iterationNum = iterationNum + 1
  //
  //
  //
  //      }
  //      useGrammar( g1, iterationNum )
  //
  //      cleanup
  //
  //      exit()
  //    }
  //  }



  /**
  * This implements the extension of standard EM from Pereira and Schabes (1992)
  * by simply overriding the relevant methods from the ShakesParser class.
  * 
  * @param id The id number of this parser (so it can identify itself easily to
  * whatever starts it).
  * @param g The grammar that this parser should use when parsing a sentence and
  * producing partial counts.
  * @param ws Amount to scale terminal probabilities by
  */
  class ShakesBracketedParser(id:Int,grammar:ShakesPCNF,ws:Int)
    extends ShakesEstimatingParser(id,grammar,ws) {
    import Math._
    import collection.mutable.{HashSet,HashMap}

    var bracketing:HashSet[(Int,Int)] = new HashSet

    def isCompatible(start:Int, end:Int):Boolean =
      ! bracketing.exists{ span =>
        val left = span._1
        val right = span._2
        
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
    override def populateChart(s:Array[String]) = {
      List.range(1,s.size+1).foreach{ j =>
        lexFill( s(j-1), j-1)
        
        List.range(0,j-1).reverse.foreach{ i =>
          if( isCompatible( i, j ) )
            synFill(i, j)
          //else
            //println("skipped cell " + (i,j))
        }
      }
      chartDescent( computeOPWithEstimates )
    }

    println("I DONE BEEN STARTED")
    
    start
    override def receive = {
      case Tuple2(s:String,b:HashSet[(Int,Int)]) => {
                          // If we get a sentence, then parse it and send the
                          // counts back
        f_i.clear
        g_i.clear
        h_i.clear
        bracketing = b


        val words = s.split(' ')
        resize(words.size + 1)

        println( "I DONE GOT A STRING CALLED " + s )

        populateChart(words)

        if( !root.contains("S") ) {
          println("WARNING: SENTENCE DID NOT PARSE")
          println( s )
        }

        val scaledBy = pow( wordScale , size - 1 )
        //sender ! (id,scaledStringProb,f_i,g_i,h_i, scaledBy)
        reply (id,scaledStringProb,f_i,g_i,h_i, scaledBy)
      }
      case Stop => {      // If we get the stop signal, then shut down.
        println("Parser " + id + " stopping")
        exit
      }
    }
  }

  class ShakesViterbiParser(grammar:ShakesPCNF,ws:Int) extends ShakesParser {
    var g = grammar
    var wordScale = ws

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
      //println( w + ": \t" + g.lexExps(w) )
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
        chart(start)(k).keys.foreach{ left =>
          chart(k)(end).keys.foreach{ right =>
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

    def parseString:String = chart(0)(chart.size - 1)("S").viterbiString
  }


  abstract class EvaluationActor(initGram:ShakesPCNF,ws:Int)
    extends ShakesViterbiParser(initGram,ws) with Actor {
    //var iterNum = 0
    //var lastGo = false
    var testCorpus:List[String]
    def receive = {
      case (intermediateGram:ShakesPCNF, iterNum:Int) =>
        if( iterNum % 2 == 0 )
        {
          g = intermediateGram
          testCorpus.foreach{ testSentence =>
            val words = testSentence.split(' ')
            clear
            resize( words.size + 1 )
            populateChart( words )

          if( !root.contains("S") ) {
            println("WARNING: SENTENCE DID NOT PARSE")
            println( testSentence )
          }

            println("Parses.Iter" + iterNum + ": " + parseString )
          }
        }
      
      case (intermediateGram:ShakesPCNF, Stop) => {

        g = intermediateGram
        testCorpus.foreach{ testSentence =>
          val words = testSentence.split(' ')
          clear
          resize( words.size + 1 )
          populateChart( words )
          println("Parses.Converged" + ": " + parseString )
        }

        println("VitActor exiting.")
        exit

      }
    }
  }

  /**
  * Training corpus classes. Apply should return whatever counts as one sentence
  * (whether it's a string or a tuple of a string and a bracketing), size should
  * give the number of sentences, and readCorpus should read in the corpus from
  * file(s)
  */
  trait ShakesTrainCorpus {
    def apply(n:Int):Any
    def size:Int
    def readCorpus(pathSpecifier:String)
  }

  /**
  * Reads, stores, and makes useful a partially bracketed corpus.
  */
  class BracketedCorpus extends ShakesTrainCorpus {
    import collection.mutable.HashSet

    var bracketings:Array[HashSet[(Int,Int)]] = Array()
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


      strings = fromFile(stringsPath).getLines.toList.filter(_.length > 0).map(_.replace("\n","")).toArray


      val corpusSize = strings.size

      bracketings = Array.fromFunction( i =>
        new HashSet[(Int,Int)]
      )(corpusSize + 1 )

      val lines = fromFile(bracketsPath).getLines
      lines.foreach( line =>
        if( line.length > 1 ) {
          val fields = line.replace("\n","").split(' ')
          
          val (start,end) = ( fields(1).toInt, fields(2).toInt )
          if( end - start > 1 )
            bracketings( fields(0).toInt ) +=
              Tuple2( fields(1).toInt, fields(2).toInt )
        }
      )
    }
    def apply(n:Int) = Tuple2(strings(n),bracketings(n))
    def size = strings.size
    override def toString = strings mkString("","\n","")
  }

  /**
  * Reads, stores, and makes useful a corpus with only strings
  */
  class StringsOnlyCorpus extends ShakesTrainCorpus {
    var strings:Array[String] = Array()
    def apply(n:Int) = strings( n )
    def size = strings.size

    def readCorpus( stringsPath:String ) {
      import scala.io.Source._
      strings = fromFile(stringsPath).getLines.toList.filter(_.length > 0).map(_.replace("\n","")).toArray
    }
    override def toString = strings mkString("","\n","")
  }


//  abstract class ShakesParserManager {
//
//
//
//  }

} // END PACKAGE


  //  /**
  //  * Create many estimating parsers and estimate a grammar. Use this by
  //  * implementing useGrammar (this receives the trained grammar after every
  //  * iteration), stoppingCondition (iterations continue until this returns true),
  //  * and parserConstructor (builds an array of parsers)
  //  * @param initGram The initial random grammar
  //  * @param trainingCorpus The training corpus the parsers will use
  //  */
  //  abstract class ShakesParserManager(
  //    initGram:ShakesPCNF,
  //    trainingCorpus:ShakesTrainCorpus ) extends Actor {
  //    import scala.collection.mutable.{ArrayBuffer,HashMap}
  //    import Math._
  //
  //
  //    //var trainingCorpus:ShakesTrainCorpus
  //    var g1 = initGram
  //    var g2:ShakesPCNF = g1.countlessCopy
  //    //g2.reestimateCounter = g1.reestimateCounter
  //
  //    //var parsers:Array[ShakesDistributedParser]
  //
  //    def stoppingCondition(n:Int,x:Double):Boolean
  //
  //    def parserConstructor:ArrayBuffer[ShakesDistributedParser]
  //
  //    def cleanup:{}
  //
  //    def useGrammar(g:ShakesPCNF,iterNum:Int):Unit
  //    var iterationNum = 0
  //    var deltaLogProb = 1.0
  //    var lastCorpusLogProb = 0.0
  //    var corpusLogProb = 0.0
  //
  //
  //    def receive {
  //      //trapExit = true
  //      iterationNum = 0
  //      deltaLogProb = 1.0
  //      lastCorpusLogProb = 0.0
  //      corpusLogProb = 0.0
  //
  //      while( ! stoppingCondition( iterationNum, deltaLogProb ) ) {
  //
  //        val parsers = parserConstructor
  //
  //
  //        parsers.foreach( _.start )
  //
  //        println("Beginning to parse iteration " + iterationNum + "...\n\n")
  //        List.range(0, parsers.size).foreach{ id =>
  //          println( "Sending sentence number " + id + " to parser " + id )
  //          parsers(id) ! trainingCorpus(id)
  //        }
  //
  //        var sentenceNumber = parsers.size - 1
  //        var numFinishedParsers = 0
  //
  //        while( numFinishedParsers < parsers.size ) {
  //          receive {
  //            case (
  //                    id:Int,
  //                    scaledStringProb:Double,
  //                    f_i:HashMap[(Int,Int,String),HashMap[String,HashMap[String,Double]]],
  //                    g_i:HashMap[(Int,String,String),Double],
  //                    h_i:HashMap[(Int,Int,String),Double],
  //                    scaledBy:Double
  //            ) => {
  //
  //              corpusLogProb = corpusLogProb + log( scaledStringProb ) -
  //                log( scaledBy )
  //
  //              f_i.keys.foreach( lhs =>
  //                f_i (lhs) .keys.foreach( left =>
  //                  f_i (lhs)(left) .keys.foreach{ right =>
  //                    g2.f (lhs._3)(left)(right) =
  //                      g2.f (lhs._3)(left)(right) +
  //                      (
  //                        f_i (lhs)(left)(right) /
  //                        scaledStringProb
  //                      )
  //                  }
  //                )
  //              )
  //              g_i.keys.foreach{ k =>
  //                val index = k._1
  //                val pos = k._2
  //                val word = k._3
  //                g2.g( (pos,word) ) = 
  //                  g2.g( (pos,word) ) +
  //                  (
  //                    g_i( k ) /
  //                    scaledStringProb
  //                  )
  //              }
  //              h_i.keys.foreach{ k =>
  //                val start = k._1
  //                val end = k._2
  //                val label = k._3
  //                g2.h(label) =
  //                  g2.h(label) + 
  //                  (
  //                    h_i(k) /
  //                    scaledStringProb
  //                  )
  //              }
  //
  //
  //              sentenceNumber = sentenceNumber + 1
  //
  //              if( sentenceNumber >= trainingCorpus.size) {
  //                numFinishedParsers = numFinishedParsers + 1
  //              } else {
  //                if( sentenceNumber % 100 == 0 )
  //                  println(
  //                    "Starting sentence number " + sentenceNumber  + " with parser " +
  //                    id
  //                  )
  //                parsers(id) ! trainingCorpus(sentenceNumber)
  //              }
  //
  //            }
  //          }
  //        }
  //
  //
  //        parsers.foreach( _ ! Stop )
  //
  //        g2.reestimateRules
  //
  //        g1 = g2
  //        g2 = g1.countlessCopy
  //        //g2.reestimateCounter = g1.reestimateCounter
  //
  //
  //        deltaLogProb = (lastCorpusLogProb - corpusLogProb) / abs(corpusLogProb)
  //
  //        println("corpusLogProb.Iter"+iterationNum + ": "+ corpusLogProb)
  //        println("deltaLogProb.Iter"+iterationNum + ": "+ deltaLogProb)
  //
  //        useGrammar( g1 , iterationNum)
  //
  //        lastCorpusLogProb = corpusLogProb
  //        corpusLogProb = 0.0
  //
  //        iterationNum = iterationNum + 1
  //
  //
  //
  //      }
  //      useGrammar( g1, iterationNum )
  //
  //      cleanup
  //
  //      exit()
  //    }
  //  }
  
