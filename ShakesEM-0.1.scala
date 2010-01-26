/**
* ShakesEM is a package for running EM for Probabilistic Context Free Grammars,
* implemented in Scala using the Actors library to allow parallel processing
* over multiple cores. This is the first release of the package and implements
* only vanilla EM. I'm fairly sure it computes what I say it does, but there's
* also almost definitely a lot of room for improvement in speed and space
* efficiency.
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
* @version 0.1
* @author John K Pate
*/
package ShakesEM {
  import scala.actors.Actor
  import scala.actors.Actor._

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
                   override def default(right:String) = log(0)
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
      override def default(key:(String,String)) = log(0)
    }
    val h = new HashMap[String,Double] {
      override def default(key:String) = log(0)
    }



    /**
    * Re-estimates a PCFG based on the counts in f, g, and h. All the action of
    * this function lies in its side-effects.
    */
    def reestimateRules {
      f.keys.foreach( lhs =>
        f (lhs) .keys.foreach( left =>
          f (lhs)(left) .keys.foreach( right =>
            phrases (lhs)(left)(right) =
              exp(
                f (lhs)(left)(right) -
                h (lhs)
              )
          )
        )
      )
      g.keys.foreach{ k =>
        lexicon (k._1) (k._2) = exp(g(k) - h(k._1))
      }
      f.clear
      g.clear
      h.clear
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
      phrExps.keys.foreach{ phrExps(_).clear }
      phrases.keys.foreach( lhs =>
        phrases (lhs) .keys.foreach( left =>
          phrases (lhs) (left).keys.foreach ( right =>
            phrExps (left) (right) = (lhs, phrases(lhs)(left)(right)) ::
                                      phrExps (left) (right)
          )
        )
      )
      lexExps.clear
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
            lhs + " " + left + " " + right + " " + phrases(lhs)(left)(right)
          }.mkString("\n","\n","")
        }.mkString("","","")
      }.mkString("","","\n") +
      lexicon.keys.map{ pos =>
        lexicon (pos) .keys.map{ word =>
          pos + " " + word + " " + lexicon(pos)(word)
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
  * This is an Actor for distributing the sentences from one corpus over as many
  * cores as you happen to have access to. This should normally be started and
  * managed through a ShakesParserManager.
  *
  * @param id The id number of this parser (so it can identify itself easily to
  * whatever starts it).
  * @param g The grammar that this parser should use when parsing a sentence and
  * producing partial counts.
  */
  class ShakesParser(id:Int,g:ShakesPCNF) extends Actor {
    import Math._
    import collection.mutable.{HashMap,HashSet}

    /**
    * For use in chart parsing. Each entry should record inside and outside
    * probabilities, as well as its children.
    *
    * @param label The label that this is an entry for.
    */
    abstract class Entry(label:String) {
      import collection.mutable.Stack
      import Math._

      /**
      * Store these in log-space for simplicity.
      */
      var ip:Double = 0.0 // Init to something ridiculous for simplicity
      var op:Double = 0.0 // Init to something ridiculous; computed after parse

      var ipSetYet = false
      var opSetYet = false

      /**
      * Keeps track of a node's children for the outside pass.
      */
      var backMatcher = new Array[Stack[(String,String)]](length)
      List.range(0,length).foreach(backMatcher(_) = new Stack[(String,String)])

      var start = 0 // Init to something ridiculous for simplicity
      var end = 0 // Init to something ridiculous for simplicity

      def length = end - start + 1
      val l = label

      /**
      * Get inside probability as a probability (i.e. not in log-space)
      * @return The inside probability of the node.
      */
      def inProb = exp(ip)
      /**
      * Get outside probability as a probability (i.e. not in log-space)
      * @return The outside probability of the node.
      */
      def outProb = exp(op)

      /**
      * Easily set the outside probability by providing a probability in
      * log-space.
      * @param x The amount (in log-space) to set the outside probability to.
      */
      def setOPScore(x:Double)
        { opSetYet = true; op = x }
      /**
      * Easily increment the outside probability by providing a probability in
      * log-space.
      * @param x The amount (in log-space) to increment the outside probability by.
      */
      def incOPScore(x:Double) {
        if(!opSetYet)
          setOPScore(x)
        else
          op = log(exp(x) + exp(op))
      }
      /**
      * Easily set the outside probability by providing a probability not in
      * log-space.
      * @param x The amount to set the outside probability to.
      */
      def setOPProb(x:Double)
        { opSetYet = true; op = log(x) }
      /**
      * Easily increment the outside probability by providing a probability not
      * in log-space.
      * @param x The amount to increment the outside probability by.
      */
      def incOPProb(x:Double) {
        if(!opSetYet)
          setOPProb(x)
        else
          op = log(x + exp(op))
      }

      /**
      * Easily set the inside probability by providing a probability in
      * log-space.
      * @param x The amount (in log-space) to set the inside probability to.
      */
      def setIPScore(x:Double)
        { ipSetYet = true; ip = x }
      /**
      * Easily increment the inside probability by providing a probability in
      * log-space.
      * @param x The amount (in log-space) to increment the inside probability by.
      */
      def incIPScore(x:Double) {
        if(!ipSetYet)
          setIPScore(x)
        else
          ip = log(exp(x) + exp(ip))
      }
      /**
      * Easily increment the inside probability by providing a probability not
      * in log-space.
      * @param x The amount to increment the inside probability by.
      */
      def setIPProb(x:Double)
        { ipSetYet = true; ip = log(x) }
      /**
      * Easily increment the inside probability by providing a probability not in
      * log-space.
      * @param x The amount to increment the inside probability by.
      */
      def incIPProb(x:Double) {
        if(! ipSetYet)
          setIPProb(x)
        else
          ip = log(x + exp(ip))
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
      * @param score The log-space probability of the span
      */
      def newExpansion(left:String,right:String,s:Int,split:Int,e:Int,score:Double) {
        if(end == 0) {
          start = s
          end = e
          backMatcher = new Array[Stack[(String,String)]](length)
          List.range(0,length).foreach(backMatcher(_) = new Stack[(String,String)])
        }

        backMatcher(split - start) += (left,right)
        incIPScore( score )
      }

      /**
      * Return an entry to its initial state.
      */
      def reset = {
        ip = 0.0
        op = 0.0
        ipSetYet = false
        opSetYet = false
        List.range(0,length).foreach(backMatcher(_) = 
          new Stack[(String,String)])
        //backMatchR = Nil
      }
      
      /**
      * Make a readable stringification for Entry.
      * @return A readable stringification for Entry.
      */
      override def toString = 
        "ip: "+inProb+"; op: "+outProb;
    }

    /**
    * Binary-branching non-terminal entries.
    * @param label The label of the entry.
    */
    case class SynEntry(label:String)
      extends Entry(label) 

    /**
    * Unary-branching terminal entries.
    * @param label The prt of spedch of the entry.
    */
    case class LexEntry(label:String)
      extends Entry(label)


    /**
    * This stores intermediate counts of binary-branching nodes for this sentence.
    */
    val f_i = new HashMap[
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
                   override def default(right:String) = log(0)
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
      override def default(key:(Int,String,String)) = log(0)
    }

    /**
    * This stores intermediate counts of non-terminal nodes for this sentence.
    */
    val h_i = new HashMap[(Int,Int,String), Double] {
      override def default(key:(Int,Int,String)) = log(0)
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
                  k + ":" + String.format("%1.3fE-5",
                  double2Double(j(k).inProb*100000)) +
                  "," + String.format("%1.3fE-5",
                  double2Double(j(k).outProb*100000))
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
    *  Resize the chart.
    *  @param The new size for the chart
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
    def synFill( start:Int, end:Int) {
      import Math._
      List.range( start+1, end).foreach{ k =>
        chart(start)(k).keys.foreach{ left =>
          chart(k)(end).keys.foreach{ right =>
            g.phrExps (left)(right) .foreach{ phrase =>
              val thisScore =  log( phrase._2 ) + 
                              chart(start)(k)(left).ip +
                              chart(k)(end)(right).ip
                chart(start)(end)(phrase._1).newExpansion(
                  left, right,
                  start,k,end,
                  thisScore
                )
            }
          }
        }
      }
    }

    /**
    * Used in the CYK parser. Fills in the parts-of-speech of a word.
    * @param w The word.
    * @param index The index of the word.
    */
    def lexFill(w:String,index:Int) {
      g.lexExps(w).foreach{ pos =>
        val l = new LexEntry(pos._1)
        l.newExpansion(
          w,w,
          index,index,index+1,
          log(pos._2)
        )
        chart(index)(index+1) += Pair( pos._1, l)
      }
    }

     
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
      List.range(1,s.size+1).foreach{ j =>
        lexFill( s(j-1), j-1)
        
        List.range(0,j-1).reverse.foreach{ i =>
          synFill(i, j)
        }
      }
      chartDescent( computeOPWithEstimates )
      chart
    }

    
    /**
    * @return The inside probability of the sentence in log-space.
    */
    def stringScore = chart(0)(size-1)("S").ip

    /**
    * @return The inside probability of the sentence not in log-space.
    */
    def stringProb = exp(chart(0)(size-1)("S").ip)


    /**
    * Keeps track of which words we have visited when computing outside
    * probabilities.
    */
    val visitedLex = new HashSet[(Int,Int,String,String)]

    /**
    * Compute the outside probability for one entry and gather estimated counts
    * based on the entry. Assumes that all referenced values are already
    * computed.
    * @param ent The entry to be scored and counted.
    */
    def computeOPWithEstimates(ent:Entry) {
      import Math._

      val h_Summand = ent.op + ent.ip - stringScore
      val h_Key = Tuple3(ent.start,ent.end,ent.l)
      h_i( h_Key ) = /*log( exp( h_i(h_Key) ) + exp(*/ h_Summand/*) )*/

      /*
      if( ent.l == "V" )
        println( "h_Summand for N: " + h_Summand + (ent.op , ent.ip ,
        stringScore ).toString)
      */

      /*
      val lefts = ent.backMatchL
      val rights = ent.backMatchR
      */

      val f_toAdd = new HashMap[
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
                     override def default(right:String) = log(0)
                  }
                )
                this(left)
              }
            }
          )
          this(lhs)
        }
      }
      //f_toAdd.clear


      List.range(0,ent.backMatcher.size - 1).foreach{ split =>
        ent.backMatcher(split).foreach{ matches =>
          val left = matches._1
          val right = matches._2
          val ruleScore =
            log( g.phrases (ent.l) (left) (right) )

          val splitPoint = split + ent.start

          //println(">"+Tuple2(ent.start,split) )
          val leftEnt = chart (ent.start) (splitPoint) (left)
          val rightEnt = chart (splitPoint) (ent.end) (right)

          val toAddLeft = ent.op + ruleScore + rightEnt.ip
          leftEnt.incOPScore( toAddLeft )

          val toAddRight = ent.op + ruleScore + leftEnt.ip
          rightEnt.incOPScore( toAddRight )

          leftEnt match{
            case LexEntry(_) => {
              //println( Tuple2(leftEnt.start,leftEnt.end))
              val g_Summand = leftEnt.ip + leftEnt.op - stringScore

              val g_Key = (leftEnt.start, leftEnt.l, leftEnt.backMatcher(0)(0)._1)
              g_i( g_Key ) = /*log( exp( g_i(g_Key) ) + exp(*/g_Summand/*) )*/

              val h_Summand = leftEnt.ip + leftEnt.op - stringScore
              val h_Key = Tuple3( leftEnt.start, leftEnt.end, leftEnt.l)
              h_i( h_Key ) = /*log( exp( h_i(h_Key) ) + exp(*/h_Summand/*) )*/
            }
            case _ =>
          }
          rightEnt match {
            case LexEntry(_) => {
              //println( Tuple2(rightEnt.start,rightEnt.end))
              val g_Summand = rightEnt.ip + rightEnt.op - stringScore

              val g_Key = (rightEnt.start, rightEnt.l, rightEnt.backMatcher(0)(0)._1)
              g_i( g_Key ) = /*log( exp( g_i( g_Key) ) + exp (*/g_Summand/*) )*/

              val h_Summand = leftEnt.ip + leftEnt.op - stringScore
              val h_Key = Tuple3( rightEnt.start, rightEnt.end, rightEnt.l)
              h_i( h_Key ) = /*log( exp( h_i(h_Key) ) + exp(*/h_Summand/*) )*/
            }
            case _ =>
          }

          val f_Summand =
            ent.op + ruleScore + leftEnt.ip + rightEnt.ip //- stringScore
          f_toAdd (ent.l)(left)(right) =
            log( exp( f_toAdd(ent.l)(left)(right) ) + exp( f_Summand ) )

          /*
          if(ent.l == "V")
            println( (ent.op , ruleScore , leftEnt.ip , rightEnt.ip ,
            stringScore ))
          */

        }



      }

      //println(f_toAdd.keySet)
      f_toAdd.keys.foreach( lhs =>
        f_toAdd (lhs) .keys.foreach( left =>
          f_toAdd (lhs)(left) .keys.foreach{ right =>
            f_i (lhs)(left)(right) =
              log(
                exp(
                  f_i (lhs)(left)(right)
                ) +
                exp(
                  f_toAdd (lhs)(left)(right) -
                  stringScore
                )
              )
              /*
              if(lhs == "V")
                println( "f_toAdd for N: " + f_toAdd (lhs)(left)(right) )
              */
          }
        )
      )
    }


    /**
    * Compute the outside probability for one entry. Assumes that all referenced
    * values are already computed.
    * @param ent The entry to be scored.
    */
    def computeOP(ent:Entry) {
      import Math._

      //val lefts = ent.backMatchL
      //val rights = ent.backMatchR

      List.range(0,ent.backMatcher.size - 1).foreach{ split =>
          ent.backMatcher(split).foreach{ matches =>
            val splitPoint  = split + ent.start

            val left = matches._1
            val right = matches._2
            val ruleScore = log( g.phrases (ent.l)(left)(right) )
            
            val leftEnt = chart( ent.start )( splitPoint )( left )
            val rightEnt = chart( splitPoint )( ent.end )( right )

            val toAddLeft = ent.op + ruleScore + rightEnt.ip
            leftEnt.incOPScore( toAddLeft )

            val toAddRight = ent.op + ruleScore + leftEnt.ip
            rightEnt.incOPScore( toAddRight )
        }
      }
    }

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
        //println("here it is: " + toCompute(0, size - 1) + " " + toCompute)

        while( !toCompute.isEmpty) {
          //println(toCompute.size)
          val(start,end) = 
            toCompute.keys.foldLeft[(Int,Int)](0,0)( (a,b) =>
              if( (a._2 - a._1) > (b._2 - b._1) ) a else b
          )
          val labels = toCompute( (start,end) )
          //println(Tuple2(start,end))
          //println(toCompute.keySet)
          toCompute -= Tuple2(start,end)
          //println( Tuple2(start,end))
          //println(toCompute.keySet)

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
                  
                  //print("\t\t")
                  //println(toCompute.keySet)
                }
              }

          }
        }

      }
    }

    /**
    * Use this as an actor
    */
    def act() {
      loop {
        react {
          case s:String => {  // If we get a sentece, then parse it and send the
                              // counts back
            f_i.clear
            g_i.clear
            h_i.clear

            val words = s.split(' ')
            //if( size != words.size + 1)
              resize(words.size + 1)
            /*
            else
              clear
            */
            populateChart(words)
            println("Parser " + id + " parsed the sentence: \"" + s + "\"")
            sender ! (id,stringScore,f_i,g_i,h_i)
          }
          case Stop => {      // If we get the stop signal, then shut down.
            println("Parser " + id + " stopping")
            exit()
          }
        }
      }
    }
  } //end ShakesParser

  /**
  * Run EM until convergence (change in log probability of a corpus after an
  * iteration is less than some tolerance)
  * @param toParse The sentences to parse.
  * @param g The (initial) grammar to use while parsing.
  * @param numParsers The number of parsers to use.
  * @param tolerance The largest change in log probability we are willing to
  * accept.
  * @param gramFilePrefix The prefix of the path to the file that grammars are
  * written to.
  */
  class ShakesConvergenceManager
  (toParse:List[String], g:ShakesPCNF, numParsers:Int, tolerance:Double,
  gramFilePrefix:String) {
    import Math._

    /**
    * The stop condition function.
    * @param n Ignored, but should be provided. I know there's a more elegant
    * way to handle this but I'm not going to bother just now.
    * @param deltaLogProb The change in probability of the corpus.
    * @return Whether the change in log probability is small enough to stop.
    */
    def underTolerance(n:Int,deltaLogProb:Double) =
      abs(deltaLogProb) <= tolerance

    val manager = new ShakesParserManager( toParse, g, numParsers,
    gramFilePrefix) ( underTolerance )

    manager.start
  }

  /**
  * Run EM a specific number of iterations.
  * @param toParse The sentences to parse.
  * @param g The (initial) grammar to use while parsing.
  * @param numParsers The number of parsers to use.
  * @param numIterations The number of iterations we would like to run for.
  * @param gramFilePrefix The prefix of the path to the file that grammars are
  * written to.
  */
  class ShakesIterationManager
  (toParse:List[String], g:ShakesPCNF, numParsers:Int, numIterations:Int,
  gramFilePrefix:String) {

    /**
    * The stop condition function
    * @param iterationNum The number of iterations we have run
    * @param n Ignored, but should be provided. I know there's a more elegant
    * way to handle this but I'm not going to bother just now.
    * @return Whether we have run enough iterations to stop.
    */
    def iterationLimit(iterationNum:Int,n:Double) =
      iterationNum >= numIterations

    val manager = new ShakesParserManager( toParse, g, numParsers,
    gramFilePrefix) (iterationLimit )

    manager.start
  }

  /**
  * Starts up a bunch of parsers and farms out the sentences to them for an
  * iteration, updates the grammar, and repeats until the stopping condition is
  * reached.
  * @param toParse The sentences to parse.
  * @param g The (initial) grammar to use while parsing.
  * @param numParsers The number of parsers to use.
  * @param numIterations The number of iterations we would like to run for.
  * @param gramFilePrefix The prefix of the path to the file that grammars are
  * @param stoppingCondtion Are we done yet?
  */
  class ShakesParserManager
  (toParse:List[String], g:ShakesPCNF, numParsers:Int, gramFilePrefix:String)
  ( stoppingCondition: ((Int,Double) => Boolean ) )
  extends Actor {
    import collection.mutable.{HashMap,Stack}
    import Math._

    var g1 = g
    var g2 = g1.countlessCopy

    val stackToParse = new Stack[String]


    def act() {

      var iterationNum = 0
      var deltaLogProb = 1.0
      var lastCorpusLogProb = 0.0
      var corpusLogProb = 0.0

      while( ! stoppingCondition(iterationNum,deltaLogProb) ) {
        import java.io._
        stackToParse ++= toParse

        /**
        * Let's make as many parsers as we are asked to.
        */
        val parsers:Array[ShakesParser] = Array.fromFunction(
          new ShakesParser(_,g1)
        ) (numParsers)

        parsers.foreach( _.start )

        println("Beginning to parse...\n\n")
        List.range(0,parsers.size).foreach{ id =>
          println( "Sending sentence number " + id + " to parser " + id )
          parsers(id) ! stackToParse.pop
        }

        var sentenceNumber = parsers.size
        var numFinishedParsers = 0
        while( numFinishedParsers < numParsers ) {
          receive {
            case (
                    id:Int,
                    stringScore:Double,
                    f_i:HashMap[String,HashMap[String,HashMap[String,Double]]],
                    g_i:HashMap[(Int,String,String),Double],
                    h_i:HashMap[(Int,Int,String),Double]
            ) => {

              corpusLogProb = corpusLogProb + stringScore

              f_i.keys.foreach( lhs =>
                f_i (lhs) .keys.foreach( left =>
                  f_i (lhs)(left) .keys.foreach{ right =>
                    g2.f (lhs)(left)(right) =
                      log(
                        exp( g2.f (lhs)(left)(right) ) +
                        exp( f_i (lhs)(left)(right) )
                      )
                  }
                )
              )
              g_i.keys.foreach{ k =>
                  val index = k._1
                  val pos = k._2
                  val word = k._3
                  g2.g( (pos,word) ) = 
                    log( 
                      exp( g2.g( (pos,word) ) ) +
                      exp( g_i( k ) ) 
                    )
              }
              h_i.keys.foreach{ k =>
                  val start = k._1
                  val end = k._2
                  val label = k._3
                  g2.h(label) = log( exp( g2.h(label) ) + exp( h_i(k)) )
              }
              // Fill g2 with the reestimated counts for the rules.
              g2.reestimateRules


              sentenceNumber = sentenceNumber + 1

              if( stackToParse.isEmpty) {
                numFinishedParsers = numFinishedParsers + 1
              } else {
                println(
                  "Starting sentence number " + sentenceNumber  + " with parser " +
                  id
                )
                parsers(id) ! stackToParse.pop
              }

            }
          }
        }

        // Out of sentences
        parsers.foreach( _ ! Stop )


        g1 = g2
        g2 = g1.countlessCopy

        deltaLogProb = (lastCorpusLogProb - corpusLogProb)/abs(corpusLogProb)


        // Write grammar from this iteration to file. How do you get a value out
        // of an actor without sending it as a message to another actor?
        val bw = new BufferedWriter(new
        FileWriter(gramFilePrefix+"Iter"+iterationNum,true));
        bw.write(
          "Corpus probability: " +
          exp(corpusLogProb) +
          "\nDelta LogProb: " +
          deltaLogProb +
          "\n\n" +
          g1.toString
        );
        bw.close();

        lastCorpusLogProb = corpusLogProb
        corpusLogProb = 0.0

        iterationNum = iterationNum + 1
      }
    }
  }
}

