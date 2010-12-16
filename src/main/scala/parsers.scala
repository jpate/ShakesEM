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
* @version 0.21a
* @author John K Pate
*/

package ShakesEM.parsers

import ShakesEM.grammars.ShakesPCNF
import ShakesEM.types.expansions._
import ShakesEM.types.messages._
import ShakesEM.types.strings._
import scala.actors.Actor
import scala.actors.Actor._
import collection.immutable.{HashMap => IHashMap,HashSet => IHashSet}
import collection.mutable.{HashMap => MHashMap, HashSet => MHashSet}


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
    * @return The size of the chart.
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

      toCompute(0, size - 1) += "S"

      while( !toCompute.isEmpty) {
        val(start,end) = 
          toCompute.keysIterator.foldLeft[(Int,Int)](0,0)( (a,b) =>
            if( (a._2 - a._1) > (b._2 - b._1) ) a else b
        )
        val labels = toCompute( (start,end) )
        toCompute -= Tuple2(start,end)

        labels.foreach{ l =>
            val rootCell = chart(start)(end)(l)
            p( chart(start)(end)(l) )

            (1 to (rootCell.backMatcher.length-1)).foreach{ split =>
              val splitPoint = rootCell.start + split
              rootCell.backMatcher(split).foreach{ children =>
                children match {
                  case RightHandSide( left, right ) => {
                    if( splitPoint - rootCell.start > 1 ) {
                      toCompute( (rootCell.start, splitPoint)) += left
                      }

                    if( rootCell.end - splitPoint > 1) {
                      toCompute ( (splitPoint, rootCell.end) ) += right
                    }

                  }
                  case _ =>
                }
              }
            }
        }
      }
    }
  }
}//end ShakesParser


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

            val ruleProb =  g.phrases(NTExpansion(ent.l,left,right) )
            
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
    import math._

    h_i += Pair( H_Key(ent.start,ent.end,ent.l), ent.op * ent.ip )

    val f_toAdd = new MHashMap[ (String,String), Double] {
      override def default(key:(String,String)) = 0D
    }


    (1 to (ent.backMatcher.size-1) ) foreach{ split =>
      ent.backMatcher(split).foreach{ matches =>
        val RightHandSide( left, right ) = matches
        val ruleProb = g.phrases(NTExpansion(ent.l,left,right) )

        val splitPoint = split + ent.start

        val leftEnt = chart (ent.start) (splitPoint) (left)
        val rightEnt = chart (splitPoint) (ent.end) (right)

        leftEnt.incOPProb( ent.op * ruleProb * rightEnt.ip )

        rightEnt.incOPProb( ent.op * ruleProb * leftEnt.ip )

        if(leftEnt.end - leftEnt.start == 1) {
          val RightHandSide( word, _ ) = leftEnt.backMatcher(0)(0)
          g_i += Pair(
            G_Key(leftEnt.start, leftEnt.l, word ),
            leftEnt.ip * leftEnt.op
          )

          h_i += Pair(
            H_Key( leftEnt.start, leftEnt.end, leftEnt.l),
            leftEnt.ip * leftEnt.op
          )
        }

        if(rightEnt.end - rightEnt.start == 1) {
          val RightHandSide( word, _ ) = rightEnt.backMatcher(0)(0)
          g_i += Pair(
            G_Key(rightEnt.start, rightEnt.l, word ),
            rightEnt.ip * rightEnt.op
          )

          h_i += Pair(
            H_Key( rightEnt.start, rightEnt.end, rightEnt.l),
            leftEnt.ip * leftEnt.op
          )
        }

        val f_Summand =
          ent.op * ruleProb * leftEnt.ip * rightEnt.ip
        f_toAdd( (left,right) ) =
            f_toAdd( (left,right) )+ f_Summand
      }
    }

    f_toAdd .keysIterator.foreach{ k =>
      val(left,right) = k
      f_i( F_Key(ent.start,ent.end,ent.l,left,right) ) +=
        f_toAdd(k)
    }
  }

  /**
  * This stores intermediate counts of binary-branching nodes for this sentence.
  */
  @serializable val f_i = new MHashMap[F_Key,Double] {
    override def default( key:F_Key ) = 0D
  }
  /**
  * This stores intermediate counts of unary-branching nodes for this sentence.
  */
  //var g_i = new IHashMap[(Int,String,String), Double] {
  @serializable val g_i = new MHashMap[G_Key, Double] {
    override def default( key:G_Key ) = 0D
  }

  /**
  * This stores intermediate counts of non-terminal nodes for this sentence.
  */
  @serializable val h_i = new MHashMap[H_Key, Double] {
    override def default( key:H_Key ) = 0D
  }
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
    g.lexExps(w).foreach{ exp =>
      val NTRevParent( pos, prob ) = exp
      val l = new LexEntry(pos)
      l.newExpansion(
        w,w,
        index,index,index+1,
        wordScale * prob
      )
      chart(index)(index+1) += Pair( pos, l)
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
          g.phrExps( NTRevChild(left, right) ).foreach{ parent =>
            val NTRevParent( lhs, expProb) = parent
            val thisprob =  expProb * 
                            chart(start)(k)(left).ip *
                            chart(k)(end)(right).ip
              chart(start)(end)(lhs).newExpansion(
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

    chartDescent( computeOPWithEstimates )
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
          if( stringCount % quietude < 10 )
            println( "Received " + numSentences + " sentences with " +
              numTerminals + " total terminals")

          val f_Reply = new MHashMap[NTExpansion,Double] {
            override def default(exp:NTExpansion) = 0D
          }
          val g_Reply = new MHashMap[TermExpansion,Double] {
            override def default(key:TermExpansion) = 0D
          }
          val h_Reply = new MHashMap[String,Double] {
            override def default(key:String) = 0D
          }




          itemList foreach ( item => 
            item match {
              case StringToParse( s:String ) => {

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

                stringCount += 1


                f_i.keysIterator.foreach{ summandKey =>
                  val F_Key( _,_,lhs,left,right ) = summandKey
                  f_Reply( NTExpansion(lhs, left, right) ) +=
                    f_i (summandKey) / scaledStringProb
                }


                g_i.keysIterator.foreach{ summandKey =>
                  val G_Key( _, pos, word ) = summandKey
                  g_Reply( TermExpansion(pos,word) ) += 
                    g_i( summandKey ) / scaledStringProb
                }

                h_i.keysIterator.foreach{ summandKey =>
                  val H_Key( _ , _, label) = summandKey
                  h_Reply(label) +=
                      h_i(summandKey) / scaledStringProb
                }

                f_i.clear
                g_i.clear
                h_i.clear

                sender ! StringProbResult( scaledStringProb, scaledBy )
              }

              case BracketedToParse( s:String, b:MHashSet[Bracketing] ) => {
                f_i.clear
                g_i.clear
                h_i.clear
                bracketing = b


                val words = s.split(' ')
                resize(words.size + 1)


                bracketedChartPopulation(words)

                if( !root.contains("S") ) {
                  println("WARNING: SENTENCE DID NOT PARSE")
                  println( s )
                }

                val scaledBy = pow( wordScale , size - 1 )

                stringCount += 1


                f_i.keysIterator.foreach{ summandKey =>
                  val F_Key( _,_,lhs,left,right ) = summandKey
                  f_Reply( NTExpansion(lhs, left, right) ) +=
                    f_i (summandKey) / scaledStringProb
                }


                g_i.keysIterator.foreach{ summandKey =>
                  val G_Key( _, pos, word ) = summandKey
                  g_Reply( TermExpansion(pos,word) ) += 
                    g_i( summandKey ) / scaledStringProb
                }

                h_i.keysIterator.foreach{ summandKey =>
                  val H_Key( _ , _, label) = summandKey
                  h_Reply(label) +=
                      h_i(summandKey) / scaledStringProb
                }
                reply( StringProbResult( scaledStringProb, scaledBy ) )

                f_i.clear
                g_i.clear
                h_i.clear
              }
            }
          )

          println("Sending back results...")
          reply( FSums( f_Reply.toMap ) )
          reply( GSums( g_Reply.toMap ) )
          reply( HSums( h_Reply.toMap ) )

          f_i.clear
          g_i.clear
          h_i.clear

          println("\n")

          if( stringCount % quietude < 10 )
            println( parserID + " asking for more...")

          sender ! parserID
        }

        case StringToParse(s:String) => {
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

          if(stringCount >= quietude )
            stringCount = 0
          else
            stringCount += 1
    
          sender ! ParsingResult(parserID,scaledStringProb,f_i.toMap,g_i.toMap,h_i.toMap,scaledBy)
          sender ! parserID
        }

        case BracketedToParse(s:String,b:MHashSet[Bracketing]) => {  
          f_i.clear
          g_i.clear
          h_i.clear
          bracketing = b


          val words = s.split(' ')
          resize(words.size + 1)


          bracketedChartPopulation(words)

          if( !root.contains("S") ) {
            println("WARNING: SENTENCE DID NOT PARSE")
            println( s )
          }

          val scaledBy = pow( wordScale , size - 1 )

          stringCount += 1

          sender ! ParsingResult(parserID,scaledStringProb,f_i.toMap,g_i.toMap,h_i.toMap,scaledBy)

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
        }

        case RemoteParserID(id:Int) => {
          parserID = RemoteParserID(id)
          println( "Parser now known as " + parserID )
        }

        case what:Any => {
          println( parserID + "got something else: "  + what)
        }
      }
    }
  }
}

/**
* This provides chart filling definitions for viterbi parsing
*/
trait ViterbiDefinitions extends /*Actor with*/ LocalDefinitions with ShakesParser {
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
    val NTRevParent(mlePOS,mleProb:Double) =
      g.lexExps(w).reduceLeft{ (l1, l2) =>
        val NTRevParent(_,prob1) = l1
        val NTRevParent(_,prob2) = l2
        if( prob1 > prob2 )
          l1 else l2
    }
    
    val l = new LexEntry( mlePOS )
    l.newExpansion(
      w,w,
      index, index, index+1,
      wordScale * mleProb
    )
    chart(index)(index+1) += Pair( mlePOS, l)
  }
  def synFill( start:Int, end:Int) {
    start+1 to (end-1) foreach{ k =>
      chart(start)(k).keysIterator.foreach{ left =>
        chart(k)(end).keysIterator.foreach{ right =>
          g.phrExps( NTRevChild(left, right) ) . foreach{ parent =>
            val NTRevParent(lhs, expProb) = parent
            val thisprob =  expProb * 
                            chart(start)(k)(left).ip *
                            chart(k)(end)(right).ip
              chart(start)(end)(lhs).newVitExpansion(
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

  def parseAndPrint(s:String) {
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

        case s:String => parseAndPrint(s)
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
