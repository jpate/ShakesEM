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

package ShakesEM.managers

import ShakesEM.grammars.ShakesPCNF
import ShakesEM.parsers._
import ShakesEM.types.messages._
import ShakesEM.types.expansions._
import ShakesEM.types.corpora._
import scala.actors.Actor
import scala.actors.Actor._
import collection.mutable.{HashMap => MHashMap, HashSet => MHashSet}

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

  def useGrammar( trainedG:ShakesPCNF, curIterCount:Int ):Unit
  def finalCleanup( trainedGram:ShakesPCNF ):Unit

  def mapPartialCounts( count:Double ):Double

  val deadHosts = new MHashSet[RemoteParserID]

  val trainingCorpus:ShakesTrainCorpus
  var quietude:Int

  val timeout:Int

  def act() {
    var iterationNum = 0
    var deltaLogProb = 1.0
    var lastCorpusLogProb = 0.0
    var corpusLogProb = 0.0

    val totalTermCount = trainingCorpus.toList.foldLeft(0)( (a,b) => a + b.size )

    while( ! stoppingCondition( iterationNum, deltaLogProb ) ) {
      val localParsers = localParserConstructor( g1 )
      val remoteParsers = remoteParserConstructor( g1 )

      var thisIterTrain = trainingCorpus.toList


      println("Beginning to parse iteration " + iterationNum + "...\n\n")

      var sentenceNumber = 0
      var numFinishedParsers = 0

      val maxTerminalsPerPackageLocal = 100
      val maxTerminalsPerPackage = round(
        totalTermCount /
        (remoteParsers.size + 4*localParsers.size - deadHosts.size )
      )
      println( maxTerminalsPerPackage +
        " terminals per package for this iteration")

      println( "Distributing to remote parsers" )
      (0 to (remoteParsers.size-1)) filter ( index =>  
        ! (deadHosts.contains(RemoteParserID( index )))
        ) foreach{ id =>
        println( "Checking parser " + RemoteParserID(id))
        //remoteParsers(id) !?(timeout, StillAlive) match {
        //  case Some(StillAlive) => {
            var prefixLength = 0
            val prefix = thisIterTrain.takeWhile( nextSent =>
              {
                prefixLength += nextSent.size
                prefixLength <= maxTerminalsPerPackage
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
          //}
        //  case None => { 
        //    deadHosts += RemoteParserID(id)
        //    println( RemoteParserID(id) + " timed out; " + deadHosts.size +
        //    " dead parsers")
        //  }
        //  case what:Any => println("Got back " + what +
        //    " when trying to send out the first batch of sentences")
        //}
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
              g2.f( NTExpansion(lhs, left, right) ) +=
                f_i (summandKey) / scaledStringProb
            }
          }

          case GResult(g_i,scaledStringProb) => {
            g_i.keysIterator.foreach{ summandKey =>
              val G_Key( _, pos, word ) = summandKey
              g2.g( TermExpansion(pos,word) ) += 
                g_i( summandKey ) / scaledStringProb
            }
          }

          case HResult(h_i,scaledStringProb) => {
            h_i.keysIterator.foreach{ summandKey =>
              val H_Key( _ , _, label) = summandKey
              g2.h(label) +=
                h_i(summandKey) / scaledStringProb
            }
          }


          case FSums(f_Reply) => {
            f_Reply.keysIterator.foreach{ replyKey =>
              g2.f( replyKey ) += f_Reply( replyKey )
            }
          }

          case GSums(g_Reply) =>  {
            g_Reply.keysIterator.foreach{ replyKey =>
              g2.g( replyKey ) += g_Reply( replyKey )
            }
          }
          case HSums(h_Reply) => {
            h_Reply.keysIterator.foreach{ replyKey =>
              g2.h( replyKey ) += h_Reply( replyKey )
            }
          }

          case StringProbResult(scaledStringProb,scaledBy) =>
            corpusLogProb = corpusLogProb + log( scaledStringProb ) -
              log( scaledBy )

          case RemoteParserID(id:Int) => {
                if( thisIterTrain.size > 0 ) {
                  var prefixLength = 0
                  val prefix = thisIterTrain.takeWhile( nextSent =>
                    {
                      prefixLength += nextSent.size
                      prefixLength <= maxTerminalsPerPackage
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
                  println( RemoteParserID(id) + " stopping")
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
                  " sentences to parser " + LocalParserID(id) +
                  ". Up to sentence number " +
                  sentenceNumber )
              }


              reply( prefix )

            
            } else {
              numFinishedParsers += 1
              println( LocalParserID(id) + " stopping" )
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


            corpusLogProb = corpusLogProb + log( scaledStringProb ) -
              log( scaledBy )


            f_i.keysIterator.foreach{ summandKey =>
              val F_Key( _,_,lhs,left,right ) = summandKey
              g2.f( NTExpansion(lhs,left,right) ) +=
                    f_i (summandKey) /
                      scaledStringProb
            }

            g_i.keysIterator.foreach{ summandKey =>
              val G_Key( _, pos, word ) = summandKey
              g2.g( TermExpansion(pos,word) ) += 
                g_i( summandKey ) / scaledStringProb
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

          }

          case what:Any =>
            println("ShakesParserManager got something else: " + what +
            " from " + sender)

        }
      }

      g2.reestimateRules( mapPartialCounts )

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

    }
    finalCleanup(g1)

    val parsersToKill = remoteParserConstructor( new ShakesPCNF )

    parsersToKill foreach ( _ ! Stop )

    exit()
  }
}


trait EvaluatingManager {
  val testSentences:List[String]

  def finalCleanup(trainedGram:ShakesPCNF) = {
    VitActor ! Evaluation("Convergence",trainedGram)

    testSentences.foreach( sent => VitActor ! sent )
    
    VitActor ! Stop
  }

  object VitActor extends ViterbiDefinitions with Actor {
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

trait StandardEMManager {
  def mapPartialCounts( x:Double ) = x
}

/**
* Use this for a mean-field approximation to the Infinite PCFG
*/
trait MeanFieldInfinitePCFGManager {
  import math._
  //def mapPartialCounts( x:Double)  = digamma

  /**
  * This is a translation of code obtained from Percy Liang (which apparently
  * was ``stolen from Radford Neal's fbm package,'' into scala.
  *
  * @param input the number which you want to take exp-digamma of
  * @return digamma of input
  *
  */
  def mapPartialCounts( input:Double ) = 
    exp( 
      if( input <= 0 ) {
        Double.NegativeInfinity
      } else {
        var r = 0D
        var x = input
        while( x <= 5 ) {
          r -= 1/x
          x += 1
        }
        val f = 1/(x*x)
        val t = f*(-1/12.0 + f*(1/120.0 + f*(-1/252.0 + f*(1/240.0 + f*(-1/132.0
            + f*(691/32760.0 + f*(-1/12.0 + f*3617/8160.0)))))));
        r + log(x) - 0.5/x + t;
      }
    )
}


trait EveryOneHundred {
  var quietude = 100
}
trait EveryFive {
  var quietude = 5
}



