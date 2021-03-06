package ShakesEM.runners

import ShakesEM.managers._
import ShakesEM.parsers._
import ShakesEM.grammars.ShakesPCNF
import ShakesEM.types.messages._
import ShakesEM.types.corpora._
import scala.actors.Actor
import scala.actors.Actor._

trait Heuristics {
  var wordScale = 10000
}

object viterbi {
  def main( args:Array[String] ) {
    import scala.io.Source._
    val gramFile = args(0)
    val lexFile = args(1)
    val stringsFile = args(2)

    val grammar = new ShakesPCNF
    grammar.readLexicon( lexFile )
    grammar.readGrammar( gramFile )

    object viterbiParser extends ViterbiDefinitions {
      var parserID:ParserID = LocalParserID(-1)
      var g = grammar
      var wordScale = 10000
    }

    val testSentences = io.Source.fromFile( stringsFile ).getLines().toList

    testSentences.foreach( viterbiParser parseAndPrint( _ ) )
  }
}

object trainAndEvaluateInfiniteBracketedMinIterAndConvergence {
  def main( args:Array[String] ) {
    import scala.io.Source._
    import math._

    val nonTermCount = args(0).toInt
    val termFile = args(1)
    val trainYieldSpec = args(2)
    val testStringsPath = args(3)
    val numLocalParsers = args(4).toInt
    val hostsPath = args(5)
    val minIter = args(6).toInt
    val tolerance = args(7).toDouble
    val randSeed = args(8).toInt
    val randomBase = args(9).toInt

    println("nonTermCount: " + nonTermCount )
    println("termFile: " + termFile )
    println("trainYieldSpec: " + trainYieldSpec )
    println("testStringsPath: " + testStringsPath )
    println("numLocalParsers: "+ numLocalParsers )
    println("hostsPath: "+ hostsPath )
    println("minIter: " + minIter )
    println("tolerance: " + tolerance )
    println("randSeed: " + randSeed )
    println("randomBase: " + randomBase )

    println("\n\n\n====\n\n\n")

    val initGram = new ShakesPCNF

    val poslist = fromFile(termFile).getLines().toList

    initGram.randomizeGrammar(nonTermCount,poslist,randSeed,randomBase)

    //initGram.readGrammar( gramFile )
    //initGram.readLexicon( lexFile )

    
    object manager extends Actor with ShakesParserManager with
    MeanFieldInfinitePCFGManager with EvaluatingManager with EveryOneHundred {
      import collection.mutable.ArrayBuffer
      import scala.actors.remote.Node
      import scala.actors.remote.RemoteActor._
      import scala.actors.AbstractActor
      
      val timeout = 10000

      var g1 = initGram
      var g2 = g1.countlessCopy


      val trainingCorpus = new BracketedCorpus
      trainingCorpus.readCorpus( trainYieldSpec )

      val testSentences = fromFile(testStringsPath).getLines().toList


      def stoppingCondition( numIter:Int, deltaLogProb:Double ) =
        numIter > minIter && abs(deltaLogProb) < tolerance

      def localParserConstructor( grammar:ShakesPCNF ) = {
        val someParsers = ((0 to (numLocalParsers-1)) map{ parserSpec:Int =>
          new Actor with EstimationParser with LocalDefinitions with
            Heuristics /*with EveryOneHundred*/ {
              var parserID:ParserID = LocalParserID(parserSpec)
              var g = grammar
              var quietude = 100
            }
        }).toList

        someParsers foreach( _.start )

        someParsers
      }

      val hostsList =
      fromFile(hostsPath).getLines().toList.map{_.split(' ')}

      def remoteParserConstructor( grammar:ShakesPCNF ) = {
        var someParsers = (hostsList map{ parserSpec =>
            val Array(ip,port) = parserSpec
            //println( "Establishing connection with " + ip +":"+
            //port +"... ")

            select( Node(ip, port.toInt), 'parser )
        }).toList

        (0 to (someParsers.size-1)) filter ( id =>
          !  deadHosts.contains(RemoteParserID(id))) foreach{ index:Int =>
          someParsers( index ) !?(timeout, StillAlive) match {
            case Some(StillAlive) => {
              println( "Connecting to " + RemoteParserID( index ) )
              someParsers( index ) ! RemoteParserID( index )
              someParsers( index ) ! grammar
            }
            case None => {
              deadHosts += RemoteParserID(index)
              println( RemoteParserID(index) + " timed out; " + deadHosts.size +
                " dead parsers")
            }
            case what:Any => println("Got back " + what +
              " in remoteParserConstructor")
          }
        }

        //someParsers foreach( _ ! RemoteParserID( parserSpec ) )
        //(0 to (someParsers.size - 1)) foreach ( index =>
        //  if( ! deadHosts.contains( RemoteParserID(index)) )
        //    someParsers foreach( _ ! grammar )
        //)

        someParsers
      }

      //def iterationCleanup( parsers:List[AbstractActor] ) = ()

    }
    manager.start
  }
}

object trainAndEvaluateBracketedMinIterAndConvergence {
  def main( args:Array[String] ) {
    import scala.io.Source._
    import math._

    val nonTermCount = args(0).toInt
    val termFile = args(1)
    val trainYieldSpec = args(2)
    val testStringsPath = args(3)
    val numLocalParsers = args(4).toInt
    val hostsPath = args(5)
    val minIter = args(6).toInt
    val tolerance = args(7).toDouble
    val randSeed = args(8).toInt
    val randomBase = args(9).toInt

    println("nonTermCount: " + nonTermCount )
    println("termFile: " + termFile )
    println("trainYieldSpec: " + trainYieldSpec )
    println("testStringsPath: " + testStringsPath )
    println("numLocalParsers: "+ numLocalParsers )
    println("hostsPath: "+ hostsPath )
    println("minIter: " + minIter )
    println("tolerance: " + tolerance )
    println("randSeed: " + randSeed )
    println("randomBase: " + randomBase )

    println("\n\n\n====\n\n\n")

    val initGram = new ShakesPCNF

    val poslist = fromFile(termFile).getLines().toList

    initGram.randomizeGrammar(nonTermCount,poslist,randSeed,randomBase)

    //initGram.readGrammar( gramFile )
    //initGram.readLexicon( lexFile )

    
    object manager extends Actor with ShakesParserManager with
    StandardEMManager with EvaluatingManager with EveryOneHundred {
      import collection.mutable.ArrayBuffer
      import scala.actors.remote.Node
      import scala.actors.remote.RemoteActor._
      import scala.actors.AbstractActor
      
      val timeout = 10000

      var g1 = initGram
      var g2 = g1.countlessCopy


      val trainingCorpus = new BracketedCorpus
      trainingCorpus.readCorpus( trainYieldSpec )

      val testSentences = fromFile(testStringsPath).getLines().toList


      def stoppingCondition( numIter:Int, deltaLogProb:Double ) =
        numIter > minIter && abs(deltaLogProb) < tolerance

      def localParserConstructor( grammar:ShakesPCNF ) = {
        val someParsers = ((0 to (numLocalParsers-1)) map{ parserSpec:Int =>
          new Actor with EstimationParser with LocalDefinitions with
            Heuristics /*with EveryOneHundred*/ {
              var parserID:ParserID = LocalParserID(parserSpec)
              var g = grammar
              var quietude = 100
            }
        }).toList

        someParsers foreach( _.start )

        someParsers
      }

      val hostsList =
      fromFile(hostsPath).getLines().toList.map{_.split(' ')}

      def remoteParserConstructor( grammar:ShakesPCNF ) = {
        var someParsers = (hostsList map{ parserSpec =>
            val Array(ip,port) = parserSpec
            //println( "Establishing connection with " + ip +":"+
            //port +"... ")

            select( Node(ip, port.toInt), 'parser )
        }).toList

        (0 to (someParsers.size-1)) filter ( id =>
          !  deadHosts.contains(RemoteParserID(id))) foreach{ index:Int =>
          someParsers( index ) !?(timeout, StillAlive) match {
            case Some(StillAlive) => {
              println( "Connecting to " + RemoteParserID( index ) )
              someParsers( index ) ! RemoteParserID( index )
              someParsers( index ) ! grammar
            }
            case None => {
              deadHosts += RemoteParserID(index)
              println( RemoteParserID(index) + " timed out; " + deadHosts.size +
                " dead parsers")
            }
            case what:Any => println("Got back " + what +
              " in remoteParserConstructor")
          }
        }

        //someParsers foreach( _ ! RemoteParserID( parserSpec ) )
        //(0 to (someParsers.size - 1)) foreach ( index =>
        //  if( ! deadHosts.contains( RemoteParserID(index)) )
        //    someParsers foreach( _ ! grammar )
        //)

        someParsers
      }

      //def iterationCleanup( parsers:List[AbstractActor] ) = ()

    }
    manager.start
  }
}

object trainReadAndEvaluateBracketedMinIterAndConvergence {
  def main( args:Array[String] ) {
    import scala.io.Source._
    import math._

    val gramFile = args(0)
    val lexFile = args(1)
    val trainYieldSpec = args(2)
    val testStringsPath = args(3)
    val numLocalParsers = args(4).toInt
    val hostsPath = args(5)
    val minIter = args(6).toInt
    val tolerance = args(7).toDouble

    println("gramFile: " + gramFile )
    println("lexFile: " + lexFile )
    println("trainYieldSpec: " + trainYieldSpec )
    println("testStringsPath: " + testStringsPath )
    println("numLocalParsers: "+ numLocalParsers )
    println("hostsPath: "+ hostsPath )
    println("minIter: " + minIter )
    println("tolerance: " + tolerance )

    println("\n\n\n====\n\n\n")

    val initGram = new ShakesPCNF

    //val poslist = fromFile(termFile).getLines().toList

    //initGram.randomizeGrammar(nonTermCount,poslist,randSeed,randomBase)

    initGram.readGrammar( gramFile )
    initGram.readLexicon( lexFile )

    object manager extends Actor with ShakesParserManager with
    StandardEMManager with EvaluatingManager with EveryOneHundred {
      import collection.mutable.ArrayBuffer
      import scala.actors.remote.Node
      import scala.actors.remote.RemoteActor._
      import scala.actors.AbstractActor
      
      val timeout = 10000

      var g1 = initGram
      var g2 = g1.countlessCopy


      val trainingCorpus = new BracketedCorpus
      trainingCorpus.readCorpus( trainYieldSpec )

      val testSentences = fromFile(testStringsPath).getLines().toList


      def stoppingCondition( numIter:Int, deltaLogProb:Double ) =
        numIter > minIter && abs(deltaLogProb) < tolerance

      def localParserConstructor( grammar:ShakesPCNF ) = {
        val someParsers = ((0 to (numLocalParsers-1)) map{ parserSpec:Int =>
          new Actor with EstimationParser with LocalDefinitions with
            Heuristics /*with EveryOneHundred*/ {
              var parserID:ParserID = LocalParserID(parserSpec)
              var g = grammar
              var quietude = 100
            }
        }).toList

        someParsers foreach( _.start )

        someParsers
      }

      val hostsList =
      fromFile(hostsPath).getLines().toList.map{_.split(' ')}

      def remoteParserConstructor( grammar:ShakesPCNF ) = {
        var someParsers = (hostsList map{ parserSpec =>
            val Array(ip,port) = parserSpec
            //println( "Establishing connection with " + ip +":"+
            //port +"... ")

            select( Node(ip, port.toInt), 'parser )
        }).toList

        (0 to (someParsers.size-1)) filter ( id =>
          !  deadHosts.contains(RemoteParserID(id))) foreach{ index:Int =>
          someParsers( index ) !?(timeout, StillAlive) match {
            case Some(StillAlive) => {
              println( "Connecting to " + RemoteParserID( index ) )
              someParsers( index ) ! RemoteParserID( index )
              someParsers( index ) ! grammar
            }
            case None => {
              deadHosts += RemoteParserID(index)
              println( RemoteParserID(index) + " timed out; " + deadHosts.size +
                " dead parsers")
            }
            case what:Any => println("Got back " + what +
              " in remoteParserConstructor")
          }
        }

        //someParsers foreach( _ ! RemoteParserID( parserSpec ) )
        //(0 to (someParsers.size - 1)) foreach ( index =>
        //  if( ! deadHosts.contains( RemoteParserID(index)) )
        //    someParsers foreach( _ ! grammar )
        //)

        someParsers
      }

      //def iterationCleanup( parsers:List[AbstractActor] ) = ()

    }
    manager.start
  }
}

object trainAndTimeVanillaMinIterAndConvergence {
  def main( args:Array[String] ) {
    import scala.io.Source._
    import math._

    val nonTermCount = args(0).toInt
    val termFile = args(1)
    val trainYieldSpec = args(2)
    val testStringsPath = args(3)
    val numLocalParsers = args(4).toInt
    val hostsPath = args(5)
    val minIter = args(6).toInt
    val tolerance = args(7).toDouble
    val randSeed = args(8).toInt
    val randomBase = args(9).toInt

    println("nonTermCount: " + nonTermCount )
    println("termFile: " + termFile )
    println("trainYieldSpec: " + trainYieldSpec )
    println("testStringsPath: " + testStringsPath )
    println("numLocalParsers: "+ numLocalParsers )
    println("hostsPath: "+ hostsPath )
    println("minIter: " + minIter )
    println("tolerance: " + tolerance )
    println("randSeed: " + randSeed )
    println("randomBase: " + randomBase )

    println("\n\n\n====\n\n\n")

    val initGram = new ShakesPCNF

    val poslist = fromFile(termFile).getLines().toList
    val hosts = fromFile(hostsPath).getLines().toList

    initGram.randomizeGrammar(nonTermCount,poslist,randSeed,randomBase)

    //initGram.readGrammar( gramFile )
    //initGram.readLexicon( lexFile )

    
    object manager extends Actor with ShakesParserManager with
    StandardEMManager with EveryOneHundred {
      import collection.mutable.ArrayBuffer
      import scala.actors.remote.Node
      import scala.actors.remote.RemoteActor._
      import scala.actors.AbstractActor

      var g1 = initGram
      var g2 = g1.countlessCopy
      val timeout = 10000

      val trainingCorpus = new StringsOnlyCorpus
      trainingCorpus.readCorpus( trainYieldSpec )

      val testSentences = fromFile(testStringsPath).getLines().toList

      val hostsList:List[Array[String]] =
      fromFile(hostsPath).getLines().toList.map{_.split(' ')}

      def useGrammar(trainedGram:ShakesPCNF, iterNum:Int) = {
        if( iterNum % 2 == 0) {
          println(trainedGram)
        }
      }
      def finalCleanup( trainedGram:ShakesPCNF ) = ()


      def stoppingCondition( numIter:Int, deltaLogProb:Double ) =
        numIter > minIter && abs(deltaLogProb) < tolerance

      def localParserConstructor( grammar:ShakesPCNF ) = {
        val someParsers = ((0 to (numLocalParsers-1)) map{ parserSpec:Int =>
          new Actor with EstimationParser with LocalDefinitions with
            Heuristics /*with EveryOneHundred*/ {
              var parserID:ParserID = LocalParserID(parserSpec)
              var g = grammar
              var quietude = 100
            }
        }).toList

        someParsers foreach( _.start )

        someParsers
      }

      def remoteParserConstructor( grammar:ShakesPCNF ) = {
        var someParsers = (hostsList map{ parserSpec =>
            val Array(ip,port) = parserSpec
            //println( "Establishing connection with " + ip +":"+
            //port +"... ")

            select( Node(ip, port.toInt), 'parser )
        }).toList

        (0 to (someParsers.size-1)) filter ( id =>
          !  deadHosts.contains(RemoteParserID(id))) foreach{ index:Int =>
          someParsers( index ) !?(timeout, StillAlive) match {
            case Some(StillAlive) => {
              println( "Connecting to " + RemoteParserID( index ) )
              someParsers( index ) ! RemoteParserID( index )
              someParsers( index ) ! grammar
            }
            case None => {
              deadHosts += RemoteParserID(index)
              println( RemoteParserID(index) + " timed out; " + deadHosts.size +
                " dead parsers")
            }
            case what:Any => println("Got back " + what +
              " when trying to send out the first batch of sentences")
          }
        }

        //someParsers foreach( _ ! RemoteParserID( parserSpec ) )
        //(0 to (someParsers.size - 1)) foreach ( index =>
        //  if( ! deadHosts.contains( RemoteParserID(index)) )
        //)

        someParsers
      }

      //def iterationCleanup( parsers:List[AbstractActor] ) = ()

    }
    manager.start

  }
}

object trainReadAndTimeVanillaMinIterAndConvergence {
  def main( args:Array[String] ) {
    import scala.io.Source._
    import math._

    val gramFile = args(0)
    val lexFile = args(1)
    val trainYieldSpec = args(2)
    val testStringsPath = args(3)
    val numLocalParsers = args(4).toInt
    val hostsPath = args(5)
    val minIter = args(6).toInt
    val tolerance = args(7).toDouble

    println("gramFile: " + gramFile )
    println("lexFile: " + lexFile )
    println("trainYieldSpec: " + trainYieldSpec )
    println("testStringsPath: " + testStringsPath )
    println("numLocalParsers: "+ numLocalParsers )
    println("hostsPath: "+ hostsPath )
    println("minIter: " + minIter )
    println("tolerance: " + tolerance )

    println("\n\n\n====\n\n\n")

    val initGram = new ShakesPCNF

    //val poslist = fromFile(termFile).getLines().toList
    val hosts = fromFile(hostsPath).getLines().toList

    //initGram.randomizeGrammar(nonTermCount,poslist,16,0)

    initGram.readGrammar( gramFile )
    initGram.readLexicon( lexFile )

    
    object manager extends Actor with StandardEMManager with
    ShakesParserManager with EveryOneHundred {
      import collection.mutable.ArrayBuffer
      import scala.actors.remote.Node
      import scala.actors.remote.RemoteActor._
      import scala.actors.AbstractActor

      var g1 = initGram
      var g2 = g1.countlessCopy
      val timeout = 10000

      val trainingCorpus = new StringsOnlyCorpus
      trainingCorpus.readCorpus( trainYieldSpec )

      val testSentences = fromFile(testStringsPath).getLines().toList

      val hostsList:List[Array[String]] =
      fromFile(hostsPath).getLines().toList.map{_.split(' ')}

      def useGrammar(trainedGram:ShakesPCNF, iterNum:Int) = {
        println(trainedGram)
      }
      def finalCleanup( trainedGram:ShakesPCNF ) = ()


      def stoppingCondition( numIter:Int, deltaLogProb:Double ) =
        numIter > minIter && abs(deltaLogProb) < tolerance

      def localParserConstructor( grammar:ShakesPCNF ) = {
        val someParsers = ((0 to (numLocalParsers-1)) map{ parserSpec:Int =>
          new Actor with EstimationParser with LocalDefinitions with
            Heuristics /*with EveryOneHundred*/ {
              var parserID:ParserID = LocalParserID(parserSpec)
              var g = grammar
              var quietude = 100
            }
        }).toList

        someParsers foreach( _.start )

        someParsers
      }

      def remoteParserConstructor( grammar:ShakesPCNF ) = {
        var someParsers = (hostsList map{ parserSpec =>
            val Array(ip,port) = parserSpec
            //println( "Establishing connection with " + ip +":"+
            //port +"... ")

            select( Node(ip, port.toInt), 'parser )
        }).toList

        (0 to (someParsers.size-1)) filter ( id =>
          !  deadHosts.contains(RemoteParserID(id))) foreach{ index:Int =>
          someParsers( index ) !?(timeout, StillAlive) match {
            case Some(StillAlive) => {
              println( "Connecting to " + RemoteParserID( index ) )
              someParsers( index ) ! RemoteParserID( index )
              someParsers( index ) ! grammar
            }
            case None => {
              deadHosts += RemoteParserID(index)
              println( RemoteParserID(index) + " timed out; " + deadHosts.size +
                " dead parsers")
            }
            case what:Any => println("Got back " + what +
              " when trying to send out the first batch of sentences")
          }
        }

        //someParsers foreach( _ ! RemoteParserID( parserSpec ) )
        //(0 to (someParsers.size - 1)) foreach ( index =>
        //  if( ! deadHosts.contains( RemoteParserID(index)) )
        //)

        someParsers
      }

      //def iterationCleanup( parsers:List[AbstractActor] ) = ()

    }
    manager.start

  }
}


object trainAndEvaluateInfiniteVanillaMinIterAndConvergence {
  def main( args:Array[String] ) {
    import scala.io.Source._
    import math._

    val nonTermCount = args(0).toInt
    val termFile = args(1)
    val trainYieldSpec = args(2)
    val testStringsPath = args(3)
    val numLocalParsers = args(4).toInt
    val hostsPath = args(5)
    val minIter = args(6).toInt
    val tolerance = args(7).toDouble
    val randSeed = args(8).toInt
    val randomBase = args(9).toInt

    println("nonTermCount: " + nonTermCount )
    println("termFile: " + termFile )
    println("trainYieldSpec: " + trainYieldSpec )
    println("testStringsPath: " + testStringsPath )
    println("numLocalParsers: "+ numLocalParsers )
    println("hostsPath: "+ hostsPath )
    println("minIter: " + minIter )
    println("tolerance: " + tolerance )
    println("randSeed: " + randSeed )
    println("randomBase: " + randomBase )

    println("\n\n\n====\n\n\n")

    val initGram = new ShakesPCNF

    val poslist = fromFile(termFile).getLines().toList
    val hosts = fromFile(hostsPath).getLines().toList

    initGram.randomizeGrammar(nonTermCount,poslist,randSeed,randomBase)

    //initGram.readGrammar( gramFile )
    //initGram.readLexicon( lexFile )

    
    object manager extends Actor with ShakesParserManager with
    MeanFieldInfinitePCFGManager with EvaluatingManager with EveryOneHundred {
      import collection.mutable.ArrayBuffer
      import scala.actors.remote.Node
      import scala.actors.remote.RemoteActor._
      import scala.actors.AbstractActor

      var g1 = initGram
      var g2 = g1.countlessCopy
      val timeout = 10000

      val trainingCorpus = new StringsOnlyCorpus
      trainingCorpus.readCorpus( trainYieldSpec )

      val testSentences = fromFile(testStringsPath).getLines().toList

      val hostsList:List[Array[String]] =
      fromFile(hostsPath).getLines().toList.map{_.split(' ')}


      def stoppingCondition( numIter:Int, deltaLogProb:Double ) =
        numIter > minIter && abs(deltaLogProb) < tolerance

      def localParserConstructor( grammar:ShakesPCNF ) = {
        val someParsers = ((0 to (numLocalParsers-1)) map{ parserSpec:Int =>
          new Actor with EstimationParser with LocalDefinitions with
            Heuristics /*with EveryOneHundred*/ {
              var parserID:ParserID = LocalParserID(parserSpec)
              var g = grammar
              var quietude = 100
            }
        }).toList

        someParsers foreach( _.start )

        someParsers
      }

      def remoteParserConstructor( grammar:ShakesPCNF ) = {
        var someParsers = (hostsList map{ parserSpec =>
            val Array(ip,port) = parserSpec
            //println( "Establishing connection with " + ip +":"+
            //port +"... ")

            select( Node(ip, port.toInt), 'parser )
        }).toList

        (0 to (someParsers.size-1)) filter ( id =>
          !  deadHosts.contains(RemoteParserID(id))) foreach{ index:Int =>
          someParsers( index ) !?(timeout, StillAlive) match {
            case Some(StillAlive) => {
              println( "Connecting to " + RemoteParserID( index ) )
              someParsers( index ) ! RemoteParserID( index )
              someParsers( index ) ! grammar
            }
            case None => {
              deadHosts += RemoteParserID(index)
              println( RemoteParserID(index) + " timed out; " + deadHosts.size +
                " dead parsers")
            }
            case what:Any => println("Got back " + what +
              " in remoteParserConstructor")
          }
        }


        //someParsers foreach( _ ! RemoteParserID( parserSpec ) )
        //(0 to (someParsers.size - 1)) foreach ( index =>
        //  if( ! deadHosts.contains( RemoteParserID(index)) )
        //    someParsers foreach( _ ! grammar )
        //)

        someParsers
      }

      //def iterationCleanup( parsers:List[AbstractActor] ) = ()

    }
    manager.start

  }
}

object trainAndEvaluateVanillaMinIterAndConvergence {
  def main( args:Array[String] ) {
    import scala.io.Source._
    import math._

    val nonTermCount = args(0).toInt
    val termFile = args(1)
    val trainYieldSpec = args(2)
    val testStringsPath = args(3)
    val numLocalParsers = args(4).toInt
    val hostsPath = args(5)
    val minIter = args(6).toInt
    val tolerance = args(7).toDouble
    val randSeed = args(8).toInt
    val randomBase = args(9).toInt

    println("nonTermCount: " + nonTermCount )
    println("termFile: " + termFile )
    println("trainYieldSpec: " + trainYieldSpec )
    println("testStringsPath: " + testStringsPath )
    println("numLocalParsers: "+ numLocalParsers )
    println("hostsPath: "+ hostsPath )
    println("minIter: " + minIter )
    println("tolerance: " + tolerance )
    println("randSeed: " + randSeed )
    println("randomBase: " + randomBase )

    println("\n\n\n====\n\n\n")

    val initGram = new ShakesPCNF

    val poslist = fromFile(termFile).getLines().toList
    val hosts = fromFile(hostsPath).getLines().toList

    initGram.randomizeGrammar(nonTermCount,poslist,randSeed,randomBase)

    //initGram.readGrammar( gramFile )
    //initGram.readLexicon( lexFile )

    
    object manager extends Actor with ShakesParserManager with
    StandardEMManager with EvaluatingManager with EveryOneHundred {
      import collection.mutable.ArrayBuffer
      import scala.actors.remote.Node
      import scala.actors.remote.RemoteActor._
      import scala.actors.AbstractActor

      var g1 = initGram
      var g2 = g1.countlessCopy
      val timeout = 10000

      val trainingCorpus = new StringsOnlyCorpus
      trainingCorpus.readCorpus( trainYieldSpec )

      val testSentences = fromFile(testStringsPath).getLines().toList

      val hostsList:List[Array[String]] =
      fromFile(hostsPath).getLines().toList.map{_.split(' ')}


      def stoppingCondition( numIter:Int, deltaLogProb:Double ) =
        numIter > minIter && abs(deltaLogProb) < tolerance

      def localParserConstructor( grammar:ShakesPCNF ) = {
        val someParsers = ((0 to (numLocalParsers-1)) map{ parserSpec:Int =>
          new Actor with EstimationParser with LocalDefinitions with
            Heuristics /*with EveryOneHundred*/ {
              var parserID:ParserID = LocalParserID(parserSpec)
              var g = grammar
              var quietude = 100
            }
        }).toList

        someParsers foreach( _.start )

        someParsers
      }

      def remoteParserConstructor( grammar:ShakesPCNF ) = {
        var someParsers = (hostsList map{ parserSpec =>
            val Array(ip,port) = parserSpec
            //println( "Establishing connection with " + ip +":"+
            //port +"... ")

            select( Node(ip, port.toInt), 'parser )
        }).toList

        (0 to (someParsers.size-1)) filter ( id =>
          !  deadHosts.contains(RemoteParserID(id))) foreach{ index:Int =>
          someParsers( index ) !?(timeout, StillAlive) match {
            case Some(StillAlive) => {
              println( "Connecting to " + RemoteParserID( index ) )
              someParsers( index ) ! RemoteParserID( index )
              someParsers( index ) ! grammar
            }
            case None => {
              deadHosts += RemoteParserID(index)
              println( RemoteParserID(index) + " timed out; " + deadHosts.size +
                " dead parsers")
            }
            case what:Any => println("Got back " + what +
              " in remoteParserConstructor")
          }
        }


        //someParsers foreach( _ ! RemoteParserID( parserSpec ) )
        //(0 to (someParsers.size - 1)) foreach ( index =>
        //  if( ! deadHosts.contains( RemoteParserID(index)) )
        //    someParsers foreach( _ ! grammar )
        //)

        someParsers
      }

      //def iterationCleanup( parsers:List[AbstractActor] ) = ()

    }
    manager.start

  }
}


object startRemoteParser {
  def main( args:Array[String] ) {
    val ip = args(0)
    val portToUse = args(1).toInt
    val thisRemoteParser = new Actor with EstimationParser with RemoteDefinitions with
      Heuristics {
        val host = ip
        val port = portToUse
        var parserID:ParserID = RemoteParserID(0)
        var quietude = 1
      }
    thisRemoteParser.start
  }
}

