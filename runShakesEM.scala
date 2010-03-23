package RunShakesEM {
  import ShakesEM._
  import scala.actors.Actor
  import scala.actors.Actor._

  trait Heuristics {
    var wordScale = 10000
  }

  object trainAndEvaluateRemoteBracketedMinIterAndConvergence {
    def main( args:Array[String] ) {
      import scala.io.Source._
      import math._
  
      val nonTermCount = args(0).toInt
      val termFile = args(1)
      val trainYieldSpec = args(2)
      val testStringsPath = args(3)
      val hostsPath = args(4)
      val minIter = args(5).toInt
      val tolerance = args(6).toDouble
      val randSeed = args(7).toInt
      val randomBase = args(8).toDouble
  
      println("nonTermCount: " + nonTermCount )
      println("termFile: " + termFile )
      println("trainYieldSpec: " + trainYieldSpec )
      println("testStringsPath: " + testStringsPath )
      println("hostsPath: "+ hostsPath )
      println("minIter: " + minIter )
      println("tolerance: " + tolerance )
      println("randSeed: " + randSeed )
      println("randomBase: " + randomBase )
  
      println("\n\n\n====\n\n\n")
  
      val initGram = new ShakesPCNF
  
      val poslist = fromPath(termFile).getLines("\n").toList
      val hosts = fromPath(hostsPath).getLines("\n").map{_.split(' ')}.toList
  
      initGram.randomizeGrammar(nonTermCount,poslist,16,0)
  
      //initGram.readGrammar( gramFile )
      //initGram.readLexicon( lexFile )
  
      
      object manager extends Actor with ShakesParserManager with
      EvaluatingManager {
        import collection.mutable.ArrayBuffer
        import scala.actors.remote.Node
        import scala.actors.remote.RemoteActor._
        import scala.actors.AbstractActor
  
        var g1 = initGram
        var g2 = g1.countlessCopy
  
        val trainingCorpus = new BracketedCorpus
        trainingCorpus.readCorpus( trainYieldSpec )
  
        val testSentences = fromPath(testStringsPath).getLines("\n").toList
  
  
        def stoppingCondition( numIter:Int, deltaLogProb:Double ) =
          numIter > minIter && abs(deltaLogProb) < tolerance
  
        def parserConstructor( grammar:ShakesPCNF ) = {
          var someParsers = hosts map( parserSpec =>
            select( Node(parserSpec(0), parserSpec(1).toInt), 'parser )
          )
  
          someParsers foreach( _ ! grammar )
  
          someParsers
        }
  
        def iterationCleanup( parsers:List[AbstractActor] ) = ()
  
      }
      manager.start
  
    }
  }

  object trainAndEvaluateRemoteVanillaMinIterAndConvergence {
    def main( args:Array[String] ) {
      import scala.io.Source._
      import math._
  
      val nonTermCount = args(0).toInt
      val termFile = args(1)
      val trainYieldSpec = args(2)
      val testStringsPath = args(3)
      val hostsPath = args(4)
      val minIter = args(5).toInt
      val tolerance = args(6).toDouble
      val randSeed = args(7).toDouble
      val randomBase = args(8).toDouble
  
      println("nonTermCount: " + nonTermCount )
      println("termFile: " + termFile )
      println("trainYieldSpec: " + trainYieldSpec )
      println("testStringsPath: " + testStringsPath )
      println("hostsPath: "+ hostsPath )
      println("minIter: " + minIter )
      println("tolerance: " + tolerance )
      println("randSeed: " + randSeed )
      println("randomBase: " + randomBase )
  
      println("\n\n\n====\n\n\n")
  
      val initGram = new ShakesPCNF
  
      val poslist = fromPath(termFile).getLines("\n").toList
      val hosts = fromPath(hostsPath).getLines("\n").toList
  
      initGram.randomizeGrammar(nonTermCount,poslist,16,0)
  
      //initGram.readGrammar( gramFile )
      //initGram.readLexicon( lexFile )
  
      
      object manager extends Actor with ShakesParserManager with
      EvaluatingManager {
        import collection.mutable.ArrayBuffer
        import scala.actors.remote.Node
        import scala.actors.remote.RemoteActor._
        import scala.actors.AbstractActor
  
        var g1 = initGram
        var g2 = g1.countlessCopy
  
        val trainingCorpus = new StringsOnlyCorpus
        trainingCorpus.readCorpus( trainYieldSpec )
  
        val testSentences = fromPath(testStringsPath).getLines("\n").toList
  
  
        def stoppingCondition( numIter:Int, deltaLogProb:Double ) =
          numIter > minIter && abs(deltaLogProb) < tolerance
  
        def parserConstructor( grammar:ShakesPCNF ) = {
          var someParsers = (hosts map{ parserSpec:String =>
              val Array(ip,port) = parserSpec.split(' ')
              println( "Establishing connection with " + ip +":"+
              port +"... ")
              select( Node(ip, port.toInt), 'parser )
          }).toList
  
          someParsers foreach( _ ! grammar )
  
          someParsers
        }
  
        def iterationCleanup( parsers:List[AbstractActor] ) = ()
  
      }
      manager.start
  
    }
  }


  object trainAndEvaluateLocalVanillaMinIterAndConvergence {
    def main( args:Array[String] ) {
      import scala.io.Source._
      import math._
  
      val nonTermCount = args(0).toInt
      val termFile = args(1)
      val trainYieldSpec = args(2)
      val testStringsPath = args(3)
      val numParsers = args(4).toInt
      val minIter = args(5).toInt
      val tolerance = args(6).toDouble
      val randSeed = args(7).toDouble
      val randomBase = args(8).toDouble
  
      println("nonTermCount: " + nonTermCount )
      println("termFile: " + termFile )
      println("trainYieldSpec: " + trainYieldSpec )
      println("testStringsPath: " + testStringsPath )
      println("numParsers: "+ numParsers )
      println("minIter: " + minIter )
      println("tolerance: " + tolerance )
      println("randSeed: " + randSeed )
      println("randomBase: " + randomBase )
  
      println("\n\n\n====\n\n\n")
  
      val initGram = new ShakesPCNF
  
      val poslist = fromPath(termFile).getLines("\n").toList
  
      initGram.randomizeGrammar(nonTermCount,poslist,16,0)
  
      //initGram.readGrammar( gramFile )
      //initGram.readLexicon( lexFile )
  
      
      object manager extends Actor with ShakesParserManager with
      EvaluatingManager {
        import collection.mutable.ArrayBuffer
        import scala.actors.AbstractActor
  
        var g1 = initGram
        var g2 = g1.countlessCopy
  
        val trainingCorpus = new StringsOnlyCorpus
        trainingCorpus.readCorpus( trainYieldSpec )
  
        val testSentences = fromPath(testStringsPath).getLines("\n").toList
  
  
        def stoppingCondition( numIter:Int, deltaLogProb:Double ) =
          numIter > minIter && abs(deltaLogProb) < tolerance
  
        def parserConstructor( grammar:ShakesPCNF ) = {
          val someParsers = ((0 to numParsers) map{ parserSpec:Int =>
            new Actor with CYKDefinitions with LocalDefinitions with
              Heuristics {
                val parserID = "Parser"+parserSpec
                var g = grammar
              }
          }).toList
  
          someParsers foreach( _.start )
  
          someParsers
        }
  
        def iterationCleanup( parsers:List[AbstractActor] ) = ()
  
      }
      manager.start
    }
  }

  object trainAndEvaluateLocalBracketedMinIterAndConvergence {
    def main( args:Array[String] ) {
      import scala.io.Source._
      import math._
  
      val nonTermCount = args(0).toInt
      val termFile = args(1)
      val trainYieldSpec = args(2)
      val testStringsPath = args(3)
      val numParsers = args(4).toInt
      val minIter = args(5).toInt
      val tolerance = args(6).toDouble
      val randSeed = args(7).toInt
      val randomBase = args(8).toDouble
  
      println("nonTermCount: " + nonTermCount )
      println("termFile: " + termFile )
      println("trainYieldSpec: " + trainYieldSpec )
      println("testStringsPath: " + testStringsPath )
      println("numParsers: "+ numParsers )
      println("minIter: " + minIter )
      println("tolerance: " + tolerance )
      println("randSeed: " + randSeed )
      println("randomBase: " + randomBase )
  
      println("\n\n\n====\n\n\n")
  
      val initGram = new ShakesPCNF
  
      val poslist = fromPath(termFile).getLines("\n").toList
  
      initGram.randomizeGrammar(nonTermCount,poslist,16,0)
  
      //initGram.readGrammar( gramFile )
      //initGram.readLexicon( lexFile )
  
      
      object manager extends Actor with ShakesParserManager with
      EvaluatingManager {
        import collection.mutable.ArrayBuffer
        import scala.actors.AbstractActor
  
        var g1 = initGram
        var g2 = g1.countlessCopy
  
        val trainingCorpus = new BracketedCorpus
        trainingCorpus.readCorpus( trainYieldSpec )
  
        val testSentences = fromPath(testStringsPath).getLines("\n").toList
  
  
        def stoppingCondition( numIter:Int, deltaLogProb:Double ) =
          numIter > minIter && abs(deltaLogProb) < tolerance
  
        def parserConstructor( grammar:ShakesPCNF ) = {
          val someParsers = (0 to numParsers) map{ parserSpec:Int =>
            new Actor with BracketedDefinitions with LocalDefinitions with
              Heuristics {
                val parserID = "Parser"+parserSpec
                var g = grammar
              }
          }
  
          someParsers foreach( _.start )
  
          someParsers
        }.toList
  
        def iterationCleanup( parsers:List[AbstractActor] ) = ()
  
      }
      manager.start
    }
  }


  object startRemoteBracketedParser {
    def main( args:Array[String] ) {
      val ip = args(0)
      val portToUse = args(1).toInt
      val thisRemoteParser = new Actor with BracketedDefinitions with RemoteDefinitions with
        Heuristics {
          val host = ip
          val port = portToUse
          val parserID = host+":"+portToUse
        }
      thisRemoteParser.start
    }
  }

  object startRemoteVanillaParser {
    def main( args:Array[String] ) {
      val ip = args(0)
      val portToUse = args(1).toInt
      val thisRemoteParser = new Actor with CYKDefinitions with RemoteDefinitions with
        Heuristics {
          val host = ip
          val port = portToUse
          val parserID = host+":"+portToUse
        }
      thisRemoteParser.start
    }
  }

}

