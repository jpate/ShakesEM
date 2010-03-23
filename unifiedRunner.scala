package RunShakesEM {
  import ShakesEM._
  import scala.actors.Actor
  import scala.actors.Actor._

  trait Heuristics {
    var wordScale = 10000
  }


  object trainAndEvaluateVanillaUnifiedMinIterAndConvergence {
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
      val randSeed = args(8).toDouble
      val randomBase = args(9).toDouble
  
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
  
        def localParserConstructor( grammar:ShakesPCNF ) = {
          val someParsers = ((0 to numLocalParsers) map{ parserSpec:Int =>
            new Actor with CYKDefinitions with LocalDefinitions with
              Heuristics {
                var parserID:ParserID = LocalParserID(parserSpec)
                var g = grammar
              }
          }).toList
  
          someParsers foreach( _.start )
  
          someParsers
        }
        def remoteParserConstructor( grammar:ShakesPCNF ) = {
          var someParsers = (hosts map{ parserSpec:String =>
              val Array(ip,port) = parserSpec.split(' ')
              println( "Establishing connection with " + ip +":"+
              port +"... ")

              select( Node(ip, port.toInt), 'parser )
          }).toList

          (0 to (someParsers.size-1)) foreach{ index:Int =>
            someParsers( index ) ! RemoteParserID( index )
          }
  
          //someParsers foreach( _ ! RemoteParserID( parserSpec ) )
          someParsers foreach( _ ! grammar )
  
          someParsers
        }
  
        //def iterationCleanup( parsers:List[AbstractActor] ) = ()
  
      }
      manager.start
  
    }
  }
  object trainAndEvaluateVanillaMinIterAndConvergence
}