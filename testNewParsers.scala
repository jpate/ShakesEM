package testNewParsers {
  import ShakesEM._
  import scala.actors.Actor
  import scala.actors.Actor._

  trait Heuristics {
    var wordScale = 10000
  }

  object testLocalManagedVanilla {
    def main( args:Array[String] ) {
      val gramFile = args(0)
      val lexFile = args(1)
      val yieldFile = args(2)


      val initGram = new ShakesPCNF

      initGram.readGrammar( gramFile )
      initGram.readLexicon( lexFile )

      
      object manager extends Actor with ShakesParserManager {
        import collection.mutable.ArrayBuffer
        import scala.actors.AbstractActor

        var g1 = initGram
        var g2 = g1.countlessCopy

        val trainingCorpus = new StringsOnlyCorpus
        trainingCorpus.readCorpus( yieldFile )

        def finalCleanup(trainedGram:ShakesPCNF) = ()

        def stoppingCondition( numIter:Int, deltaLogProb:Double ) = numIter >=1

        def iterationCleanup( parsers:List[AbstractActor] ) = ()
        def parserConstructor( grammar:ShakesPCNF ) = {

          //val someParsers = new List[Actor]

          val localVanilla = new Actor with CYKDefinitions with LocalDefinitions
          with Heuristics {
            val parserID = "0"
            var g = initGram
          }

          //someParsers foreach( _.start )
          println("trying to start parser..")
          localVanilla.start

          localVanilla::Nil
        }

        def useGrammar( trainedGram:ShakesPCNF, iterNum:Int ) {
          println( "Iteration " + iterNum + ":\n" + trainedGram )
        }
      }
      manager.start

    }
  }

  object testRemoteManagedVanilla {
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

        val trainingCorpus = new StringsOnlyCorpus
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

  object startRemoteVanillaParser {
    def main( args:Array[String] ) {
      val portToUse = args(0).toInt
      val thisRemoteParser = new Actor with CYKDefinitions with RemoteDefinitions with
        Heuristics {
          val host = "127.0.0.1"
          val port = portToUse
          val parserID = host+":"+portToUse
        }
      thisRemoteParser.start
    }
  }

  object testLocalBracketed {
    def main( args:Array[String] ) {

      val initGram = new ShakesPCNF

      initGram.readGrammar("development/toyGrammar.txt")
      initGram.readLexicon("development/toyLexicon.txt") 

      val text = new BracketedCorpus
      text.readCorpus("development/testSentences")

      val localBracketed = new Actor with BracketedDefinitions with LocalDefinitions
      with Heuristics {
        val parserID = "0"
        var g = initGram
      }

      localBracketed.start

      localBracketed ! text(0)

      localBracketed ! Stop

    }
  }

  object testLocalVanilla {
    def main( args:Array[String] ) {

      val initGram = new ShakesPCNF

      initGram.readGrammar("development/toyGrammar.txt")
      initGram.readLexicon("development/toyLexicon.txt") 

      val text = new StringsOnlyCorpus
      text.readCorpus("development/testSentences.txt")

      val localVanilla = new Actor with CYKDefinitions with LocalDefinitions
      with Heuristics {
        val parserID = "0"
        var g = initGram
      }

      localVanilla.start

      localVanilla ! text(0)

      localVanilla ! Stop

    }
  }

  object testRemoteVanilla {
    import scala.actors.remote.RemoteActor
    import scala.actors.remote.RemoteActor._
    import scala.actors.remote.Node
    def main( args:Array[String] ) {

      val initGram = new ShakesPCNF

      initGram.readGrammar("development/toyGrammar.txt")
      initGram.readLexicon("development/toyLexicon.txt") 

      val text = new StringsOnlyCorpus
      text.readCorpus("development/testSentences.txt")

      val remoteVanilla = new Actor with CYKDefinitions with RemoteDefinitions with Heuristics {
        val parserID = "0"
        val host = "127.0.0.1"
        val port = 9999
      }

      remoteVanilla.start

      val accessVanilla = select( Node("127.0.0.1", 9999) , 'parser )

      remoteVanilla ! initGram

      remoteVanilla ! text(0)


      remoteVanilla ! Stop

    }
  }

  object testRemoteBracketed {
    import scala.actors.remote.RemoteActor
    import scala.actors.remote.RemoteActor._
    import scala.actors.remote.Node
    def main( args:Array[String] ) {

      val initGram = new ShakesPCNF

      initGram.readGrammar("development/toyGrammar.txt")
      initGram.readLexicon("development/toyLexicon.txt") 

      val text = new BracketedCorpus
      text.readCorpus("development/testSentences")

      val remoteBracketed = new Actor with BracketedDefinitions with RemoteDefinitions
      with Heuristics {
        val parserID = "0"
        val host = "127.0.0.1"
        val port = 9999
      }

      remoteBracketed.start

      val accessBracketed = select( Node("129.215.90.105", 9999) , 'parser )

      remoteBracketed ! initGram

      remoteBracketed ! text(0)

      remoteBracketed ! Stop

    }
  }
}

