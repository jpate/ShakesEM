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

        def stoppingCondition( numIter:Int, deltaLogProb:Double ) = numIter >=1

        def parserConstructor( grammar:ShakesPCNF ) = {

          //val someParsers = new List[Actor]

          val localVanilla = new Actor with CYKDefinitions with LocalDefinitions
          with Heuristics {
            val id = 0
            var g = initGram
          }

          //someParsers foreach( _.start )
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
      val gramFile = args(0)
      val lexFile = args(1)
      val yieldFile = args(2)

      val initGram = new ShakesPCNF

      initGram.readGrammar( gramFile )
      initGram.readLexicon( lexFile )

      
      object manager extends Actor with ShakesParserManager {
        import collection.mutable.ArrayBuffer
        import scala.actors.remote.Node
        import scala.actors.remote.RemoteActor._
        import scala.actors.AbstractActor

        var g1 = initGram
        var g2 = g1.countlessCopy

        val trainingCorpus = new StringsOnlyCorpus
        trainingCorpus.readCorpus( yieldFile )

        def stoppingCondition( numIter:Int, deltaLogProb:Double ) = numIter >=1

        def parserConstructor( grammar:ShakesPCNF ) = {

          //val someParsers = new ArrayBuffer[AbstractActor]

          val someParsers = select( Node("127.0.0.1", 9999), 'parser ) :: Nil

          someParsers foreach( _ ! grammar )

          someParsers
        }

        def useGrammar( trainedGram:ShakesPCNF, iterNum:Int ) {
          println( "Iteration " + iterNum + ":\n" + trainedGram )
        }
      }
      manager.start

    }
  }

  object startRemoteVanillaParser {
    def main( args:Array[String] ) {
      val thisRemoteParser = new Actor with
        CYKDefinitions with
        RemoteDefinitions with
        Heuristics {
          val host = "127.0.0.1"
          val port = 9999
          val id = 0
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
        val id = 0
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
        val id = 0
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

      val remoteVanilla = new Actor with CYKDefinitions with RemoteDefinitions
      with Heuristics {
        val id = 0
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
        val id = 0
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

