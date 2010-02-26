import ShakesEM._


object trainAndEvaluateVanillaMinIterAndConvergence {
  def main( args: Array[String] ) {
    import scala.actors.Actor
    import scala.actors.Actor._
    import collection.mutable.ArrayBuffer
    import Math._
    import scala.io.Source._

    val nonTermCount = args(0).toInt
    val termFile = args(1)
    val trainYieldSpec = args(2)
    val testStringsPath = args(3)
    val numParsers = args(4).toInt
    val minIter = args(5).toInt
    val tolerance = args(6).toDouble
    val randSeed = args(7).toInt
    val randomBase = args(8).toInt

    val wordScale = 10000

    val initGram = new ShakesPCNF

    val termList =
      fromFile( termFile ).getLines.toList.filter( _.length > 0 ).
      map (_.replace("\n",""))

    //initGram.readGrammar(gramFile)
    //initGram.readLexicon(lexFile)

    initGram.randomizeGrammar( nonTermCount, termList, randSeed, randomBase )

    val trainingCorpus = new StringsOnlyCorpus
    trainingCorpus.readCorpus( trainYieldSpec )

    val testStrings = fromFile(testStringsPath).getLines.toList.filter(_.length >
      0).map(_.replace("\n",""))


    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
      import java.io._



      object VitActor extends EvaluationActor(initGram,10000) {
        var testCorpus = testStrings
      }
      VitActor.start

      def useGrammar( trainedGram: ShakesPCNF, iterNum:Int) {
        VitActor ! (g1,iterNum)

        //val vit = new ShakesViterbiParser(trainedGram, wordScale)
        //
        //for( s <- testStrings ) {
        //  val testWords = s.split(' ')
        //  vit.resize( testWords.size + 1 )
        //  vit.populateChart( testWords )
        //  if( iterNum % 2 == 0 )
        //    println( "Iter" + iterNum + ":" + vit.parseString )
        //}
      }

      def parserConstructor = {
        val someParsers = new ArrayBuffer[ShakesDistributedParser]

        (0 to (numParsers-1) ) foreach(
          someParsers += new ShakesEstimatingParser( _, g1, wordScale
          )
        )
        someParsers
      }

      def stoppingCondition( iterNum:Int, deltaLogProb:Double ) = 
        iterNum > minIter && abs(deltaLogProb) < tolerance
      def cleanup = {
        VitActor ! (g1,Stop)
      }
    }

    manager.start
  }
}

object trainAndEvaluateVanillaToConvergence {
  def main( args: Array[String] ) {
    import scala.actors.Actor
    import scala.actors.Actor._
    import collection.mutable.ArrayBuffer
    import Math._
    import scala.io.Source._

    val nonTermCount = args(0).toInt
    val termFile = args(1)
    val trainYieldSpec = args(2)
    val testStringsPath = args(3)
    val numParsers = args(4).toInt
    val tolerance = args(5).toDouble
    val randSeed = args(6).toInt
    val randomBase = args(7).toInt

    val wordScale = 10000

    val initGram = new ShakesPCNF

    val termList =
      fromFile( termFile ).getLines.toList.filter( _.length > 0 ).
      map (_.replace("\n",""))

    //initGram.readGrammar(gramFile)
    //initGram.readLexicon(lexFile)

    initGram.randomizeGrammar( nonTermCount, termList, randSeed, randomBase )

    val trainingCorpus = new StringsOnlyCorpus
    trainingCorpus.readCorpus( trainYieldSpec )

    val testStrings = fromFile(testStringsPath).getLines.toList.filter(_.length >
      0).map(_.replace("\n",""))


    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
      import java.io._



      ////object VitActor extends EvaluationActor(initGram,10000) {
      ////  var testCorpus = testStrings
      ////}
      ////VitActor.start

      def useGrammar( trainedGram: ShakesPCNF, iterNum:Int) {
        //VitActor ! g1

        val vit = new ShakesViterbiParser(trainedGram, wordScale)
        
        for( s <- testStrings ) {
          val testWords = s.split(' ')
          vit.resize( testWords.size + 1 )
          vit.populateChart( testWords )
          if( iterNum % 2 == 0 )
            println( "Iter" + iterNum + ":" + vit.parseString )
        }
      }

      def parserConstructor = {
        val someParsers = new ArrayBuffer[ShakesDistributedParser]

        (0 to (numParsers-1) ) foreach(
          someParsers += new ShakesEstimatingParser( _, g1, wordScale
          )
        )
        someParsers
      }

      def stoppingCondition( n:Int, deltaLogProb:Double ) = 
        abs(deltaLogProb) < tolerance
      def cleanup = {
        //VitActor ! Stop 
        //VitActor ! g1
      }
    }

    manager.start
  }
}

