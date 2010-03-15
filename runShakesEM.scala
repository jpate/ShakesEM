package RunShakesEM {
  import ShakesEM._

  object trainAndEvaluateEstimatingMinIterAndConvergence {
    def main( args: Array[String] ) {
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
  
      println("nonTermCount: " + nonTermCount )
      println("termFile: " + termFile )
      println("trainYieldSpec: " + trainYieldSpec )
      println("testStringsPath: " + testStringsPath )
      println("numParsers: "+ numParsers )
      println("minIter: " + minIter )
      println("tolerance: " + tolerance )
      println("randSeed: " + randSeed )
      println("randomBase: " + randomBase )
      println("wordScale: " + wordScale )
  
      println("\n\n\n====\n\n\n")
  
      val initGram = new ShakesPCNF
  
      val termList =
        fromFile( termFile ).getLines.toList.filter( _.length > 0 ).
        map (_.replace("\n",""))
  
  
      initGram.randomizeGrammar( nonTermCount, termList, randSeed, randomBase )
  
      val trainingCorpus = new StringsOnlyCorpus
      trainingCorpus.readCorpus( trainYieldSpec )
  
      val testStrings = fromFile(testStringsPath).getLines.toList.filter(_.length >
        0).map(_.replace("\n",""))
  
  
      object manager extends ShakesParserManager( initGram ) {
        import java.io._
  
        val trainCorpus = trainingCorpus
  
  
        object VitActor extends EvaluationActor(initGram,10000) {
          var testCorpus = testStrings
        }
        VitActor.start
  
        def useGrammar( trainedGram: ShakesPCNF, iterNum:Int) {
          VitActor ! Tuple2(g1,iterNum)
  
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
          VitActor ! Tuple2(g1,Stop)
        }
      }
  
      manager.start
    }
  }
  
  object trainAndEvaluateEstimatingByIterAndReadGrammar {
    def main( args: Array[String] ) {
      import collection.mutable.ArrayBuffer
      import Math._
      import scala.io.Source._
  
      val gramFile = args(0)
      val lexFile = args(1)
      val trainYieldSpec = args(2)
      val testStringsPath = args(3)
      val numParsers = args(4).toInt
      val maxIter = args(5).toInt
  
      val wordScale = 10000
  
      val initGram = new ShakesPCNF
  
      initGram.readGrammar(gramFile)
      initGram.readLexicon(lexFile)
  
  
      val trainingCorpus = new StringsOnlyCorpus
      trainingCorpus.readCorpus( trainYieldSpec )
  
      println(trainingCorpus)
  
      val testStrings = fromFile(testStringsPath).getLines.toList.filter(_.length >
        0).map(_.replace("\n","")).toList
  
  
      object manager extends ShakesParserManager( initGram ) {
        import java.io._
  
        val trainCorpus = trainingCorpus
  
        def useGrammar( trainedGram: ShakesPCNF, iterNum:Int) {
          
          val vit = new ShakesViterbiParser(trainedGram, wordScale)
          
          for( s <- testStrings ) {
            val testWords = s.split(' ')
            vit.resize( testWords.size + 1 )
            vit.populateChart( testWords )
            println( "Iter" + iterNum + ":" + vit.parseString )
          }
  
          println( trainedGram )
  
        }
  
  
        def parserConstructor = {
          val someParsers = new ArrayBuffer[ShakesDistributedParser]
  
          (0 to (numParsers-1) ) foreach(
            someParsers += new ShakesEstimatingParser( _, g1, wordScale )
          )
          someParsers
        }
  
        def stoppingCondition( numIters:Int, x:Double ) = 
          numIters >= maxIter
  
        def cleanup = ()
      }
      manager.start
    }
  }



  object trainAndEvaluateBracketedMinIterAndConvergence {
    def main( args: Array[String] ) {
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
  
      println("nonTermCount: " + nonTermCount )
      println("termFile: " + termFile )
      println("trainYieldSpec: " + trainYieldSpec )
      println("testStringsPath: " + testStringsPath )
      println("numParsers: "+ numParsers )
      println("minIter: " + minIter )
      println("tolerance: " + tolerance )
      println("randSeed: " + randSeed )
      println("randomBase: " + randomBase )
      println("wordScale: " + wordScale )
  
      println("\n\n\n====\n\n\n")
  
      val initGram = new ShakesPCNF
  
      val termList =
        fromFile( termFile ).getLines.toList.filter( _.length > 0 ).
        map (_.replace("\n",""))
  
  
      initGram.randomizeGrammar( nonTermCount, termList, randSeed, randomBase )
  
      val trainingCorpus = new BracketedCorpus
      trainingCorpus.readCorpus( trainYieldSpec )
  
      val testStrings = fromFile(testStringsPath).getLines.toList.filter(_.length >
        0).map(_.replace("\n",""))
  
  
      object manager extends ShakesParserManager( initGram ) {
        import java.io._
  
        val trainCorpus = trainingCorpus
  
  
        object VitActor extends EvaluationActor(initGram,10000) {
          var testCorpus = testStrings
        }
        VitActor.start
  
        def useGrammar( trainedGram: ShakesPCNF, iterNum:Int) {
          VitActor ! Tuple2(g1,iterNum)
  
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
            someParsers += new ShakesBracketedParser( _, g1, wordScale
            )
          )
          someParsers
        }
  
        def stoppingCondition( iterNum:Int, deltaLogProb:Double ) = 
          iterNum > minIter && abs(deltaLogProb) < tolerance
        def cleanup = {
          VitActor ! Tuple2(g1,Stop)
        }
      }
  
      manager.start
    }
  }
  
  object trainAndEvaluateBracketedByIterAndReadGrammar {
    def main( args: Array[String] ) {
      import collection.mutable.ArrayBuffer
      import Math._
      import scala.io.Source._
  
      val gramFile = args(0)
      val lexFile = args(1)
      val trainYieldSpec = args(2)
      val testStringsPath = args(3)
      val numParsers = args(4).toInt
      val maxIter = args(5).toInt
  
      val wordScale = 10000
  
      val initGram = new ShakesPCNF
  
      initGram.readGrammar(gramFile)
      initGram.readLexicon(lexFile)
  
  
      val trainingCorpus = new BracketedCorpus
      trainingCorpus.readCorpus( trainYieldSpec )
  
      println(trainingCorpus)
  
      val testStrings = fromFile(testStringsPath).getLines.toList.filter(_.length >
        0).map(_.replace("\n","")).toList
  
  
      object manager extends ShakesParserManager( initGram ) {
        import java.io._
  
        val trainCorpus = trainingCorpus
  
        def useGrammar( trainedGram: ShakesPCNF, iterNum:Int) {
          
          val vit = new ShakesViterbiParser(trainedGram, wordScale)
          
          for( s <- testStrings ) {
            val testWords = s.split(' ')
            vit.resize( testWords.size + 1 )
            vit.populateChart( testWords )
            println( "Iter" + iterNum + ":" + vit.parseString )
          }
  
          println( trainedGram )
  
        }
  
  
        def parserConstructor = {
          val someParsers = new ArrayBuffer[ShakesDistributedParser]
  
          (0 to (numParsers-1) ) foreach(
            someParsers += new ShakesBracketedParser( _, g1, wordScale )
          )
          someParsers
        }
  
        def stoppingCondition( numIters:Int, x:Double ) = 
          numIters >= maxIter
  
        def cleanup = ()
      }
      manager.start
    }
  }

}


