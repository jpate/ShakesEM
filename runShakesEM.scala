<<<<<<< HEAD
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
=======
import ShakesEM._


////object trainVanillaByIter {
////  def main( args: Array[String] ) {
////    import scala.collection.mutable.ArrayBuffer
////    import Math._
////    import scala.io.Source
////
////    var gramFile = args(0)
////    var lexFile = args(1)
////    var trainYieldFile = args(2)
////    var numParsers = args(3).toInt
////    var maxIter = args(4).toInt
////
////    val wordScale = 10000
////
////    val initGram = new ShakesPCNF
////
////    initGram.readGrammar(gramFile)
////    initGram.readLexicon(lexFile)
////
////    val trainingCorpus = new StringsOnlyCorpus
////
////    trainingCorpus.readCorpus( trainYieldFile )
////
////    //println(initGram)
////    //println(trainingCorpus)
////
////    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
////      def stoppingCondition( numIters:Int, x:Double ) = 
////        numIters >= maxIter
////
////      def parserConstructor = {
////        val someParsers = new ArrayBuffer[ShakesDistributedParser]
////
////        (0 to (numParsers-1) ) foreach(
////          someParsers += new ShakesEstimatingParser( _, g1, wordScale
////          )
////        )
////        someParsers
////      }
////
////      def useGrammar( trainedGram: ShakesPCNF) { }
////      def cleanup = ()
////    }
////
////    manager.start
////  }
////}
////
////object trainAndPrintVanillaByIter {
////  def main( args: Array[String] ) {
////    import scala.collection.mutable.ArrayBuffer
////    import Math._
////    import scala.io.Source
////
////    var gramFile = args(0)
////    var lexFile = args(1)
////    var trainYieldFile = args(2)
////    var numParsers = args(3).toInt
////    var maxIter = args(4).toInt
////
////    val wordScale = 10000
////
////    val initGram = new ShakesPCNF
////
////    initGram.readGrammar(gramFile)
////    initGram.readLexicon(lexFile)
////
////    val trainingCorpus = new StringsOnlyCorpus
////
////    trainingCorpus.readCorpus( trainYieldFile )
////
////    //println(initGram)
////    //println(trainingCorpus)
////
////    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
////      def stoppingCondition( numIters:Int, x:Double ) = 
////        numIters >= maxIter
////
////      def parserConstructor = {
////        val someParsers = new ArrayBuffer[ShakesDistributedParser]
////
////        (0 to (numParsers-1) ) foreach(
////          someParsers += new ShakesEstimatingParser( _, g1, wordScale
////          )
////        )
////        someParsers
////      }
////
////      def useGrammar( trainedGram: ShakesPCNF) { println(trainedGram) }
////      def cleanup = ()
////    }
////
////    manager.start
////  }
////}
////
////object trainAndPrintVanillaToConvergence{
////  def main( args: Array[String] ) {
////    import scala.collection.mutable.ArrayBuffer
////    import Math._
////    import scala.io.Source
////
////    var gramFile = args(0)
////    var lexFile = args(1)
////    var trainYieldFile = args(2)
////    var numParsers = args(3).toInt
////    var tolerance = args(4).toDouble
////
////    val wordScale = 10000
////
////    val initGram = new ShakesPCNF
////
////    initGram.readGrammar(gramFile)
////    initGram.readLexicon(lexFile)
////
////    val trainingCorpus = new StringsOnlyCorpus
////
////    trainingCorpus.readCorpus( trainYieldFile )
////
////    //println(initGram)
////    //println(trainingCorpus)
////
////    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
////      def stoppingCondition( n:Int, deltaLogProb:Double ) = 
////        abs(deltaLogProb) < tolerance
////
////      def parserConstructor = {
////        val someParsers = new ArrayBuffer[ShakesDistributedParser]
////
////        (0 to (numParsers-1) ) foreach(
////          someParsers += new ShakesEstimatingParser( _, g1, wordScale
////          )
////        )
////        someParsers
////      }
////
////      def useGrammar( trainedGram: ShakesPCNF) { println(trainedGram) }
////      def cleanup = ()
////    }
////
////    manager.start
////  }
////}
////
////object trainAndPrintBracketedByIter {
////  def main( args: Array[String] ) {
////    import collection.mutable.ArrayBuffer
////    import Math._
////    import scala.io.Source
////
////    var gramFile = args(0)
////    var lexFile = args(1)
////    var trainYieldSpec = args(2)
////    var numParsers = args(3).toInt
////    var maxIter = args(4).toInt
////
////    val wordScale = 10000
////
////    val initGram = new ShakesPCNF
////
////    initGram.readGrammar(gramFile)
////    initGram.readLexicon(lexFile)
////
////    val trainingCorpus = new BracketedCorpus
////
////    trainingCorpus.readCorpus( trainYieldSpec )
////
////    //println(initGram)
////    //println(trainingCorpus)
////
////    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
////
////      def stoppingCondition( numIters:Int, x:Double ) = 
////        numIters >= maxIter
////
////      def parserConstructor = {
////        val someParsers = new ArrayBuffer[ShakesDistributedParser]
////
////        (0 to (numParsers-1) ) foreach(
////          someParsers += new ShakesBracketedParser( _, g1, wordScale
////          )
////        )
////        someParsers
////      }
////      //def parserConstructor(id:Int) = new ShakesEstimatingParser( id, g1, wordScale )
////
////
////      def useGrammar( trainedGram: ShakesPCNF) { println(trainedGram) }
////      def cleanup = ()
////    }
////
////    manager.start
////  }
////}
////
object trainAndEvaluateBracketedMinIterAndConvergence {
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

    println("nonTermCount: " + nonTermCount )
    println("termFile: " + termFile )
    println("trainYieldSpec: "+  trainYieldSpec )
    println("testStringsPath: " +   testStringsPath )
    println("numParsers: "+    numParsers )
    println("minIter: " +    minIter )
    println("tolerance: " + tolerance )
    println("randSeed: " + randSeed )
    println("randomBase: " + randomBase )
    println("wordScale: " + wordScale )

    println("\n\n\n====\n\n\n")

    val initGram = new ShakesPCNF

    val termList =
      fromFile( termFile ).getLines.toList.filter( _.length > 0 ).
      map (_.replace("\n",""))

    //initGram.readGrammar(gramFile)
    //initGram.readLexicon(lexFile)

    initGram.randomizeGrammar( nonTermCount, termList, randSeed, randomBase )

    val trainingCorpus = new BracketedCorpus
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
          someParsers += new ShakesBracketedParser( _, g1, wordScale
          )
        )
        someParsers
      }

      def stoppingCondition( iterNum:Int, deltaLogProb:Double ) = 
        iterNum > minIter && abs(deltaLogProb) < tolerance
      def cleanup = {
        VitActor ! (g1,Stop)
>>>>>>> 92651b2... this all seems to work. remote actors next.
      }
  
      manager.start
    }
  }
<<<<<<< HEAD
  
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
=======
}
object trainAndEvaluateBracketedToConvergence {
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

    val trainingCorpus = new BracketedCorpus
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
>>>>>>> 92651b2... this all seems to work. remote actors next.
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



<<<<<<< HEAD
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
=======
    //initGram.readGrammar(gramFile)
    //initGram.readLexicon(lexFile)

    initGram.randomizeGrammar( nonTermCount, termList, randSeed, 100 )

    val trainingCorpus = new BracketedCorpus
    trainingCorpus.readCorpus( trainYieldSpec )

    println(trainingCorpus)

    val testStrings = fromFile(testStringsPath).getLines.toList.filter(_.length >
      0).map(_.replace("\n","")).toList


    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
      import java.io._

      object VitActor extends EvaluationActor(initGram,10000) {
        var testCorpus = testStrings
      }

      VitActor.start

      def useGrammar( trainedGram: ShakesPCNF, n:Int) {
        VitActor ! g1.copy
        //println( VitActor.parseString )
      }

      def parserConstructor = {
        val someParsers = new ArrayBuffer[ShakesDistributedParser]

        (0 to (numParsers-1) ) foreach(
          someParsers += new ShakesBracketedParser( _, g1, wordScale
          )
        )
        someParsers
      }

      def stoppingCondition( numIters:Int, x:Double ) = 
        numIters >= maxIter

      def cleanup = {
        VitActor ! Stop 
        VitActor ! g1.copy
>>>>>>> 92651b2... this all seems to work. remote actors next.
      }
  
      manager.start
    }
  }
<<<<<<< HEAD
  
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
=======
}

object trainAndEvaluateBracketedByIterAndReadGrammar {
  def main( args: Array[String] ) {
    import scala.actors.Actor
    import scala.actors.Actor._
    import collection.mutable.ArrayBuffer
    import Math._
    import scala.io.Source._

    val gramFile = args(0)
    val lexFile = args(1)
    val trainYieldSpec = args(2)
    val testStringsPath = args(3)
    val numParsers = args(4).toInt
    val maxIter = args(5).toInt
    //val randSeed = args(6).toInt

    val wordScale = 10000

    val initGram = new ShakesPCNF

//    val termList =
//      fromFile( termFile ).getLines.toList.filter( _.length > 0 ).
//      map (_.replace("\n",""))

    initGram.readGrammar(gramFile)
    initGram.readLexicon(lexFile)

    //initGram.randomizeGrammar( nonTermCount, termList, randSeed, 100 )

    val trainingCorpus = new BracketedCorpus
    trainingCorpus.readCorpus( trainYieldSpec )

    println(trainingCorpus)

    val testStrings = fromFile(testStringsPath).getLines.toList.filter(_.length >
      0).map(_.replace("\n","")).toList


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
          println( "Iter" + iterNum + ":" + vit.parseString )
        }

      }

      ////def useGrammar( trainedGram: ShakesPCNF) {
      ////  VitActor ! g1.copy
      ////  //println( VitActor.parseString )
      ////}

      def parserConstructor = {
        val someParsers = new ArrayBuffer[ShakesDistributedParser]

        (0 to (numParsers-1) ) foreach(
          someParsers += new ShakesBracketedParser( _, g1, wordScale )
        )
        someParsers
      }

      def stoppingCondition( numIters:Int, x:Double ) = 
        numIters >= maxIter

      def cleanup = {
////        VitActor ! Stop 
////        VitActor ! g1.copy
>>>>>>> 92651b2... this all seems to work. remote actors next.
      }
      manager.start
    }
  }

}


