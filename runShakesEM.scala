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

    //initGram.readGrammar(gramFile)
    //initGram.readLexicon(lexFile)

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


    object manager extends ShakesParserManager( initGram ) {
      import java.io._

      val trainCorpus = trainingCorpus


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
          someParsers += new ShakesBracketedParser( _, g1, wordScale
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
////
////object trainAndEvaluateVanillaToConvergence {
////  def main( args: Array[String] ) {
////    import scala.actors.Actor
////    import scala.actors.Actor._
////    import collection.mutable.ArrayBuffer
////    import Math._
////    import scala.io.Source._
////
////    val nonTermCount = args(0).toInt
////    val termFile = args(1)
////    val trainYieldPath = args(2)
////    val testStringsPath = args(3)
////    val numParsers = args(4).toInt
////    val tolerance = args(5).toDouble
////    val randSeed = args(6).toInt
////
////    val wordScale = 10000
////
////    val initGram = new ShakesPCNF
////
////    val termList =
////      fromFile( termFile ).getLines.toList.filter( _.length > 0 ).
////      map (_.replace("\n",""))
////
////    //initGram.readGrammar(gramFile)
////    //initGram.readLexicon(lexFile)
////
////    initGram.randomizeGrammar( nonTermCount, termList, randSeed, 100 )
////
////    val trainingCorpus = new StringsOnlyCorpus
////    trainingCorpus.readCorpus( trainYieldPath )
////
////    val testStrings = fromFile(testStringsPath).getLines.toList.filter(_.length >
////      0).map(_.replace("\n",""))
////
////
////    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
////      import java.io._
////
////      object VitActor extends EvaluationActor(initGram,10000) {
////        var testCorpus = testStrings
////      }
////
////      VitActor.start
////
////      def useGrammar( trainedGram: ShakesPCNF) {
////        VitActor ! g1
////
////        /*
////        val bw = new BufferedWriter(new
////        FileWriter(gramFilePrefix+"Iter"+iterationNum,false));
////        bw.write(
////          "Corpus log probability: " +
////          corpusLogProb +
////          "\nCorpus probability: " +
////          exp(corpusLogProb) +
////          "\nDelta LogProb: " +
////          deltaLogProb +
////          "\n\n" +
////          g1.toString
////        );
////        bw.close();
////        */
////        //println( VitActor.parseString )
////      }
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
////      def stoppingCondition( n:Int, deltaLogProb:Double ) = 
////        deltaLogProb < tolerance
////      def cleanup = {
////        VitActor ! Stop 
////        VitActor ! g1
////      }
////    }
////    manager.start
////  }
////}
////
object trainAndEvaluateBracketedByIter {
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
    val maxIter = args(5).toInt
    val randSeed = args(6).toInt

    val wordScale = 10000

    val initGram = new ShakesPCNF

    val termList =
      fromFile( termFile ).getLines.toList.filter( _.length > 0 ).
      map (_.replace("\n",""))

    //initGram.readGrammar(gramFile)
    //initGram.readLexicon(lexFile)

    initGram.randomizeGrammar( nonTermCount, termList, randSeed, 100 )

    val trainingCorpus = new BracketedCorpus
    trainingCorpus.readCorpus( trainYieldSpec )

    println(trainingCorpus)

    val testStrings = fromFile(testStringsPath).getLines.toList.filter(_.length >
      0).map(_.replace("\n","")).toList


    object manager extends ShakesParserManager( initGram ) {
      import java.io._

      val trainCorpus = trainingCorpus

      object VitActor extends EvaluationActor(initGram,10000) {
        var testCorpus = testStrings
      }

      VitActor.start

      def useGrammar( trainedGram: ShakesPCNF, iterNum:Int) {
        VitActor ! Tuple2(g1.copy, iterNum)
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
        VitActor ! Tuple2(g1.copy,Stop)
        //VitActor ! 
      }
    }

    manager.start
  }
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


    object manager extends ShakesParserManager( initGram ) {
      import java.io._

      val trainCorpus = trainingCorpus


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

        if( iterNum == 1 )
          println( trainedGram )

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
      }
    }

    manager.start
  }
}

////object trainAndEvaluateVanillaByIterAndReadGrammar {
////  def main( args: Array[String] ) {
////    import scala.actors.Actor
////    import scala.actors.Actor._
////    import collection.mutable.ArrayBuffer
////    import Math._
////    import scala.io.Source._
////    import scala.actors.Exit
////
////    val gramFile = args(0)
////    val lexFile = args(1)
////    val trainYieldPath = args(2)
////    val testStringsPath = args(3)
////    val numParsers = args(4).toInt
////    val maxIter = args(5).toInt
////    //val randSeed = args(6).toInt
////
////    val wordScale = 10000
////
////    val initGram = new ShakesPCNF
////
////    //val termList =
////    //  fromFile( termFile ).getLines.toList.filter( _.length > 0 ).
////    //  map (_.replace("\n",""))
////
////    initGram.readGrammar(gramFile)
////    initGram.readLexicon(lexFile)
////
////    //initGram.randomizeGrammar( nonTermCount, termList, randSeed, 100 )
////
////    //println(initGram)
////
////    val trainingCorpus = new StringsOnlyCorpus
////    trainingCorpus.readCorpus( trainYieldPath )
////
////    val testStrings = fromFile(testStringsPath).getLines.toList.filter(_.length >
////      0).map(_.replace("\n",""))
////
////
////    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
////      import java.io._
////
////      object VitActor extends EvaluationActor(initGram,10000) {
////        var testCorpus = testStrings
////      }
////
////      VitActor.start
////
////      def useGrammar( trainedGram: ShakesPCNF) {
////        VitActor ! g1
////      }
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
////      def stoppingCondition( numIters:Int, x:Double ) = 
////        numIters >= maxIter
////
////      def cleanup = {
////        VitActor ! Stop 
////        VitActor ! g1
////      }
////    }
////
////
////    manager.start
////  }
////}
////
////object trainAndEvaluateVanillaByIter {
////  def main( args: Array[String] ) {
////    import scala.actors.Actor
////    import scala.actors.Actor._
////    import collection.mutable.ArrayBuffer
////    import Math._
////    import scala.io.Source._
////    import scala.actors.Exit
////
////    val nonTermCount = args(0).toInt
////    val termFile = args(1)
////    val trainYieldPath = args(2)
////    val testStringsPath = args(3)
////    val numParsers = args(4).toInt
////    val maxIter = args(5).toInt
////    val randSeed = args(6).toInt
////
////    val wordScale = 10000
////
////    val initGram = new ShakesPCNF
////
////    val termList =
////      fromFile( termFile ).getLines.toList.filter( _.length > 0 ).
////      map (_.replace("\n",""))
////
////    //initGram.readGrammar(gramFile)
////    //initGram.readLexicon(lexFile)
////
////    initGram.randomizeGrammar( nonTermCount, termList, randSeed, 100 )
////
////    //println(initGram)
////
////    val trainingCorpus = new StringsOnlyCorpus
////    trainingCorpus.readCorpus( trainYieldPath )
////
////    val testStrings = fromFile(testStringsPath).getLines.toList.filter(_.length >
////      0).map(_.replace("\n",""))
////
////
////    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
////      import java.io._
////
////      object VitActor extends EvaluationActor(initGram,10000) {
////        var testCorpus = testStrings
////      }
////
////      VitActor.start
////
////      def useGrammar( trainedGram: ShakesPCNF) {
////        VitActor ! g1
////      }
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
////      def stoppingCondition( numIters:Int, x:Double ) = 
////        numIters >= maxIter
////
////      def cleanup = {
////        VitActor ! Stop 
////        VitActor ! g1
////      }
////    }
////
////
////    manager.start
////  }
////}


//object trainAndEvaluateBracketedByIter extends evaluator {
//  import scala.actors.Actor
//  import scala.actors.Actor._
//
//
//  def main( args:Array[String] ) {
//    import scala.io.Source._
//    import Math._
//    val gramFile = args(0)
//    val lexFile = args(1)
//    val trainYieldFile = args(2)
//    val trainBracketFile = args(3)
//    val testYieldFile = args(4)
//    val numParsers = args(5).toInt
//    maxIter = args(6).toInt
//    //var gramFilePrefix = args(7)
//    var wordScale = args(7).toInt
//
//    val initialGrammar = new ShakesPCNF
//
//    initialGrammar.readGrammar( gramFile )
//    initialGrammar.readLexicon( lexFile )
//
//
//    testCorpus = fromFile( testYieldFile ).getLines.toList.map( s =>
//      s.replace("\n","")
//    )
//
//    def iterCompleted( numIter:Int, x:Double ) =
//      numIter >= maxIter
//
//
//
//    val manager = new ShakesBracketedParserManager(
//      trainYieldFile, trainBracketFile, initialGrammar, numParsers,
//      wordScale) ( iterCompleted ) ( evaluate )
//
//    VitActor.start
//    manager.start
//  }
//}
//
//object trainAndEvaluateBracketed {
//  def main( args:Array[String] ) {
//    import scala.io.Source._
//    import Math._
//    val gramFile = args(0)
//    val lexFile = args(1)
//    val trainYieldFile = args(2)
//    val trainBracketFile = args(3)
//    val testYieldFile = args(4)
//    val numParsers = args(5).toInt
//    val tolerance = args(6).toDouble
//    var gramFilePrefix = args(7)
//    var wordScale = args(8).toInt
//
//    val g = new ShakesPCNF
//
//    g.readGrammar( gramFile )
//    g.readLexicon( lexFile )
//
//    /*
//    val trainCorpus = fromFile( trainYieldFile ).getLines.toList.map( s =>
//      s.replace("\n","")
//    )
//    */
//
//    val testCorpus = fromFile( testYieldFile ).getLines.toList.map( s =>
//      s.replace("\n","")
//    )
//
//    def underTolerance( n:Int, deltaLogProb:Double ) =
//      abs(deltaLogProb) <= tolerance
//
//    def evaluate ( trainedGram: ShakesPCNF ) {
//      val vit = new ShakesViterbiParser( trainedGram, wordScale )
//      testCorpus foreach { testSentence =>
//        val words = testSentence.split(' ')
//        vit.clear
//        vit.resize( words.size + 1 )
//        vit.populateChart( words )
//        println( vit.parseString )
//      }
//    }
//
//    val manager = new ShakesBracketedParserManager(
//      trainYieldFile, trainBracketFile, g, numParsers,
//      gramFilePrefix, wordScale) ( underTolerance ) ( evaluate )
//
//    manager.start
//  }
//}
//
//object trainAndEvaluateVanillaByIter {
//  def main( args:Array[String] ) {
//    import scala.io.Source._
//    import Math._
//    val gramFile = args(0)
//    val lexFile = args(1)
//    val trainYieldFile = args(2)
//    val testYieldFile = args(3)
//    val numParsers = args(4).toInt
//    val maxIter = args(5).toInt
//    var gramFilePrefix = args(6)
//    var wordScale = args(7).toInt
//
//    val g = new ShakesPCNF
//
//    g.readGrammar( gramFile )
//    g.readLexicon( lexFile )
//
//    val trainCorpus = fromFile( trainYieldFile ).getLines.toList.map( s =>
//      s.replace("\n","")
//    )
//
//    val testCorpus = fromFile( testYieldFile ).getLines.toList.map( s =>
//      s.replace("\n","")
//    )
//
//    def iterCompleted( numIter:Int, x:Double ) =
//      numIter >= maxIter
//      //abs(deltaLogProb) <= tolerance
//
//    def evaluate ( trainedGram: ShakesPCNF ) {
//      val vit = new ShakesViterbiParser( trainedGram, wordScale )
//      testCorpus foreach { testSentence =>
//        val words = testSentence.split(' ')
//        vit.clear
//        vit.resize( words.size + 1 )
//        vit.populateChart( words )
//        println( vit.parseString )
//      }
//    }
//
//    val manager = new ShakesVanillaParserManager( 
//      trainCorpus, g, numParsers,
//      gramFilePrefix, wordScale) ( iterCompleted ) ( evaluate )
//
//    manager.start
//  }
//}
//
//object trainAndEvaluateVanilla {
//  def main( args:Array[String] ) {
//    import scala.io.Source._
//    import Math._
//    val gramFile = args(0)
//    val lexFile = args(1)
//    val trainYieldFile = args(2)
//    val testYieldFile = args(3)
//    val numParsers = args(4).toInt
//    val tolerance = args(5).toDouble
//    var gramFilePrefix = args(6)
//    var wordScale = args(7).toInt
//
//    val g = new ShakesPCNF
//
//    g.readGrammar( gramFile )
//    g.readLexicon( lexFile )
//
//    val trainCorpus = fromFile( trainYieldFile ).getLines.toList.map( s =>
//      s.replace("\n","")
//    )
//
//    val testCorpus = fromFile( testYieldFile ).getLines.toList.map( s =>
//      s.replace("\n","")
//    )
//
//    def underTolerance( n:Int, deltaLogProb:Double ) =
//      abs(deltaLogProb) <= tolerance
//
//    def evaluate ( trainedGram: ShakesPCNF ) {
//      val vit = new ShakesViterbiParser( trainedGram, wordScale )
//      testCorpus foreach { testSentence =>
//        val words = testSentence.split(' ')
//        vit.clear
//        vit.resize( words.size + 1 )
//        vit.populateChart( words )
//        println( vit.parseString )
//      }
//    }
//
//    val manager = new ShakesVanillaParserManager( 
//      trainCorpus, g, numParsers,
//      gramFilePrefix, wordScale) ( underTolerance ) ( evaluate )
//
//    manager.start
//  }
//}
//
//
//object runViterbi {
//  def main(args:Array[String]) {
//    import scala.io.Source._
//    val gramFile = args(0)
//    val lexFile = args(1)
//    val stringsFile = args(2)
//
//    val g = new ShakesPCNF
//
//    g.readGrammar(gramFile)
//    g.readLexicon(lexFile)
//
//    val sentences = fromFile(stringsFile).getLines.toList.map{ s =>
//      s.replace("\n","")
//    }
//
//    val vit = new ShakesViterbiParser(g,100)
//
//    sentences foreach( s =>
//      {
//        val words = s.split(' ')
//        vit.clear
//        vit.resize( words.size + 1 )
//        vit.populateChart( words )
//        //println( vit.stringProb + "\t" + vit.parseString )
//        println( vit.parseString )
//      }
//    )
//
//
//    ()
//  }
//}
//
//
//object bracketedEMByIter {
//  def main(args:Array[String]) {
//    import scala.io.Source._
//    val gramFile = args(0)
//    val lexFile = args(1)
//    val bracketsFile = args(2)
//    val stringsFile = args(3)
//    val numParsers = args(4).toInt
//    val numIterations = args(5).toInt
//    var gramFilePrefix = "ShakesEMGram"
//    if( args.size > 6 )
//      gramFilePrefix = args(6)
//    var wordScale = args(7).toInt
//
//    val g = new ShakesPCNF
//
//    g.readGrammar(gramFile)
//    g.readLexicon(lexFile)
//
//    val em = new
//    ShakesBracketedIterationManager(stringsFile,bracketsFile,g,numParsers,numIterations,gramFilePrefix,wordScale)
//
//    ()
//  }
//}
//
//object bracketedEM {
//  def main(args:Array[String]) {
//    import scala.io.Source._
//    val gramFile = args(0)
//    val lexFile = args(1)
//    val bracketsFile = args(2)
//    val stringsFile = args(3)
//    val numParsers = args(4).toInt
//    val tolerance = args(5).toDouble
//    var gramFilePrefix = "ShakesEMGram"
//    if( args.size > 6 )
//      gramFilePrefix = args(6)
//    var wordScale = args(7).toInt
//
//    val g = new ShakesPCNF
//
//    g.readGrammar(gramFile)
//    g.readLexicon(lexFile)
//
//    val em = new
//    ShakesBracketedConvergenceManager(stringsFile,bracketsFile,g,numParsers,tolerance,gramFilePrefix,wordScale)
//
//    ()
//  }
//}
//
//object vanillaEMByIter {
//  def main(args:Array[String]) {
//    import scala.io.Source._
//    val gramFile = args(0)
//    val lexFile = args(1)
//    val yieldFile = args(2)
//    val numParsers = args(3).toInt
//    val numIter = args(4).toInt
//    var gramFilePrefix = "ShakesEMGram"
//    if( args.size > 5 )
//      gramFilePrefix = args(5)
//    var wordScale = args(6).toInt
//
//    val g = new ShakesPCNF
//
//    g.readGrammar(gramFile)
//    g.readLexicon(lexFile)
//
//    val sentences = fromFile(yieldFile).getLines.toList.map{ s =>
//      s.replace("\n","")
//    }
//
//    val em = new
//    ShakesIterationManager(sentences,g,numParsers,numIter,gramFilePrefix,wordScale)
//
//    ()
//  }
//}
//
//
//object vanillaEM {
//  def main(args:Array[String]) {
//    import scala.io.Source._
//    val gramFile = args(0)
//    val lexFile = args(1)
//    val yieldFile = args(2)
//    val numParsers = args(3).toInt
//    val tolerance = args(4).toDouble
//    var gramFilePrefix = "ShakesEMGram"
//    if( args.size > 5 )
//      gramFilePrefix = args(5)
//    var wordScale = args(6).toInt
//
//    val g = new ShakesPCNF
//
//    g.readGrammar(gramFile)
//    g.readLexicon(lexFile)
//
//    val sentences = fromFile(yieldFile).getLines.toList.map{ s =>
//      s.replace("\n","")
//    }
//
//    val em = new
//    ShakesConvergenceManager(sentences,g,numParsers,tolerance,gramFilePrefix,wordScale)
//
//    ()
//  }
//}
//
