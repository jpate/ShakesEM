import ShakesEM._


object trainVanillaByIter {
  def main( args: Array[String] ) {
    import scala.collection.mutable.ArrayBuffer
    import Math._
    import scala.io.Source

    var gramFile = args(0)
    var lexFile = args(1)
    var trainYieldFile = args(2)
    var numParsers = args(3).toInt
    var maxIter = args(4).toInt

    val wordScale = 10000

    val initGram = new ShakesPCNF

    initGram.readGrammar(gramFile)
    initGram.readLexicon(lexFile)

    val trainingCorpus = new StringsOnlyCorpus

    trainingCorpus.readCorpus( trainYieldFile )

    //println(initGram)
    //println(trainingCorpus)

    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
      def stoppingCondition( numIters:Int, x:Double ) = 
        numIters >= maxIter

      def parserConstructor = {
        val someParsers = new ArrayBuffer[ShakesDistributedParser]

        (0 to (numParsers-1) ) foreach(
          someParsers += new ShakesEstimatingParser( _, g1, wordScale
          )
        )
        someParsers
      }

      def useGrammar( trainedGram: ShakesPCNF) { }
      def cleanup = ()
    }

    manager.start
  }
}

object trainAndPrintVanillaByIter {
  def main( args: Array[String] ) {
    import scala.collection.mutable.ArrayBuffer
    import Math._
    import scala.io.Source

    var gramFile = args(0)
    var lexFile = args(1)
    var trainYieldFile = args(2)
    var numParsers = args(3).toInt
    var maxIter = args(4).toInt

    val wordScale = 10000

    val initGram = new ShakesPCNF

    initGram.readGrammar(gramFile)
    initGram.readLexicon(lexFile)

    val trainingCorpus = new StringsOnlyCorpus

    trainingCorpus.readCorpus( trainYieldFile )

    //println(initGram)
    //println(trainingCorpus)

    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
      def stoppingCondition( numIters:Int, x:Double ) = 
        numIters >= maxIter

      def parserConstructor = {
        val someParsers = new ArrayBuffer[ShakesDistributedParser]

        (0 to (numParsers-1) ) foreach(
          someParsers += new ShakesEstimatingParser( _, g1, wordScale
          )
        )
        someParsers
      }

      def useGrammar( trainedGram: ShakesPCNF) { println(trainedGram) }
      def cleanup = ()
    }

    manager.start
  }
}

object trainAndPrintBracketedByIter {
  def main( args: Array[String] ) {
    import collection.mutable.ArrayBuffer
    import Math._
    import scala.io.Source

    var gramFile = args(0)
    var lexFile = args(1)
    var trainYieldSpec = args(2)
    var numParsers = args(3).toInt
    var maxIter = args(4).toInt

    val wordScale = 10000

    val initGram = new ShakesPCNF

    initGram.readGrammar(gramFile)
    initGram.readLexicon(lexFile)

    val trainingCorpus = new BracketedCorpus

    trainingCorpus.readCorpus( trainYieldSpec )

    //println(initGram)
    //println(trainingCorpus)

    object manager extends ShakesParserManager( initGram, trainingCorpus ) {

      def stoppingCondition( numIters:Int, x:Double ) = 
        numIters >= maxIter

      def parserConstructor = {
        val someParsers = new ArrayBuffer[ShakesDistributedParser]

        (0 to (numParsers-1) ) foreach(
          someParsers += new ShakesBracketedParser( _, g1, wordScale
          )
        )
        someParsers
      }
      //def parserConstructor(id:Int) = new ShakesEstimatingParser( id, g1, wordScale )


      def useGrammar( trainedGram: ShakesPCNF) { println(trainedGram) }
      def cleanup = ()
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

    val gramFile = args(0)
    val lexFile = args(1)
    val trainYieldSpec = args(2)
    val testStringsPath = args(3)
    val numParsers = args(4).toInt
    val tolerance = args(5).toDouble
    val gramFilePrefix = args(6)

    val wordScale = 10000

    val initGram = new ShakesPCNF

    initGram.readGrammar(gramFile)
    initGram.readLexicon(lexFile)

    val trainingCorpus = new BracketedCorpus
    trainingCorpus.readCorpus( trainYieldSpec )

    val testCorpus = fromFile(testStringsPath).getLines.toList.filter(_.length >
      0).map(_.replace("\n","")).toArray


    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
      import java.io._
      object VitActor
        extends ShakesViterbiParser(initGram,10000) with Actor {
        var iterNum = 0
        def act = {
          while(true) {
            receive {
              case intermediateGram:ShakesPCNF =>
                if( iterNum % 2 == 0 )
                {
                  g = intermediateGram
                  testCorpus.foreach{ testSentence =>
                    val words = testSentence.split(' ')
                    clear
                    resize( words.size + 1 )
                    populateChart( words )
                    println("Iter" + iterNum + ": " + parseString )
                  }
                }
              case Stop => {
                println("VitActor exiting.")
                exit()
              }
            }
            iterNum = iterNum + 1
          }
        }
      }
      VitActor.start

      def useGrammar( trainedGram: ShakesPCNF) {
        VitActor ! g1

        val bw = new BufferedWriter(new
        FileWriter(gramFilePrefix+"Iter"+iterationNum,false));
        bw.write(
          "Corpus log probability: " +
          corpusLogProb +
          "\nCorpus probability: " +
          exp(corpusLogProb) +
          "\nDelta LogProb: " +
          deltaLogProb +
          "\n\n" +
          g1.toString
        );
        bw.close();
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
        deltaLogProb < tolerance
      def cleanup = { VitActor ! Stop }
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

    val gramFile = args(0)
    val lexFile = args(1)
    val trainYieldPath = args(2)
    val testStringsPath = args(3)
    val numParsers = args(4).toInt
    val tolerance = args(5).toDouble
    val gramFilePrefix = args(6)

    val wordScale = 10000

    val initGram = new ShakesPCNF

    initGram.readGrammar(gramFile)
    initGram.readLexicon(lexFile)

    val trainingCorpus = new StringsOnlyCorpus
    trainingCorpus.readCorpus( trainYieldPath )

    val testCorpus = fromFile(testStringsPath).getLines.toList.filter(_.length >
      0).map(_.replace("\n","")).toArray


    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
      import java.io._

      object VitActor
        extends ShakesViterbiParser(initGram,10000) with Actor {
        var iterNum = 0
        def act = {
          while(true) {
            receive {
              case intermediateGram:ShakesPCNF =>
                if( iterNum % 2 == 0 )
                {
                  g = intermediateGram
                  testCorpus.foreach{ testSentence =>
                    val words = testSentence.split(' ')
                    clear
                    resize( words.size + 1 )
                    populateChart( words )
                    println("Iter" + iterNum + ": " + parseString )
                  }
                }
              case Stop => {
                println("VitActor exiting.")
                exit()
              }
            }
            iterNum = iterNum + 1
          }
        }
      }
      VitActor.start

      def useGrammar( trainedGram: ShakesPCNF) {
        VitActor ! g1

        val bw = new BufferedWriter(new
        FileWriter(gramFilePrefix+"Iter"+iterationNum,false));
        bw.write(
          "Corpus log probability: " +
          corpusLogProb +
          "\nCorpus probability: " +
          exp(corpusLogProb) +
          "\nDelta LogProb: " +
          deltaLogProb +
          "\n\n" +
          g1.toString
        );
        bw.close();
        //println( VitActor.parseString )
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
        deltaLogProb < tolerance
      def cleanup = { VitActor ! Stop }
    }
    manager.start
  }
}

object trainAndEvaluateBracketedByIter {
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
    val gramFilePrefix = args(6)

    val wordScale = 10000

    val initGram = new ShakesPCNF

    initGram.readGrammar(gramFile)
    initGram.readLexicon(lexFile)

    val trainingCorpus = new BracketedCorpus
    trainingCorpus.readCorpus( trainYieldSpec )

    val testCorpus = fromFile(testStringsPath).getLines.toList.filter(_.length >
      0).map(_.replace("\n","")).toArray


    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
      import java.io._
      object VitActor
        extends ShakesViterbiParser(initGram,10000) with Actor {
        var iterNum = 0
        def act = {
          while(true) {
            receive {
              case intermediateGram:ShakesPCNF =>
                if( iterNum % 2 == 0 )
                {
                  g = intermediateGram
                  testCorpus.foreach{ testSentence =>
                    val words = testSentence.split(' ')
                    clear
                    resize( words.size + 1 )
                    populateChart( words )
                    println("Iter" + iterNum + ": " + parseString )
                  }
                }
              case Stop => {
                println("VitActor exiting.")
                exit()
              }
            }
            iterNum = iterNum + 1
          }
        }
      }
      VitActor.start

      def useGrammar( trainedGram: ShakesPCNF) {
        VitActor ! g1
        val bw = new BufferedWriter(new
        FileWriter(gramFilePrefix+"Iter"+iterationNum,false));
        bw.write(
          "Corpus log probability: " +
          corpusLogProb +
          "\nCorpus probability: " +
          exp(corpusLogProb) +
          "\nDelta LogProb: " +
          deltaLogProb +
          "\n\n" +
          g1.toString
        );
        bw.close();
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
      def cleanup = { VitActor ! Stop }
    }

    manager.start
  }
}

object trainAndEvaluateVanillaByIter {
  def main( args: Array[String] ) {
    import scala.actors.Actor
    import scala.actors.Actor._
    import collection.mutable.ArrayBuffer
    import Math._
    import scala.io.Source._
    import scala.actors.Exit

    val gramFile = args(0)
    val lexFile = args(1)
    val trainYieldPath = args(2)
    val testStringsPath = args(3)
    val numParsers = args(4).toInt
    val maxIter = args(5).toInt
    val gramFilePrefix = args(6)

    val wordScale = 10000

    val initGram = new ShakesPCNF

    initGram.readGrammar(gramFile)
    initGram.readLexicon(lexFile)

    val trainingCorpus = new StringsOnlyCorpus
    trainingCorpus.readCorpus( trainYieldPath )

    val testCorpus = fromFile(testStringsPath).getLines.toList.filter(_.length >
      0).map(_.replace("\n","")).toArray

    object VitActor
      extends ShakesViterbiParser(initGram,10000) with Actor {
      var iterNum = 0
      def act = {
        trapExit = true
        while(true) {
          receive {
            case intermediateGram:ShakesPCNF =>
              if( iterNum % 2 == 0 )
              {
                g = intermediateGram
                testCorpus.foreach{ testSentence =>
                  val words = testSentence.split(' ')
                  clear
                  resize( words.size + 1 )
                  populateChart( words )
                  println("Iter" + iterNum + ": " + parseString )
                  //println(g)
                }


              }
            case Stop => {
              println("VitActor exiting.")
              exit()
            }
          }
          iterNum = iterNum + 1
        }
      }
    }

    object manager extends ShakesParserManager( initGram, trainingCorpus ) {
      import java.io._

      VitActor.start

      def useGrammar( trainedGram: ShakesPCNF) {
        VitActor ! g1

        val bw = new BufferedWriter(new
        FileWriter(gramFilePrefix+"Iter"+iterationNum,false));
        bw.write(
          "Corpus log probability: " +
          corpusLogProb +
          "\nCorpus probability: " +
          exp(corpusLogProb) +
          "\nDelta LogProb: " +
          deltaLogProb +
          "\n\n" +
          g1.toString
        );
        bw.close();
        //println( VitActor.parseString )
      }

      def parserConstructor = {
        val someParsers = new ArrayBuffer[ShakesDistributedParser]

        (0 to (numParsers-1) ) foreach(
          someParsers += new ShakesEstimatingParser( _, g1, wordScale
          )
        )
        someParsers
      }

      def stoppingCondition( numIters:Int, x:Double ) = 
        numIters >= maxIter

      def cleanup = { VitActor ! Stop }
    }


    manager.start
  }
}


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
