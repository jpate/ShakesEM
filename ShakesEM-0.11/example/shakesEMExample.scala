import ShakesEM._

object shakesEMExample {
  def main(args:Array[String]) {
    import scala.io.Source._
    val gramFile = args(0)
    val lexFile = args(1)
    val yieldFile = args(2)
    val numParsers = args(3).toInt
    val tolerance = args(4).toDouble
    var gramFilePrefix = "ShakesEMGram"
    if( args.size > 5 )
      gramFilePrefix = args(5)

    val g = new ShakesPCNF

    g.readGrammar(gramFile)
    g.readLexicon(lexFile)

    val sentences = fromFile(yieldFile).getLines.toList.map{ s =>
      s.replace("\n","")
    }

    val em = new
    ShakesConvergenceManager(sentences,g,numParsers,tolerance,gramFilePrefix)

    ()
  }
}

