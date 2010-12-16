/**
* ShakesEM is a package for running EM for Probabilistic Context Free Grammars,
* implemented in Scala using the Actors library to allow parallel processing
* over multiple machines and multiple cores. This version implements both
* vanilla EM and the modification to EM in Pereira and Schabes (1992) for
* partially bracketed corpora
*
* Copyright 2010 John K Pate
* Distributed under the GNU General Public License
* 
*
* This program is free software: you can redistribute it and/or modify it under
* the terms of the GNU General Public License as published by the Free Software
* Foundation, either version 3 of the License, or (at your option) any later
* version. This program is distributed in the hope that it will be useful, but
* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
* details. You should have received a copy of the GNU General Public License along
* with this program. If not, see <http://www.gnu.org/licenses/>.
*
*
* @version 0.21a
* @author John K Pate
*/

package ShakesEM.grammars

import collection.immutable.{HashMap => IHashMap,HashSet => IHashSet}
import collection.mutable.{HashMap => MHashMap, HashSet => MHashSet}
import ShakesEM.types.expansions._

/**
* <code>ShakesPCNF</code> defines a Probabilistic Chomsky Normal Form grammar
* for use with the ShakesEM library
*/
@serializable class ShakesPCNF {
  import math._


  /**
  * Used in re-estimation of the PCFG. f Sums estimated counts for binary
  * branching nodes, g for unary branching nodes, and h for any non-terminal
  * node.
  * See Manning &amp; Schutze p. 396.
  * The defaults just make the incrementation code cleaner.
  */
  val f = new MHashMap[NTExpansion,Double] {
    override def default(exp:NTExpansion) = 0D
  }
  val g = new MHashMap[TermExpansion,Double] {
    override def default(key:TermExpansion) = 0D
  }
  val h = new MHashMap[String,Double] {
    override def default(key:String) = 0D
  }

  def reestimateRules( p:(Double => Double) ) {
    f.keysIterator.foreach{ exp =>
      val NTExpansion(lhs,_,_) = exp
        phrases( exp ) =
          p( 100000 * f(exp) ) /
          p( h (lhs) )
    }



    g.keysIterator.foreach{ exp =>
      val TermExpansion( pos, _ ) = exp
      lexicon ( exp ) =
         p( 100000 * g( exp ) ) /
         p( h(pos) )
    }

    f.clear
    g.clear
    h.clear
    normalize
    preCalcExps
  }



  def normalize {

    val ntTotals = new MHashMap[String,Double] {
      override def default( lhs:String ) = 0D
    }

    phrases.keysIterator.foreach{ exp =>
      val NTExpansion( lhs, _, _ ) = exp
      ntTotals ( lhs ) += phrases( exp )
    }
    phrases.keysIterator.foreach{ exp =>
      val NTExpansion( lhs, _, _ ) = exp
      phrases ( exp ) = phrases( exp ) / ntTotals(lhs)
    }

    ntTotals.clear

    lexicon.keysIterator.foreach{ exp =>
      val TermExpansion( pos, _ ) = exp
      ntTotals( pos ) += lexicon( exp )
    }
    lexicon.keysIterator.foreach{ exp =>
      val TermExpansion( pos, _ ) = exp
      lexicon ( exp ) = lexicon( exp ) / ntTotals(pos)
    }
  }


  def randomizeGrammar( nonTermCount:Int, termSymbols:List[String],
                        randSeed:Int, centeredOn:Int) {
    import scala.util.Random

    val nonTermSymbols = "S" :: (
       ( (1 to nonTermCount - 1) toList ) map ( "N" + _ )
    )

    val r = new Random( randSeed )

    val allSymbols:List[String] = nonTermSymbols ::: termSymbols

    for( lhs <- nonTermSymbols ) 
      for( left <- allSymbols )
        for( right <- allSymbols )
          //phrases(lhs)(left)(right) = r.nextDouble + centeredOn
          phrases( NTExpansion( lhs, left, right ) ) = r.nextDouble + centeredOn


    for( pos <- nonTermSymbols )
      for( word <- termSymbols )
        lexicon( TermExpansion( pos, word ) ) = r.nextDouble + centeredOn

    normalize
    preCalcExps
  }



  var lexicon = new MHashMap[TermExpansion,Double] {
    override def default( exp: TermExpansion ) = 0D
  }

    
  var lexExps = new MHashMap[String,List[NTRevParent]] {
    override def default( word:String ) = Nil
  }
  

  var phrases = new MHashMap[ NTExpansion, Double] { 
    override def default(exp:NTExpansion) = 0D
  }

  var phrExps = new MHashMap[ NTRevChild, List[NTRevParent] ] {
    override def default( key: NTRevChild ) = Nil
  }

  /**
  * Reads a grammar from a file. Grammar should be in the format:
  * <br/><br/>
  * <code>parent left-child right-child probability</code>
  * <br/><br/>
  * and contain only binary rules.
  * 
  * @param gramPath File path to the grammar (relative to current working
  * directory)
  */
  def readGrammar(gramPath:String) {
    import scala.io.Source._
    val lines = fromFile(gramPath).getLines()
    lines.foreach( line =>
      if( line.length > 1 ) {
        val fields = line.split(' ')

        phrases (NTExpansion( fields(0), fields(1), fields(2))) += fields(3).toDouble
        phrExps ( NTRevChild( fields(1), fields(2)) ) = NTRevParent(fields(0), fields(3).toDouble) ::
          phrExps( NTRevChild(fields(1),fields(2)) )
      }
    )
  }

  /**
  * Reads a lexicon from a file. Lexicon should be in the format:
  * <br/><br/>
  * <code>part-of-speech word probability</code>
  * <br/><br/>
  * and contain only unary rules.
  * 
  * @param gramPath File path to the grammar (relative to current working
  * directory)
  */
  def readLexicon(lexPath:String) {
    import scala.io.Source._
    val lines = fromFile(lexPath).getLines()
    lines.foreach( line =>
      if( line.length > 1 ) {
        val fields = line.split(' ')
        lexicon( TermExpansion(fields(0), fields(1)) ) += fields(2).toDouble
        lexExps(fields(1)) = NTRevParent( fields(0), fields(2).toDouble ) ::
                              lexExps(fields(1))

      }
    )
  }

  
  /**
  * Computes the value of phrExps on the basis of phrases and the value of
  * lexExps on the basis of lexicon. Basically, keep these values in two forms
  * to save computation time at the expense of space.
  */
  def preCalcExps {
    phrExps.clear
    lexExps.clear

    phrExps.clear
    phrases.keysIterator.foreach{ exp =>
      val NTExpansion( lhs, left, right ) = exp
      phrExps( NTRevChild(left, right) ) =
        NTRevParent(lhs, phrases( exp ))::
        phrExps( NTRevChild(left, right) )
    }

    lexicon.keysIterator.foreach{ exp =>
      val TermExpansion( pos, word ) = exp
      lexExps(word) = NTRevParent(pos, lexicon( TermExpansion(pos,word))) ::
        lexExps(word)
    }
  }

  /**
  * Returns a ShakesPCNF which has the same lexicon, phrases, lexExps, and
  * phrExps but zero counts
  *
  * @return A ShakesPCNF with the same lexicon, phrases, lexExps, and phrExps
  * as the ShakesPCNF calling the function, but with all zero counts.
  */
  def countlessCopy:ShakesPCNF = {
    val copy = new ShakesPCNF
    phrases.keysIterator.foreach( exp =>
      copy.phrases( exp ) +=  0.0
    )
    lexicon.keysIterator.foreach{ exp =>
      val TermExpansion( pos, word ) = exp
      copy.lexicon( TermExpansion(pos, word)) += 0.0
    }
    copy.preCalcExps
    copy
  }
  
  def copy:ShakesPCNF = {
    val copy = new ShakesPCNF
    copy.phrases = phrases
    copy.lexicon = lexicon
    copy.phrExps = phrExps
    copy.lexExps = lexExps
    copy
  }


  /**
  * Produce a readable stringification of lexicon and phrases in the same
  * format that readGrammar and readLexicon expect.
  * @return A string representation of lexicon and phrases.
  */
  override def toString =
    phrases.keysIterator.map{ exp =>
      val NTExpansion( lhs, left, right ) = exp
      lhs + " " + left + " " + right + " " + phrases( exp )
    }.mkString("Phrases:\n\t","\n\t","\n") +
    lexicon.keysIterator.map{ exp =>
      val TermExpansion( pos, word ) = exp
      pos + " " + word + " " +  " " + lexicon( exp )
    }.mkString("Lexicon:\n\t","\n\t","")
}

