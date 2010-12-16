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

package ShakesEM.types.corpora

import ShakesEM.types.strings._
import collection.immutable.{HashMap => IHashMap,HashSet => IHashSet}
import collection.mutable.{HashMap => MHashMap, HashSet => MHashSet}

/**
* Training corpus classes. Apply should return whatever counts as one sentence
* (whether it's a string or a tuple of a string and a bracketing), size should
* give the number of sentences, and readCorpus should read in the corpus from
* file(s)
*/
trait ShakesTrainCorpus {
  def apply(n:Int):ToParse
  def size:Int
  def readCorpus(pathSpecifier:String)
  def toList:List[ToParse]
}

/**
* Reads, stores, and makes useful a partially bracketed corpus.
*/
class BracketedCorpus extends ShakesTrainCorpus {
  var bracketings:Array[MHashSet[Bracketing]] = Array()
  var strings:Array[String] = Array()

  /**
  * Reads in strings of terminals from <code>stringsPath</code> and the
  * brackets from <code>bracketsPath</code>. stringsPath should point to a
  * file with one sentence per line. bracketsPath should point to a file that
  * specifies the partial bracketing with each span specified in the following
  * format:
  * <br /><br />
  * <code>sentence-id span-start-index span-end-index</code>
  * <br /><br />
  * Where <code>sentence-id</code> is the zero-based index of the sentence in
  * the stringsPath. Basically, I think it is just easier to get a perl script
  * to take a Penn-Treebank style corpus and produce these files than to do it
  * in Scala.
  *
  * @param stringsPath File path to the strings we're trying to induce a
  * grammar from.
  * @param bracketsPath File path to the corresponding partial bracketing.
  */
  def readCorpus(corporaPrefix:String) {
    import scala.io.Source._

    val stringsPath = corporaPrefix+"-strings.txt"
    val bracketsPath = corporaPrefix+"-brackets.txt"


    strings = io.Source.fromFile(stringsPath).getLines().toList.filter(_.length > 0).map(_.replace("\n","")).toArray

    val corpusSize = strings.size

    bracketings = Array.fill(corpusSize + 1 )(
      new MHashSet[Bracketing]
    )

    val lines = fromFile(bracketsPath).getLines()//("\n")
    lines.foreach( line =>
      if( line.length > 1 ) {
        val fields = line.replace("\n","").split(' ')
        
        val (start,end) = ( fields(1).toInt, fields(2).toInt )
        if( end - start > 1 )
          bracketings( fields(0).toInt ) +=
            Bracketing( fields(1).toInt, fields(2).toInt )
      }
    )
  }
  def apply(n:Int) = BracketedToParse(strings(n),bracketings(n))
  def size = strings.size
  override def toString = strings mkString("","\n","")
  def toList = ((0 to (strings.size-1)) map{ index =>
    BracketedToParse( strings( index ), bracketings( index ) )
  }).toList
}

/**
* Reads, stores, and makes useful a corpus with only strings
*/
class StringsOnlyCorpus extends ShakesTrainCorpus {
  var strings:Array[String] = Array()
  def apply(n:Int) = StringToParse( strings( n ) )
  def size = strings.size

  def readCorpus( stringsPath:String ) {
    import scala.io.Source._
    strings = fromFile(stringsPath).getLines().toList.filter(_.length > 0).map(_.replace("\n","")).toArray
  }
  override def toString = strings mkString("","\n","")

  def toList = ((0 to (strings.size-1)) map{ index =>
    StringToParse( strings( index ) )
  }).toList
}

