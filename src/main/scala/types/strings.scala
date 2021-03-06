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

package ShakesEM.types.strings

import collection.immutable.{HashMap => IHashMap,HashSet => IHashSet}
import collection.mutable.{HashMap => MHashMap, HashSet => MHashSet}

case class Bracketing(leftSpanPoint:Int,rightSpanPoint:Int)

abstract class ToParse {
  def size:Int
}

case class BracketedToParse(s:String,b:MHashSet[Bracketing]) extends ToParse {
  def words = s.split(' ')
  def size = words.size
}

case class StringToParse(s:String) extends ToParse {
  def words = s.split(' ')
  def size = words.size
}

