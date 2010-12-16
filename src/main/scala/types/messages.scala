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

package ShakesEM.types.messages

import ShakesEM.types.expansions._
import ShakesEM.grammars.ShakesPCNF
import collection.immutable.{HashMap => IHashMap,HashSet => IHashSet}
import collection.mutable.{HashMap => MHashMap, HashSet => MHashSet}

@serializable case class F_Key(
  start:Int,
  end:Int,
  lhs:String,
  left:String,
  right:String
)
@serializable case class G_Key(
  index:Int,
  pos:String,
  word:String
)
@serializable case class H_Key(
  start:Int,
  end:Int,
  label:String
)

@serializable case object StillAlive


@serializable case class FResult(
  f_i:scala.collection.immutable.Map[F_Key,Double],
  scaledBy:Double
)

@serializable case class GResult(
  g_i:scala.collection.immutable.Map[G_Key,Double],
  scaledBy:Double
)

@serializable case class HResult(
  h_i:scala.collection.immutable.Map[H_Key,Double],
  scaledBy:Double
)

@serializable case class FSums(
  f_Reply:scala.collection.immutable.Map[NTExpansion,Double]
)

@serializable case class GSums(
  f_Reply:scala.collection.immutable.Map[TermExpansion,Double]
)

@serializable case class HSums(
  f_Reply:scala.collection.immutable.Map[String,Double]
)

@serializable case class StringProbResult(
  scaledStringProb:Double,
  scaledBy:Double
)

@serializable case class ParsingResult(
  parserID:ParserID,
  scaledStringProb:Double,
  f_i:scala.collection.immutable.Map[F_Key,Double],
  g_i:scala.collection.immutable.Map[G_Key,Double],
  h_i:scala.collection.immutable.Map[H_Key,Double],
  scaledBy:Double
)

@serializable abstract class ParserID(id:Int)
@serializable case class RemoteParserID(id:Int) extends ParserID(id)
@serializable case class LocalParserID(id:Int) extends ParserID(id)

case class Evaluation( prefix:String, trainedGram:ShakesPCNF )

// Use this to terminate parsers when we run out of sentences to give them.
case object Stop


