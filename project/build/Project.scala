import sbt._

class ShakesEMProject(info: ProjectInfo) extends DefaultProject(info)
{
    override def compileOptions = Unchecked :: super.compileOptions.toList
}

