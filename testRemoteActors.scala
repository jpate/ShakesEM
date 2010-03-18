package testRemote {
  import scala.actors.Actor
  import scala.actors.Actor._
  import scala.actors.remote.RemoteActor
  import scala.actors.remote.RemoteActor._
  import scala.actors.remote.Node

  class Receiver(someVar:Int) extends Actor {
    def act = {
      alive( 9999 )
      register( 'receiver, self )
      println("started up")
      loop {
        react {
          case s:String => {
            println("Whoa! got " + s + " and variable " + someVar)
          }
          case 'stop => {
            exit()
          }
        }
      }
    }

  }

  object startReceiver {
    def main( args:Array[String] ) {
  
      val ohReceiver = new Receiver(16)
      ohReceiver.start

    }
  }


  object testReceiver {
    def main( args:Array[String] ) {
  
      actor {
        val rec = select( Node("127.0.0.1", 9999) , 'receiver )

        println( rec )

        println("sending one")
        rec ! "yeehaw"
        println("sending two")
        rec ! 'stop
      }
    }
  }
 
}

