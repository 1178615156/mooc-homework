package actorbintree

import akka.actor.{Actor, ActorSystem, Props}

object Main {

  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem()
    val actor = actorSystem.actorOf(Props(new Actor {
      println("init")
      override def receive: Receive = {
        case _ => throw new Exception("")
      }
    }))
    actor ! "restart"

  }
}
