package controllers

import akka.actor.{Actor, ActorSystem}
import org.joda.time.DateTime


object BeerAppActorSystem {
  val system: ActorSystem = ActorSystem("BeerApp")

  def shutdown = {system.shutdown()}
}



//class BeerActor extends Actor {
//  def receive = {
//    case message: Any => {}
//    case _ => println("unknown message")
//
//  }
//
//  object BeerActor {
//    def apply {}
//  }

//}
