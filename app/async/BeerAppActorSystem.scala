package async

import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, Props, Actor, ActorSystem}
import org.joda.time.DateTime

import scala.concurrent.duration.FiniteDuration


object BeerAppActorSystem {

  var actorList: List[Actor] = List()
  val system: ActorSystem = ActorSystem("BeerApp")

  def shutdown = {system.shutdown()}


//  val scheduler = system.actorOf(Props[BeerSchedulerActor], name = "scheduler")
//  val tickInterval  = new FiniteDuration(1, TimeUnit.SECONDS)
//  val cancellable = system.scheduler.schedule(tickInterval, tickInterval, scheduler, "tick") //initialDelay, delay, Actor, Message
//}

//class BeerSchedulerActor() extends Actor {
//
//  val aggregateActor: ActorRef = context.actorOf(Props[BeerSchedulerActor], name = "scheduler")
//
//  def receive = {
//    case "tick" => {
//      println("still going " + DateTime.now)
//      //propagate ticks..
//    }
//    case _ =>
//  }
}



