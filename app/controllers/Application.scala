package controllers

import akka.actor.ActorSystem
import connector.{K8055Stub, K8055}
import play.api._
import play.api.mvc._
import sequencer.Sequencer

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
}

object Beer extends App{
  //instantiate Sequencer singleton Object
  val sequencer = new Sequencer with K8055Stub

  println("About to run sequence")
  sequencer.runSequence
  println("Kicked off sequence")
  Thread.sleep(1000)
  println("App End")

}