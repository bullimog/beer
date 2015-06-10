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
  val componentManager = new ComponentManager with ComponentManagerK8055
  var componentCollection = controllers.ConfigIO.readComponentCollection("deviceSetup.json")
  var sequence = controllers.ConfigIO.readSteps("sequence1.json")
  val sequencer = new Sequencer

  println("About to run sequence")
  sequencer.runSequence(componentManager, componentCollection, sequence)
  println("Kicked off sequence")
  Thread.sleep(1000)
  println("App End")

}