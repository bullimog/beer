package controllers

import akka.actor.ActorSystem
import connector.{K8055Stub, K8055}
import model._
import play.api._
import play.api.mvc._
import sequencer.Sequencer

import scala.collection.mutable.ListBuffer

object Application extends Controller {

  def index = Action {
    val sequence = controllers.ConfigIO.readSteps("sequence1.json")
    val componentCollection = controllers.ConfigIO.readComponentCollection("deviceSetup.json")


    val componentManager = new ComponentManager with ComponentManagerK8055{
      override val k8055:K8055 = new K8055 with K8055Stub //stub for now...
    }


    Ok(views.html.index(sequenceToReadableSequence(sequence, componentManager, componentCollection)))
  }


  def sequenceToReadableSequence(sequence: Sequence, componentManager: ComponentManager,
                                 componentCollection: ComponentCollection): FriendlySequence ={
    val lbFSteps = new ListBuffer[FriendlyStep]()
    var n = 1
    sequence.steps.foreach(step => {
      lbFSteps += FriendlyStep(n, step.device,
        componentManager.getComponentFromCollection(step, componentCollection).description,
        step.eventType, step.decode, step.temperature, step.duration)
      n = n +1
    })
    FriendlySequence(sequence.description, lbFSteps.toList)
  }
}

object Beer extends App{
  //instantiate Sequencer singleton Object
  val componentManager = new ComponentManager with ComponentManagerK8055{
    override val k8055:K8055 = new K8055 with K8055Stub //stub for now...
  }
  var componentCollection = controllers.ConfigIO.readComponentCollection("deviceSetup.json")
  val sequence = controllers.ConfigIO.readSteps("sequence1.json")
  val sequencer = new Sequencer

  println("About to run sequence")
  sequencer.runSequence(componentManager, componentCollection, sequence)
  println("Kicked off sequence")
  Thread.sleep(1000)
  println("App End")
  componentManager.stopThermostats()

}