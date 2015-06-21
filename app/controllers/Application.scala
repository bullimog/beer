package controllers

import akka.actor.ActorSystem
import connector.{K8055Stub, K8055}
import model._
import play.api._
import play.api.mvc._
import sequencer.Sequencer

import scala.collection.mutable.ListBuffer

object Application extends Controller {

  val componentManager = new ComponentManager with ComponentManagerK8055{
    override val k8055:K8055 = new K8055 with K8055Stub //stub for now...
  }

  val sequence = controllers.ConfigIO.readSteps("sequence1.json")
  val componentCollection = controllers.ConfigIO.readComponentCollection("deviceSetup.json")

  def index = Action {
    val fs:ReadableSequence = sequenceToReadableSequence(sequence, componentManager, componentCollection)
    //println("friendlySequenceToJSON = " + friendlySequenceToJSON(fs))
    Ok(views.html.index(sequenceToReadableSequence(sequence, componentManager, componentCollection),
                        componentCollection))
  }



  //Ajax Services
  def sequencerStatus() = Action { implicit request =>
    Ok(Sequencer.running.toString+":"+Sequencer.currentStep.toString)
  }
  def startSequencer() = Action { implicit request =>
    Sequencer.runSequence(componentManager, componentCollection, sequence)
    Ok("Started")
  }
  def stopSequencer() = Action { implicit request =>
    Sequencer.abortSequence(componentManager)
    Ok("Stopped")
  }

  def javascriptRoutes = Action { implicit request =>
    Ok(Routes.javascriptRouter("jsRoutes") (routes.javascript.Application.sequencerStatus,
                                            routes.javascript.Application.startSequencer,
                                            routes.javascript.Application.stopSequencer)).as("text/javascript")
  }



//  def friendlySequenceToJSON(fs:ReadableSequence):String = {
//    import play.api.libs.json._
//    implicit val devWrites = Json.writes[ReadableStep]
//    implicit val devicesWrites = Json.writes[ReadableSequence]
//
//
//    Json.toJson(fs).toString()
//  }

  def sequenceToReadableSequence(sequence: Sequence, componentManager: ComponentManager,
                                 componentCollection: ComponentCollection): ReadableSequence ={
    val lbFSteps = new ListBuffer[ReadableStep]()
    sequence.steps.foreach(step => {
      lbFSteps += ReadableStep(step.id, step.device,
        componentManager.getComponentFromCollection(step, componentCollection).description,
        step.eventType, step.decode, step.temperature, step.duration)
    })
    ReadableSequence(sequence.description, lbFSteps.toList, 0)
  }
}

object Beer extends App{
  //instantiate Sequencer singleton Object
  val componentManager = new ComponentManager with ComponentManagerK8055{
    override val k8055:K8055 = new K8055 with K8055Stub //stub for now...
  }
  var componentCollection = controllers.ConfigIO.readComponentCollection("deviceSetup.json")
  val sequence = controllers.ConfigIO.readSteps("sequence1.json")
  //val sequencer = new Sequencer

  println("About to run sequence")
  Sequencer.runSequence(componentManager, componentCollection, sequence)
  println("Kicked off sequence")
  Thread.sleep(1000)
  println("App End")
  componentManager.stopThermostats()

}