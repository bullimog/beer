package controllers

import akka.actor.ActorSystem
import connector.{K8055Stub, K8055}
import model._
import org.joda.time.Period
import org.joda.time.format.PeriodFormat
import play.api._
import play.api.libs.json.Json
import play.api.mvc._
import sequencer.Sequencer


import scala.collection.mutable.ListBuffer

object Application extends Controller {

  val componentManager = new ComponentManager with ComponentManagerK8055 {
    override val k8055: K8055 = new K8055 with K8055Stub //stub for now...
  }

  val sequence = controllers.ConfigIO.readSteps("sequence1.json")
  val componentCollection = controllers.ConfigIO.readComponentCollection("deviceSetup.json")

  def index = Action {
    //val fs:ReadableSequence = sequenceToReadableSequence(sequence, componentManager, componentCollection)
    //println("friendlySequenceToJSON = " + friendlySequenceToJSON(fs))
    Ok(views.html.index(sequenceToReadableSequence(sequence, componentManager, componentCollection),
      componentCollection))
  }


  /** ***************** Ajax Services ********************
    * *******************************************************/
  def sequencerStatus() = Action { implicit request =>
    val ss = SequenceStatus(Sequencer.running, Sequencer.currentStep, compileComponentsStatuses())
    //Ok(Sequencer.running.toString+":"+Sequencer.currentStep.toString)
    Ok(Json.toJson(ss).toString())
  }

  //  def friendlySequenceToJSON(fs:ReadableSequence):String = {
  //    import play.api.libs.json._
  //    implicit val devWrites = Json.writes[ReadableStep]
  //    implicit val devicesWrites = Json.writes[ReadableSequence]
  //    Json.toJson(fs).toString()
  //  }


  def compileComponentsStatuses(): List[ComponentStatus] = {
    var componentStatuses: ListBuffer[ComponentStatus] = ListBuffer[ComponentStatus]()
    componentCollection.devices.foreach(device => {
      var cs = ComponentStatus(device.id, device.deviceType, componentManager.isOn(device).toString)
      device.deviceType match {
        case Component.TIMER => cs = ComponentStatus(device.id, device.deviceType, Timer.remainingTime().toString)
        case Component.ANALOGUE_IN => cs = ComponentStatus(device.id, device.deviceType, componentManager.readTemperature(device).getOrElse(0) toString)
        case Component.ANALOGUE_OUT => cs = ComponentStatus(device.id, device.deviceType, componentManager.getPower(device).getOrElse(0).toString)
        case Component.DIGITAL_IN => cs = ComponentStatus(device.id, device.deviceType, componentManager.isOn(device).toString)
        case Component.DIGITAL_OUT => cs = ComponentStatus(device.id, device.deviceType, componentManager.isOn(device).toString)
      }
      componentStatuses += cs
    })
    componentStatuses.toList
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
    Ok(Routes.javascriptRouter("jsRoutes")(routes.javascript.Application.sequencerStatus,
      routes.javascript.Application.startSequencer,
      routes.javascript.Application.stopSequencer)).as("text/javascript")
  }

  def sequenceToReadableSequence(sequence: Sequence, componentManager: ComponentManager,
                                 componentCollection: ComponentCollection): ReadableSequence = {
    val lbFSteps = new ListBuffer[ReadableStep]()
    sequence.steps.foreach(step => {
      lbFSteps += ReadableStep(step.id, step.device,
        componentManager.getComponentFromCollection(step, componentCollection).description,
        step.eventType, step.decode, formatTemperature(step.temperature), formatPeriod(step.duration))
    })
    ReadableSequence(sequence.description, lbFSteps.toList, 0)
  }

  def formatPeriod(seconds: Option[Int]): Option[String] = {
    seconds match {
      case Some(secs) => {
        val period: Period = Period.seconds(secs)
        Some(PeriodFormat.getDefault().print(period.normalizedStandard()))
      }
      case None => None
    }
  }

  def formatTemperature(temperature: Option[Double]): Option[String] = {
    temperature match {
      case Some(temp) => {
        Some("" + temp + " \u00b0c")  //degree symbol
      }
      case None => None
    }
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