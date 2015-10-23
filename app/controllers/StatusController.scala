/**
Copyright © 2015 Graeme Bullimore

This file is part of BulliBrew.
BulliBrew is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

BulliBrew is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
  along with BulliBrew.  If not, see <http://www.gnu.org/licenses/>.
*/

package controllers

import connector.{ConfigIO, K8055Board, DeviceConnector, DeviceConnectorStub}
import model._
import org.joda.time.Period
import org.joda.time.format.PeriodFormat
import play.api.Routes
import play.api.libs.json.Json
import play.api.mvc._
import sequencer.Sequencer
import scala.concurrent.Future

object StatusController extends Controller {

  val componentManager = new ComponentManager with BrewComponentManager {
    override val deviceConnector: DeviceConnector = new DeviceConnector with DeviceConnectorStub //stub for now...
    //override val deviceConnector: DeviceConnector = new DeviceConnector with K8055Board
  }

  val defaultComponentCollection: ComponentCollection =  ComponentCollection ("Empty", "None", List(), List())

  //Mutable state, shared by all users...
  var sequence:Sequence = ConfigIO.readSteps("sequence1.json") //TODO move to session??
//  var componentCollection = defaultComponentCollection

  def componentCollection()(implicit req: Request[_]):ComponentCollection = {
    val deviceConfigFile = req.session.get("devices").getOrElse("badConfigFile")
    ComponentsController.componentCollection(deviceConfigFile)
  }

  //initialise the monitor data...
  //  componentManager.initMonitors(componentCollection)

  def index() = Action.async {
    implicit request => {Future.successful(Redirect(routes.StatusController.present()))}
  }

  def present = Action.async {
    implicit request => {
      if (request.session.get("devices").isDefined) {
//          val deviceConfigFile = request.session.get("devices").getOrElse("badConfigFile")
//          componentCollection = ComponentsController.componentCollection(deviceConfigFile)

          Future.successful(Ok(views.html.index(
            sequenceToReadableSequence(sequence, componentManager, componentCollection),
            cCToReadableCc(componentCollection)))
          )
      }
      else Future.successful(Redirect(routes.DeviceEdit.present()))
    }
  }



  def cCToReadableCc(componentCollection: ComponentCollection):ReadableComponentCollection = {
    val rccMonitors:List[ReadableMonitor] = for {monitor <- componentCollection.monitors
      if componentManager.deviceFromId(componentCollection, monitor.sensor).isDefined &&
         componentManager.deviceFromId(componentCollection, monitor.increaser).isDefined
    }
    yield{
      ReadableMonitor(monitor.id, monitor.description, monitor.deviceType,
        componentManager.deviceFromId(componentCollection, monitor.sensor).orNull,
        componentManager.deviceFromId(componentCollection, monitor.increaser).orNull)
    }

    ReadableComponentCollection(componentCollection.name, componentCollection.description,
      componentCollection.devices, rccMonitors)

  }

  /** copies a Sequence to a ReadableSequence, formatting internal data to human-readable. */
  def sequenceToReadableSequence(sequence: Sequence, componentManager: ComponentManager,
                                 componentCollection: ComponentCollection)
                                (implicit request: Request[_]) :ReadableSequence = {
    val steps = for (step <- sequence.steps) yield {
      componentManager.getComponentFromCollection(step, componentCollection).map( // find a component, for each step...
        component => ReadableStep(step.id, step.device, component.description,    // Create a human-readable step...
                                  step.eventType, step.decode,
                                  formatTarget(step.target, step.device), formatPeriod(step.duration))
      )
    }
    ReadableSequence(sequence.description, steps.flatMap(s => s), 0)
  }

  private def formatTarget(target: Option[Double], device:Int) (implicit request: Request[_]): Option[String] = {
    val oComp = componentManager.componentFromId(componentCollection, device)
    (oComp, target) match {
      case (Some(device: Device), Some(temp)) => Some("" + temp + device.units.getOrElse(""))
      case (Some(monitor: Monitor), Some(temp)) => {
        val maybeSensor:Option[Component] = componentManager.componentFromId(componentCollection, monitor.sensor)

        maybeSensor match {
          case Some(sensor: Device) => Some("" + temp + sensor.units.getOrElse(""))
          case (_) => None
        }
      }
      case (_) => None
    }
  }

  private def formatPeriod(seconds: Option[Int]): Option[String] = {
    seconds.map(secs => PeriodFormat.getDefault.print(Period.seconds(secs).normalizedStandard()))
  }



  /** ***************** Ajax Services ********************
    * *******************************************************/
  def sequencerStatus() = Action { implicit request =>
    val ss = SequenceStatus(Sequencer.running, Sequencer.currentStep, compileComponentsStatuses(), compileMonitorStatuses())
    Ok(Json.toJson(ss).toString())
  }

  private def compileComponentsStatuses()(implicit request: Request[_]): List[ComponentStatus] = {
    for(device <- componentCollection.devices) yield {
      device.deviceType match {
        case Component.TIMER => ComponentStatus(device.id, device.deviceType, Timer.remainingTime().toString, device.units)
        case Component.ANALOGUE_IN => ComponentStatus(device.id, device.deviceType, componentManager.readSensor(device).getOrElse(0).toString, device.units)
        case Component.ANALOGUE_OUT => ComponentStatus(device.id, device.deviceType, componentManager.getPower(device).getOrElse(0).toString, device.units)
        case Component.DIGITAL_IN => ComponentStatus(device.id, device.deviceType, componentManager.isOn(device).toString, device.units)
        case Component.DIGITAL_OUT => ComponentStatus(device.id, device.deviceType, componentManager.isOn(device).toString, device.units)
      }
    }
  }

  private def compileMonitorStatuses()(implicit request: Request[_]): List[MonitorStatus] = {
    val monitorStatuses = for(monitor <- componentCollection.monitors) yield {
      val enabled:Boolean = componentManager.getMonitorEnabled(monitor)
      val temperature:Double = componentManager.getMonitorTarget(monitor)
      val cSensor:Option[Component] = componentManager.componentFromId(componentCollection, monitor.sensor)
      val cIncreaser:Option[Component] = componentManager.componentFromId(componentCollection, monitor.increaser)
      (cSensor, cIncreaser) match {
        case(Some(sensor:Device), Some(increaser:Device)) => {  //Need to cast to Devices, to get units
          val sensorStatus = ComponentStatus(sensor.id, Component.ANALOGUE_IN, componentManager.readSensor(sensor).getOrElse(0).toString, sensor.units)
          val increaserStatus = ComponentStatus(increaser.id, increaser.deviceType, componentManager.getPower(increaser).getOrElse(0).toString, increaser.units)
          MonitorStatus(monitor.id, enabled, temperature, sensorStatus, increaserStatus)
        }
        case (_,_) => MonitorStatus(Integer.MAX_VALUE, false, 0, null, null) //dummy, to satisfy yield :(
      }
    }
    monitorStatuses.filter(ms => ms.componentId != Integer.MAX_VALUE)
  }

  //----------------------------------------------------------------------

  def startSequencer() = Action { implicit request =>
    Sequencer.runSequence(componentManager, componentCollection, sequence)
    Ok("Started")
  }

  def stopSequencer() = Action { implicit request =>
    Sequencer.abortSequence(componentManager)
    Ok("Stopped")
  }


  def setComponentState(componentId: String, state:String) = Action { implicit request =>
    val component:Option[Component] = componentManager.componentFromId(componentCollection, componentId.toInt)
    component match {
      case Some(t:Monitor) => setMonitorState(t, state)
      case Some(d:Device) => setDeviceState(d, state)
      case (_) =>
    }
    Ok("Ok")
  }

  private def setMonitorState(monitor: Monitor, state:String) (implicit request: Request[_]): Unit ={
    try{ // to convert state to Boolean
      componentManager.setMonitorEnabled(componentCollection, monitor, state.toBoolean)
    }catch{ // ok, not a Boolean, so execute a heat adjustment...
      case e:Exception => {
        val current:Double = componentManager.getMonitorTarget(monitor)
        state match{
          case "ddown" => componentManager.setMonitorTarget(componentCollection, monitor, current - 10)
          case "down" => componentManager.setMonitorTarget(componentCollection, monitor, current - 1)
          case "up" => componentManager.setMonitorTarget(componentCollection, monitor, current + 1)
          case "dup" => componentManager.setMonitorTarget(componentCollection, monitor, current + 10)
        }
      }
    }
  }


  private def setDeviceState(device:Device, state:String) (implicit request: Request[_]) = {
    val limitPercent:Int=>Int =(in:Int) => math.min(math.max(0,in) ,100)
    device.deviceType match {
      case Component.DIGITAL_OUT =>
        state.toBoolean match {
          case true => componentManager.on(componentCollection, device)
          case _ => componentManager.off(componentCollection, device)
        }
      case Component.ANALOGUE_OUT => {
        val current:Int = componentManager.getPower(device).getOrElse(0)
        state match{
          case "ddown" => componentManager.setPower(device, limitPercent(current - 10))
          case "down" => componentManager.setPower(device, limitPercent(current - 1))
          case "up" => componentManager.setPower(device, limitPercent(current + 1))
          case "dup" => componentManager.setPower(device, limitPercent(current +10))
        }
      }
    }
  }



  def javascriptRoutes = Action { implicit request =>
    Ok(Routes.javascriptRouter("jsRoutes")(routes.javascript.StatusController.sequencerStatus,
      routes.javascript.StatusController.setComponentState,
      routes.javascript.StatusController.startSequencer,
      routes.javascript.StatusController.stopSequencer)).as("text/javascript")
  }
}


/**********************************************************
  For testing...
 *********************************************************/
/*
object Beer extends App{
  //instantiate Sequencer singleton Object
  val componentManager = new ComponentManager with BrewComponentManager{
    override val deviceConnector:DeviceConnector = new DeviceConnector with DeviceConnectorStub //stub for now...
  }
  var componentCollection = connector.ConfigIO.readComponentCollection("deviceSetup.json").get
  val sequence = connector.ConfigIO.readSteps("sequence1.json")
  //val sequencer = new Sequencer

  println("About to run sequence")
  Sequencer.runSequence(componentManager, componentCollection, sequence)
  println("Kicked off sequence")
  Thread.sleep(1000)
  println("App End")
  componentManager.stopMonitors()

}
*/