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

import connector.{K8055Board, DeviceConnector, DeviceConnectorStub}
import model._
import org.joda.time.Period
import org.joda.time.format.PeriodFormat
import play.api._
import play.api.libs.json.Json
import play.api.mvc._
import sequencer.Sequencer


import scala.collection.mutable.ListBuffer

object Application extends Controller {

  val componentManager = new ComponentManager with BrewComponentManager {
    override val deviceConnector: DeviceConnector = new DeviceConnector with K8055Board //DeviceConnectorStub //stub for now...
  }

  val sequence:Sequence = controllers.ConfigIO.readSteps("sequence1.json")
  val componentCollection = controllers.ConfigIO.readComponentCollection("deviceSetup.json")

  //initialise the monitor data...
  componentManager.initMonitors(componentCollection)

  def index = Action {
    Ok(views.html.index(sequenceToReadableSequence(sequence, componentManager, componentCollection),
      componentCollection))
  }

  /** copies a Sequence to a ReadableSequence, formatting internal data to human-readable. */
  //TODO: Use For/Yield
  def sequenceToReadableSequence(sequence: Sequence, componentManager: ComponentManager,
                                 componentCollection: ComponentCollection): ReadableSequence = {
    val lbFSteps = new ListBuffer[ReadableStep]()
    sequence.steps.foreach(step => {
      lbFSteps += ReadableStep(step.id, step.device,
        componentManager.getComponentFromCollection(step, componentCollection).description,
        step.eventType, step.decode, formatTarget(step.target, step.device), formatPeriod(step.duration))
    })
    ReadableSequence(sequence.description, lbFSteps.toList, 0)
  }

  private def formatTarget(target: Option[Double], device:Int): Option[String] = {
    val comp:Component = componentManager.componentFromId(componentCollection, device)
    (comp,target) match {
      case (device:Device, Some(temp)) => Some("" + temp + device.units.getOrElse(""))
      case (monitor:Monitor, Some(temp)) => {
        val therm:Component = componentManager.componentFromId(componentCollection, monitor.sensor)
        therm match {case device:Device => Some("" + temp + device.units.getOrElse(""))}
      }
      case (_,_) => None
    }
  }

  private def formatPeriod(seconds: Option[Int]): Option[String] = {
    seconds match {
      case Some(secs) => {
        val period: Period = Period.seconds(secs)
        Some(PeriodFormat.getDefault.print(period.normalizedStandard()))
      }
      case None => None
    }
  }

  /** ***************** Ajax Services ********************
    * *******************************************************/
  def sequencerStatus() = Action { implicit request =>
    val ss = SequenceStatus(Sequencer.running, Sequencer.currentStep, compileComponentsStatuses(), compileMonitorStatuses())
    Ok(Json.toJson(ss).toString())
  }

  //TODO: can we use a yield here??
  private def compileComponentsStatuses(): List[ComponentStatus] = {
    var componentStatuses: ListBuffer[ComponentStatus] = ListBuffer[ComponentStatus]()
    componentCollection.devices.foreach(device => {
      var cs = ComponentStatus(device.id, device.deviceType, componentManager.isOn(device).toString, device.units)
      device.deviceType match {
        case Component.TIMER => cs = ComponentStatus(device.id, device.deviceType, Timer.remainingTime().toString, device.units)
        case Component.ANALOGUE_IN => cs = ComponentStatus(device.id, device.deviceType, componentManager.readSensor(device).getOrElse(0).toString, device.units)
        case Component.ANALOGUE_OUT => cs = ComponentStatus(device.id, device.deviceType, componentManager.getPower(device).getOrElse(0).toString, device.units)
        case Component.DIGITAL_IN => cs = ComponentStatus(device.id, device.deviceType, componentManager.isOn(device).toString, device.units)
        case Component.DIGITAL_OUT => cs = ComponentStatus(device.id, device.deviceType, componentManager.isOn(device).toString, device.units)
      }
      componentStatuses += cs
    })
    componentStatuses.toList
  }

  //TODO: can we use a yield here??
  private def compileMonitorStatuses(): List[MonitorStatus] = {
    var monitorStatuses: ListBuffer[MonitorStatus] = ListBuffer[MonitorStatus]()
    componentCollection.monitors.foreach(monitor => {
      val enabled:Boolean = componentManager.getMonitorEnabled(monitor)
      val temperature:Double = componentManager.getMonitorTarget(monitor)
      val cSensor:Component = componentManager.componentFromId(componentCollection, monitor.sensor)
      val cIncreaser:Component = componentManager.componentFromId(componentCollection, monitor.increaser)
      (cSensor, cIncreaser) match {
        case(sensor:Device, increaser:Device) => {  //Need to cast to Devices, to get units
          val sensorStatus = ComponentStatus(sensor.id, Component.ANALOGUE_IN, componentManager.readSensor(sensor).getOrElse(0).toString, sensor.units)
          val increaserStatus = ComponentStatus(increaser.id, increaser.deviceType, componentManager.getPower(increaser).getOrElse(0).toString, increaser.units)
          val monitorStatus = MonitorStatus(monitor.id, enabled, temperature, sensorStatus, increaserStatus)
          monitorStatuses += monitorStatus
        }
      }
    })
    monitorStatuses.toList
  }

  def startSequencer() = Action { implicit request =>
    Sequencer.runSequence(componentManager, componentCollection, sequence)
    Ok("Started")
  }

  def stopSequencer() = Action { implicit request =>
    Sequencer.abortSequence(componentManager)
    Ok("Stopped")
  }


  def setComponentState(componentId: String, state:String) = Action { implicit request =>
    val component = componentManager.componentFromId(componentCollection, componentId.toInt)
    component match {
      case t:Monitor => setMonitorState(t, state)
      case d:Device => setDeviceState(d, state)
    }
    Ok("Ok")
  }

  private def setMonitorState(monitor: Monitor, state:String): Unit ={
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


  private def setDeviceState(device:Device, state:String) = {
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
    Ok(Routes.javascriptRouter("jsRoutes")(routes.javascript.Application.sequencerStatus,
      routes.javascript.Application.setComponentState,
      routes.javascript.Application.startSequencer,
      routes.javascript.Application.stopSequencer)).as("text/javascript")
  }



}


/**********************************************************
  For testing...
 *********************************************************/
object Beer extends App{
  //instantiate Sequencer singleton Object
  val componentManager = new ComponentManager with BrewComponentManager{
    override val deviceConnector:DeviceConnector = new DeviceConnector with DeviceConnectorStub //stub for now...
  }
  var componentCollection = controllers.ConfigIO.readComponentCollection("deviceSetup.json")
  val sequence = controllers.ConfigIO.readSteps("sequence1.json")
  //val sequencer = new Sequencer

  import sys.process.Process
  val command = "ls -al"
  val result = Process(""+command+"")
  println("result="+result.!)


  println("About to run sequence")
  Sequencer.runSequence(componentManager, componentCollection, sequence)
  println("Kicked off sequence")
  Thread.sleep(1000)
  println("App End")
  componentManager.stopMonitors()

}