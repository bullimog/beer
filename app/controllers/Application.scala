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

  //initialise the thermostat data...
  componentManager.initThermostats(componentCollection)

  def index = Action {
    Ok(views.html.index(sequenceToReadableSequence(sequence, componentManager, componentCollection),
      componentCollection))
  }

  /** copies a Sequence to a ReadableSequence, formatting internal data to human-readable. */
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

  /** ***************** Ajax Services ********************
    * *******************************************************/
  def sequencerStatus() = Action { implicit request =>
    val ss = SequenceStatus(Sequencer.running, Sequencer.currentStep, compileComponentsStatuses(), compileThermostatStatuses())
    Ok(Json.toJson(ss).toString())
  }

  def compileComponentsStatuses(): List[ComponentStatus] = {
    var componentStatuses: ListBuffer[ComponentStatus] = ListBuffer[ComponentStatus]()
    componentCollection.devices.foreach(device => {
      var cs = ComponentStatus(device.id, device.deviceType, componentManager.isOn(device).toString, device.units)
      device.deviceType match {
        case Component.TIMER => cs = ComponentStatus(device.id, device.deviceType, Timer.remainingTime().toString, device.units)
        case Component.ANALOGUE_IN => cs = ComponentStatus(device.id, device.deviceType, componentManager.readTemperature(device).getOrElse(0).toString, device.units)
        case Component.ANALOGUE_OUT => cs = ComponentStatus(device.id, device.deviceType, componentManager.getPower(device).getOrElse(0).toString, device.units)
        case Component.DIGITAL_IN => cs = ComponentStatus(device.id, device.deviceType, componentManager.isOn(device).toString, device.units)
        case Component.DIGITAL_OUT => cs = ComponentStatus(device.id, device.deviceType, componentManager.isOn(device).toString, device.units)
      }
      componentStatuses += cs
    })
    componentStatuses.toList
  }

  def compileThermostatStatuses(): List[ThermostatStatus] = {
    var thermostatStatuses: ListBuffer[ThermostatStatus] = ListBuffer[ThermostatStatus]()
    componentCollection.thermostats.foreach(thermostat => {
      val enabled:Boolean = componentManager.getThermostatEnabled(thermostat)
      val temperature:Double = componentManager.getThermostatHeat(thermostat)
      val cThermometer:Component = componentManager.componentFromId(componentCollection, thermostat.thermometer)
      val cHeater:Component = componentManager.componentFromId(componentCollection, thermostat.heater)
      (cThermometer, cHeater) match {  //Need to cast to Devices, to get units
        case(thermometer:Device, heater:Device) => {
          val thermometerStatus = ComponentStatus(thermometer.id, Component.ANALOGUE_IN, componentManager.readTemperature(thermometer).getOrElse(0).toString, thermometer.units)
          val heaterStatus = ComponentStatus(heater.id, heater.deviceType, componentManager.getPower(heater).getOrElse(0).toString, heater.units)
          val thermostatStatus = ThermostatStatus(thermostat.id, enabled, temperature, thermometerStatus, heaterStatus)
          thermostatStatuses += thermostatStatus
        }
      }
    })
    thermostatStatuses.toList
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
      case t:Thermostat => setThermostatState(t, state)
      case d:Device => setDeviceState(d, state)
    }
    Ok("Ok")
  }

  private def setThermostatState(thermostat: Thermostat, state:String): Unit ={
    try{
      componentManager.setThermostatEnabled(componentCollection, thermostat, state.toBoolean)
    }catch{
      case e:Exception => {
        val current:Double = componentManager.getThermostatHeat(thermostat)
        state match{
          case "ddown" => componentManager.setThermostatHeat(componentCollection, thermostat, current - 10)
          case "down" => componentManager.setThermostatHeat(componentCollection, thermostat, current - 1)
          case "up" => componentManager.setThermostatHeat(componentCollection, thermostat, current + 1)
          case "dup" => componentManager.setThermostatHeat(componentCollection, thermostat, current + 10)
        }
      }
    }
  }

  private def setDeviceState(device:Device, state:String) = {
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


  private def limitPercent(in:Int):Int = {
    if(in > 100) 100
    else if (in < 0) 0
    else in
  }

  def javascriptRoutes = Action { implicit request =>
    Ok(Routes.javascriptRouter("jsRoutes")(routes.javascript.Application.sequencerStatus,
      routes.javascript.Application.setComponentState,
      routes.javascript.Application.startSequencer,
      routes.javascript.Application.stopSequencer)).as("text/javascript")
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

  private def formatTemperature(temperature: Option[Double]): Option[String] = {
    temperature match {
      case Some(temp) => Some("" + temp + " \u00b0c")  //degree symbol
      case None => None
    }
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
  componentManager.stopThermostats()

}