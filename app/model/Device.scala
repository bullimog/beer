package model

import akka.actor.Actor
import org.joda.time.DateTime


class Device(val id: Int, val description: String, val deviceType: Int, port:Int) {
  override def toString(): String ={
    "## Device:"+id+", description:"+description+", deviceType:"+deviceType+", port:"+port
  }

  def on() = {println(description+ " switched on")}
  def off() = {println(description+ " switched off")}
  def setThermostat(temperature:Double) = {
    //Boiler setting
    //Start Akka Actor, to adjust element, according to temperature
    println(description+ " set thermostat")
  }
  def waitTemperature(temperature: Double) = {println(description+ " wait temperature")}
  def waitTime(duration: Int) = {println(description+ " wait time")}
  def setPower(power: Int) = {println(description+ " set power")}
  def readTemperature(): Option[Double] = {
    println(description+ " read temperature")
    None
  }

}

object Device {
  val ANALOGUE_IN = 1
  val ANALOGUE_OUT = 2
  val DIGITAL_IN = 3
  val DIGITAL_OUT = 4
  val MONITOR = 5

  def apply(id: Int, description: String, deviceType: Int, port:Int): Device ={
    //TODO: validate values
    new Device(id, description, deviceType, port:Int)
  }
}

class BoilerActor(thermometer: Device, heater: Device, targetTemperature: Double) extends Actor {
  def receive = {
    case tick: String => {
      println("still going " + DateTime.now)
      thermometer.readTemperature() match {
        case Some(currentTemp) => heater.setPower(calculateHeatSetting(targetTemperature - currentTemp))
        case _ => heater.off() //to be safe!
      }
    }
    case _ => println("unknown message")
  }

  def calculateHeatSetting(tempDiff: Double): Int ={
    if(tempDiff > 1.0) 100
    else (tempDiff * 50).toInt
  }
}