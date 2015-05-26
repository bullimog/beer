package model

import java.util.concurrent.TimeUnit

import akka.actor.{Cancellable, Props, Actor}
import org.joda.time.DateTime
import async.BeerAppActorSystem.system
import sequencer.Sequencer

import scala.concurrent.duration.FiniteDuration


class Device(val id: Int, val description: String, val deviceType: Int,
//             val port:Option[Int], thermometer:Option[Device], heater:Option[Device]) {
             val port:Option[Int]) {

  var cancellable:Cancellable = null
  override def toString(): String ={
    "## Device:"+id+", description:"+description+", deviceType:"+deviceType+", port:"+port
  }

  def on() = {println(description+ " switched on")}
  def off() = {

    println(description+ " switched off")
  }

  def setThermostat(temperature:Double) = {}

  def waitTemperature(temperature: Double) = {
    print(description+ " waiting for temperature: " + temperature +"... ")
    Thread.sleep(5000)
    println(" Done!")
    //Will need an Actor, to periodically check
  }
  def waitTime(duration: Int) = {
    print(description+ " waiting for "+ duration + " seconds...")
    Thread.sleep(5000)
    println(" Done!")
  }

  def setPower(power: Int) = {println(description+ " set power to " + power + "%")}
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

  def apply(id: Int, description: String, deviceType: Int, port:Option[Int]): Device ={
    //TODO: validate values
    new Device(id, description, deviceType, port)
  }
}

class ThermometerActor(thermometer: Device, targetTemperature: Double) extends Actor {
  def receive = {
    case tick: String => {
      println("still going " + DateTime.now)
      thermometer.readTemperature() match {
        case Some(currentTemp) => if(currentTemp == targetTemperature) thermometer.cancellable.cancel()
        case _ =>  //to be safe!
      }
    }
    case _ => println("unknown message")
  }

  def calculateHeatSetting(tempDiff: Double): Int ={
    if(tempDiff > 1.0) 100
    else (tempDiff * 50).toInt
  }
}