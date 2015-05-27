package model

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, Cancellable, Props}
import async.BeerAppActorSystem._
import org.joda.time.DateTime
import sequencer.Sequencer

import scala.concurrent.duration.FiniteDuration

//Thermostat is composed of a heater and a Thermometer Device
class Thermostat(override val id: Int, override val description: String, override val deviceType: Int,
             override val port:Option[Int], thermometer:Device, heater:Device)
  extends Device(id, description, deviceType, port){

  override def setThermostat(temperature:Double) = {
    //Thermostat setting
    //Start Akka Actor, to adjust element, according to temperature
    //val scheduler = system.actorOf(Props[BoilerActor], name = "scheduler")
    val thermostat = system.actorOf(Props(new ThermostatActor(thermometer, heater, temperature)), name = "thermostat")
    val tickInterval  = new FiniteDuration(1, TimeUnit.SECONDS)
    cancellable = system.scheduler.schedule(tickInterval, tickInterval, thermostat, "tick") //initialDelay, delay, Actor, Message
    println(description+ " set thermostat to "+ temperature)
  }
}

object Thermostat{
  def apply(id: Int, description: String, deviceType: Int, port:Option[Int],
          thermometerId:Int, heaterId:Int): Device ={

    new Thermostat(id, description, deviceType, port, deviceFromId(thermometerId),deviceFromId(heaterId))
  }

  val deviceFromId = (id:Int) => {
    Sequencer.devices.filter((device:Device) => device.id == id).head
  }
}

class ThermostatActor(thermometer: Device, heater: Device, targetTemperature: Double) extends Actor {
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