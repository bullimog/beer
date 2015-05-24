package equipment

import java.util.concurrent.TimeUnit

import akka.actor._
import org.joda.time.DateTime

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.BeerAppActorSystem.system
import EquipmentIndexer.getId

// Heater with a pump and thermometer
class Boiler(id: Int, name: String, equipmentType: String,
             heatingElement: HeatingElement, thermometer: Option[Thermometer],
             pump: Option[Pump]) extends Equipment(id, name, equipmentType){
  var cancellable: Cancellable = null //mutable state :(

//  parts.foreach(i => println(i))

  //Just a Heater
  def this(id: Int, name: String, equipmentType: String,
           heatingElement: HeatingElement) = this(id, name, equipmentType, heatingElement, None, None)

  // Heater with just a Thermometer
  def this(id: Int, name: String, equipmentType: String,
           heatingElement: HeatingElement, thermometer: Thermometer) =
      this(id, name, equipmentType, heatingElement, Some(thermometer), None)

  //Heater with just a Pump
  def this(id: Int, name: String, equipmentType: String,
           heatingElement: HeatingElement, pump: Pump) =
    this(id, name, equipmentType, heatingElement, None, Some(pump))

  //Pump Control
  def circulateOn() = {

    pump match{
      case Some(p) => { p.on }
      case _ =>
    }
  }

  def circulateOff() = {
    pump match{
      case Some(p) => { p.off }
      case _ =>
    }
  }

  def isCirculating: Boolean = {
    pump match{
      case Some(p) => { p.isOn }
      case _ => false
    }
  }


  //Heating Element control
  def heatOn() = {
    heatingElement.setPower(100)
  }
  def heatOff() = {
    heatingElement.setPower(0)
  }

  def heat(power: Int) = {
    heatingElement.setPower(power)
  }

  def heatingPower: Int = {
    heatingElement.getPower
  }


  def readTemperature: Option[Double] = {
    thermometer match{
      case Some(t) => { Some(t.readTemperature()) }
      case _ => None
    }
  }

  //Hold at the target temperature for the specified time
  def maintainTemperature(targetTemperature: Double, seconds: Int): Boolean ={
    thermometer match{
      case None => false  //No thermometer = no monitoring!
      case _ => {
        monitorAndControlTemperatureUntil(DateTime.now.plusSeconds(seconds), targetTemperature)
        true
      }
    }
  }


  private def monitorAndControlTemperatureUntil(finishTime: DateTime, targetTemperature: Double) = {
    println("will stop at " + finishTime)
    //val boiler = system.actorOf(Props[BoilerActor], name = "boiler")
    val boiler = system.actorOf(Props(new BoilerActor(this, targetTemperature)), name = "boiler")
    val fd  = new FiniteDuration(1, TimeUnit.SECONDS)
    println("1")
    cancellable = system.scheduler.schedule(fd, fd, boiler, finishTime) //initialDelay, delay, Actor, Message
    println("2")
  }

  def stopMonitorAndControlTemperature() = {
    cancellable.cancel()
    heatingElement.off
  }

  //TODO: Need to return List of name/value pairs with finish time and target temp, etc
  def monitorAndControlTemperatureStatus : Boolean = {
    cancellable.isCancelled
  }
}

/**
 *
 */
class BoilerActor(boiler: Boiler, targetTemperature: Double) extends Actor {
  def receive = {
    case finishTime: DateTime => {
      println("still going " + DateTime.now)
        boiler.readTemperature match {
        case Some(currentTemp) => boiler.heat(calculateHeatSetting(targetTemperature - currentTemp))
        case _ => boiler.heatOff() //to be safe!
      }
    }
    case _ => println("unknown message")
  }

  def calculateHeatSetting(tempDiff: Double): Int ={
    if(tempDiff > 1.0) 100
    else (tempDiff * 50).toInt
  }


}


// Companion factory object
object Boiler {
//  def apply(name: String, parts: List[Equipment]): Boiler ={
//    new Boiler(name, "Boiler", parts)
//  }

  def apply(name: String, heatingElement: HeatingElement ): Boiler = {
    new Boiler(getId, name, "Boiler", heatingElement)
  }

  def apply(name: String, heatingElement: HeatingElement, thermometer: Thermometer ): Boiler = {
    new Boiler(getId, name, "Boiler", heatingElement, Some(thermometer), None)
  }

  def apply(name: String, heatingElement: HeatingElement, pump: Pump ): Boiler = {
    new Boiler(getId, name, "Boiler", heatingElement, None, Some(pump) )
  }

  def apply(name: String, heatingElement: HeatingElement, thermometer: Thermometer,  pump: Pump ): Boiler = {
    new Boiler(getId, name, "Boiler", heatingElement, Some(thermometer), Some(pump) )
  }

}