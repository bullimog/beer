package controllers

import java.util.concurrent.TimeUnit

import akka.actor.{Props, Actor}
import async.BeerAppActorSystem._
import model.{Device, ComponentCollection, Thermostat, Component}
import org.joda.time.DateTime

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global

// abstract base trait
trait ComponentManager{
  def on(component:Component)
  def off(component:Component)
  def isOn(component:Component):Boolean
  def pause(component:Component)
  def resume(component:Component)
  val deviceFromId = (componentCollection:ComponentCollection, id:Int) => {}
  def waitTemperatureHeating(component:Component, targetTemperature: Double):Unit
  def readTemperature(component:Component): Option[Double]
  def waitTime(component:Component, duration: Int)
  def setPower(component:Component, power: Int)
  def setThermostatHeat(componentCollection:ComponentCollection, thermostat: Thermostat, temperature:Double)
}


//Subtrait, with concrete implementation
trait ComponentManagerK8055 extends ComponentManager{

  override def on(component:Component) = {println(component.description+ " switched on")} //TODO
  override def off(component:Component) = {println(component.description+ " switched off")} //TODO
  override def isOn(component:Component):Boolean = {println(component.description+ " is being examined"); true} //TODO
  override def pause(component:Component) = {}  //TODO
  override def resume(component:Component) = {} //TODO


  override val deviceFromId = (componentCollection:ComponentCollection, id:Int) => {
    componentCollection.devices.filter((device:Device) => device.id == id).head
  }

  @tailrec
  override final def waitTemperatureHeating(component:Component, targetTemperature: Double):Unit = {
    print(component.description + " waiting for temperature: " + targetTemperature + "... ")
    val risingTemp:Double = readTemperature(component).getOrElse(-273)
    if (risingTemp < targetTemperature) {
      Thread.sleep(1000)
      waitTemperatureHeating(component, targetTemperature)
    }
  }

  override def readTemperature(component:Component): Option[Double] = {
    println(component.description+ " read temperature")
    None
  }

  override def waitTime(component:Component, duration: Int) = {
    print(component.description+ " waiting for "+ duration + " seconds...")
    Thread.sleep(5000)
    println(" Done!")
  }

  override def setPower(component:Component, power: Int) = {println(component.description+ " set power to " + power + "%")}



  override def setThermostatHeat(componentCollection:ComponentCollection, thermostat: Thermostat, temperature:Double) = {

    //Thermostat setting
    //Start Akka Actor, to adjust element, according to temperature
    //val scheduler = system.actorOf(Props[BoilerActor], name = "scheduler")
    val thermometer = deviceFromId(componentCollection, thermostat.thermometer)
    val heater = deviceFromId(componentCollection, thermostat.heater)
    val actorRef = system.actorOf(Props(new ThermostatHeatActor(this, thermometer, heater, temperature)), name = "thermostat")
    val tickInterval  = new FiniteDuration(1, TimeUnit.SECONDS)
    var cancellable = Some(system.scheduler.schedule(tickInterval, tickInterval, actorRef, "tick")) //initialDelay, delay, Actor, Message
    println(thermostat.description+ " set thermostat to "+ temperature)
  }
}

//class ComponentManagerClass extends ComponentManagerK8055{}


class ThermostatHeatActor(componentManager: ComponentManager, thermometer: Component, heater: Component, targetTemperature: Double) extends Actor {
  def receive = {
    case tick: String => {
      println("still going " + DateTime.now)
      componentManager.readTemperature(thermometer) match {
        case Some(currentTemp) => componentManager.setPower(heater, calculateHeatSetting(targetTemperature - currentTemp))
        case _ => componentManager.off(heater) //to be safe!
      }
    }
    case _ => println("unknown message")
  }

  def calculateHeatSetting(tempDiff: Double): Int ={
    if(tempDiff > 2.0) 100
    else (tempDiff * 50).toInt
  }
}




//class ThermometerActor(thermometer: Component, targetTemperature: Double) extends Actor {
//  def receive = {
//    case tick: String => {
//      (ComponentManager.readTemperature(thermometer), thermometer.cancellable) match {
//        case (Some(currentTemp), Some(cancellable)) => {
//          if (currentTemp >= targetTemperature) cancellable.cancel()
//        }
//      }
//    }
//    case _ => println("unknown message")
//  }
//}
