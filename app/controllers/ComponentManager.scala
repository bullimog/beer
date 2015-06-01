package controllers

import java.util.concurrent.TimeUnit

import akka.actor.{Props, Actor}
import async.BeerAppActorSystem._
import model.{Thermostat, Component}
import org.joda.time.DateTime

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global

object ComponentManager {

  def on(component:Component) = {println(component.description+ " switched on")} //TODO
  def off(component:Component) = {println(component.description+ " switched off")} //TODO
  def pause(component:Component) = {}  //TODO
  def resume(component:Component) = {} //TODO


  @tailrec
  final def waitTemperatureHeating(component:Component, targetTemperature: Double):Unit = {
    print(component.description + " waiting for temperature: " + targetTemperature + "... ")
    val risingTemp:Double = readTemperature(component).getOrElse(-273)
    if (risingTemp < targetTemperature) {
      Thread.sleep(1000)
      waitTemperatureHeating(component, targetTemperature)
    }
  }

  def readTemperature(component:Component): Option[Double] = {
    println(component.description+ " read temperature")
    None
  }

  def waitTime(component:Component, duration: Int) = {
    print(component.description+ " waiting for "+ duration + " seconds...")
    Thread.sleep(5000)
    println(" Done!")
  }

  def setPower(component:Component, power: Int) = {println(component.description+ " set power to " + power + "%")}



  def setThermostat(thermostat: Thermostat, temperature:Double) = {
    //Thermostat setting
    //Start Akka Actor, to adjust element, according to temperature
    //val scheduler = system.actorOf(Props[BoilerActor], name = "scheduler")
    val actorRef = system.actorOf(Props(new ThermostatActor(thermostat.thermometer , thermostat.heater, temperature)), name = "thermostat")
    val tickInterval  = new FiniteDuration(1, TimeUnit.SECONDS)
    thermostat.cancellable = Some(system.scheduler.schedule(tickInterval, tickInterval, actorRef, "tick")) //initialDelay, delay, Actor, Message
    println(thermostat.description+ " set thermostat to "+ temperature)
  }
}


class ThermostatActor(thermometer: Component, heater: Component, targetTemperature: Double) extends Actor {
  def receive = {
    case tick: String => {
      println("still going " + DateTime.now)
      ComponentManager.readTemperature(thermometer) match {
        case Some(currentTemp) => ComponentManager.setPower(heater, calculateHeatSetting(targetTemperature - currentTemp))
        case _ => ComponentManager.off(heater) //to be safe!
      }
    }
    case _ => println("unknown message")
  }

  def calculateHeatSetting(tempDiff: Double): Int ={
    if(tempDiff > 2.0) 100
    else (tempDiff * 50).toInt
  }
}

class ThermometerActor(thermometer: Component, targetTemperature: Double) extends Actor {
  def receive = {
    case tick: String => {
      (ComponentManager.readTemperature(thermometer), thermometer.cancellable) match {
        case (Some(currentTemp), Some(cancellable)) => {
          if (currentTemp >= targetTemperature) cancellable.cancel()
        }
      }
    }
    case _ => println("unknown message")
  }
}
