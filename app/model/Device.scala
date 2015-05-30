package model

import java.util.concurrent.TimeUnit

import akka.actor.{Cancellable, Props, Actor}
import org.joda.time.DateTime
import async.BeerAppActorSystem.system
import play.api.libs.json.Json
import sequencer.Sequencer

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration


case class Device(val id: Int, val description: String, val deviceType: Int,
//             val port:Option[Int], thermometer:Option[Device], heater:Option[Device]) {
             val port:Option[Int]) {

  var cancellable:Cancellable = null
  override def toString(): String ={
    "## Device:"+id+", description:"+description+", deviceType:"+deviceType+", port:"+port
  }

  def on() = {println(description+ " switched on")} //TODO
  def off() = {println(description+ " switched off")} //TODO
  def pause() = {}  //TODO
  def resume() = {} //TODO

  def setThermostat(temperature:Double) = {}

  @tailrec
  final def waitTemperatureHeating(targetTemperature: Double):Unit = {
    print(description + " waiting for temperature: " + targetTemperature + "... ")
    val risingTemp:Double = readTemperature().getOrElse(-273)
    if (risingTemp < targetTemperature) {
      Thread.sleep(1000)
      waitTemperatureHeating(targetTemperature)
    }
  }







//    println(" Done!")
//    val thermostat = system.actorOf(Props(new ThermometerActor(this, temperature)), name = "thermometer")
//    val tickInterval  = new FiniteDuration(1, TimeUnit.SECONDS)
//    //cancellable = system.scheduler.schedule(tickInterval, tickInterval, thermostat, "tick") //initialDelay, delay, Actor, Message
//    cancellable = system.scheduler.scheduleOnce(tickInterval, thermostat, "foo")
//    //Will need an Actor, to periodically check
//  }

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

//  def apply(id: Int, description: String, deviceType: Int, port:Option[Int]): Device ={
//    //TODO: validate values
//    new Device(id, description, deviceType, port)
//  }

//  implicit val formats=Json.format[Device]
}

class ThermometerActor(thermometer: Device, targetTemperature: Double) extends Actor {
  def receive = {
    case tick: String => {
       thermometer.readTemperature() match {
        case Some(currentTemp) => if(currentTemp == targetTemperature) thermometer.cancellable.cancel()
        case _ =>
      }
    }
    case _ => println("unknown message")
  }
}