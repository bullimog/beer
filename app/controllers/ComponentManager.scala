package controllers

import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, Cancellable, Props, Actor}
import async.BeerAppActorSystem._
import connector.K8055
import model._
import sequencer.Sequencer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global

/***********************************************************************
 ComponentManager: abstract base trait
***********************************************************************/
trait ComponentManager{
  def on(component:Component)
  def off(component:Component)
  def isOn(component:Component):Boolean
  def pause(component:Component)
  def resume(component:Component)
  def deviceFromId(componentCollection:ComponentCollection, id:Int):Component
  def waitTemperatureHeating(component:Component, targetTemperature: Double):Unit
  def readTemperature(component:Component): Option[Double]
  def waitTime(component:Component, duration: Int)
  def setPower(component:Component, power: Int)
  def getPower(component:Component):Option[Int]
  def setThermostatHeat(componentCollection:ComponentCollection, thermostat: Thermostat, temperature:Double)
  def stopThermostats()

  var cancellable:Option[Cancellable] = None
  var actorRef:ActorRef = null
  def getComponentFromList(step:Step, componentList:List[Component]):Component

  //function to find the item of Equipment, for the given step
  def getComponentFromCollection(step:Step, componentCollection:ComponentCollection):Component
}

/***********************************************************************
 ComponentManagerK8055: sub-trait
***********************************************************************/
trait ComponentManagerK8055 extends ComponentManager{

  val k8055:K8055

  override def on(component:Component) = {
    println(component.description+ " switched on")
    component.deviceType match{
      case Component.DIGITAL_OUT =>
        component match{
          case d:Device => k8055.setDigitalOut(d.port, true)
        }
    }
  }

  override def off(component:Component) = {
    println(component.description+ " switched off")
    component match{
      case d:Device => k8055.setDigitalOut(d.port, false)
      case _ =>
    }
  }
  override def isOn(component:Component):Boolean = {
    println(component.description+ " is being examined")
    component.deviceType match{
      case Component.DIGITAL_OUT =>
        component match{
          case d:Device => k8055.getDigitalOut(d.port)
          case _ => false
        }
      case _ => false
    }
  }

  //Switch all components (pump heater) off. Remember state...

  override def pause(component:Component) = {}  //TODO


  override def resume(component:Component) = {} //TODO

  //function to find the (first) item of Equipment, for the given step
  override def getComponentFromList(step:Step, componentList:List[Component]):Component = {
    componentList.filter(component => component.id == step.device).head
  }

  //function to find the item of Equipment, for the given step
  override def getComponentFromCollection(step:Step, componentCollection:ComponentCollection):Component = {
    val components:List[Component] = componentCollection.devices ::: componentCollection.thermostats
    getComponentFromList(step, components)
  }

//  val deviceFromIdFn = (componentCollection:ComponentCollection, id:Int) => {
//    componentCollection.devices.filter((device:Device) => device.id == id).head
//  }

  override def deviceFromId(componentCollection:ComponentCollection, id:Int):Component = {
    println("componentCollection="+componentCollection)
    println("step.device="+id)
    componentCollection.devices.filter((device:Device) => device.id == id).head
  }

  @tailrec
  override final def waitTemperatureHeating(component:Component, targetTemperature: Double):Unit = {
    val risingTemp:Double = readTemperature(component).getOrElse(-273)
    println(component.description + s" comparing temperature: target $targetTemperature with readTemperature: $risingTemp ... ")
    if ((risingTemp < targetTemperature) && Sequencer.running) {
      Thread.sleep(1000)
      waitTemperatureHeating(component, targetTemperature)
    }
  }

  override def readTemperature(component:Component): Option[Double] = {
    //println(component.description+ " read temperature")
    component.deviceType match{
      case Component.ANALOGUE_IN =>
        component match{
          case d:Device => Some(k8055.getAnalogueIn(d.port))
          case _ => println(" Can't read temperature, not a component"); None
        }
      case _ => println(" Can't read temperature, not an Analogue In"); None
    }
  }

  @tailrec
  final override def waitTime(component:Component, duration: Int) = {
    println(component.description+ " waiting for "+ duration + " seconds...")
    if((duration > 0) &&  Sequencer.running) {
      Thread.sleep(1000)
      waitTime(component, duration - 1)
    }
  }

  override def setPower(component:Component, power: Int) = {

    component.deviceType match{
      case Component.ANALOGUE_OUT =>
        component match{
          case d:Device => {
            if(k8055.getAnalogueOut(d.port) != power){
              println(component.description+ " updating power to " + power + " from "+k8055.getAnalogueOut(d.port))
              Some(k8055.setAnalogueOut(d.port, power))
            }
          }
          case _ => println(" Can't set power, not a component")
        }
      case _ => println(" Can't set power, not an Analogue Out")
    }
  }

  override def getPower(component:Component): Option[Int] = {
    println(component.description+ " read power")
    component.deviceType match{
      case Component.ANALOGUE_OUT =>
        component match{
          case d:Device => Some(k8055.getAnalogueOut(d.port))
          case _ => println(" Can't get power, not a component"); None
        }
      case _ => println(" Can't get power, not an Analogue Out"); None
    }
  }

  override def setThermostatHeat(componentCollection:ComponentCollection, thermostat: Thermostat, temperature:Double) = {
    val thermometer = deviceFromId(componentCollection, thermostat.thermometer)
    val heater = deviceFromId(componentCollection, thermostat.heater)
    cancellable match{
      case None => startThermostats(componentCollection)
      case _ =>
    }
    actorRef ! (thermometer, heater, temperature)
  }

  def startThermostats(componentCollection:ComponentCollection) {
    actorRef = system.actorOf(Props(new ThermostatHeatActor(this, componentCollection)), name = "thermostat")
    val tickInterval  = new FiniteDuration(1, TimeUnit.SECONDS)
    cancellable = Some(system.scheduler.schedule(tickInterval, tickInterval, actorRef, "tick")) //initialDelay, delay, Actor, Message
  }

  override def stopThermostats()={
    actorRef ! "stop"
    //cancellable = None
  }
}

/***********************************************************************
 ThermostatHeatActor: Akka Actor
***********************************************************************/
class ThermostatHeatActor(componentManager: ComponentManager, componentCollection: ComponentCollection) extends Actor {

  // List(thermometer, heater, targetTemp, enabled)
  var thermostats:mutable.MutableList[(Component, Component, Double, Boolean)] = mutable.MutableList()

  //populate thermostat List
//  println("Constructing the ThermostatHeatActor...")
  componentCollection.thermostats.foreach(thermostat =>{
    val thermometer = componentManager.deviceFromId(componentCollection, thermostat.thermometer)
    val heater = componentManager.deviceFromId(componentCollection, thermostat.heater)
    addThermostat(thermometer, heater, -273, true)  //low default target temp.
  })

  private def addThermostat(thermometer: Component, heater: Component, targetTemperature:Double, enabled:Boolean): Unit ={
    thermostats += ((thermometer, heater, targetTemperature, enabled))
//    println("Added thermostat..." + thermostats)
  }

  private def setThermostat(thermometer: Component, heater: Component, targetTemperature:Double): Unit = {
    thermostats = thermostats.filter(t => (t._1 != thermometer) && t._2 != heater) //remove old one
    thermostats += ((thermometer, heater, targetTemperature, true)) //add new one
    //    println("set thermostats..." + thermostats)
  }

  private def disableThermostat(thermometer: Component, heater: Component): Unit ={
    thermostats = thermostats.filter(t => (t._1 != thermometer) && t._2 != heater) //remove old one
    thermostats += ((thermometer, heater, -273, false)) //add new one
  }

  def receive = {
    case "tick" => {
      thermostats.foreach( thermostat => {
        val heater = thermostat._2
        val enabled = thermostat._4
        if(enabled){
          val thermometer = thermostat._1
          val targetTemperature = thermostat._3

          componentManager.readTemperature(thermometer) match {
           case Some(currentTemp) => componentManager.setPower(heater, calculateHeatSetting(targetTemperature - currentTemp))
           case _ => componentManager.off(heater) //to be safe!
          }
        }else componentManager.off(heater)
      })
    }
    case (thermometer:Component, heater:Component, temperature:Double) => {setThermostat(thermometer, heater, temperature)}
    case (thermometer:Component, heater:Component, enabled:Boolean) => {disableThermostat(thermometer, heater)}
    case "stop" => {context.stop(self)}
    case _ => println("unknown message")
  }

  def calculateHeatSetting(tempDiff: Double): Int ={
    if(tempDiff > 2.0) 100
    else if(tempDiff < 0) 0
      else (tempDiff * 50).toInt
  }
}

