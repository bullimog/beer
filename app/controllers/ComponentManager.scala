package controllers

import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, Cancellable, Props, Actor}
import async.BeerAppActorSystem._
import connector.DeviceConnector
import model._

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global

/***********************************************************************
 ComponentManager: abstract base class
***********************************************************************/
abstract class ComponentManager{
  def on(componentCollection: ComponentCollection, component:Component)
  def off(componentCollection: ComponentCollection, component:Component)
  def isOn(component:Component):Boolean
  //def pause(component:Component)
  //def resume(component:Component)
  def componentFromId(componentCollection:ComponentCollection, id:Int):Component
  def reachedTemperatureHeating(component:Component, targetTemperature: Double):Boolean
  def readTemperature(component:Component): Option[Double]
//  def waitTime(component:Component, duration: Int)
//  def getTime(component:Component): Int
  def reachedCount(component:Component, targetCount: Int):Boolean
  def readCount(component:Component): Option[Int]
  def resetCount(component: Component):Unit

  def setPower(component:Component, power: Int)
  def getPower(component:Component):Option[Int]
  def setThermostatHeat(componentCollection:ComponentCollection, thermostat: Monitor, temperature:Double)
  def stopThermostats()
  def initThermostats(componentCollection: ComponentCollection): Unit
  def getThermostatHeat(thermostat: Monitor):Double
  def getThermostatEnabled(thermostat: Monitor):Boolean
  def setThermostatEnabled(componentCollection:ComponentCollection, thermostat: Monitor, enabled:Boolean)


    var cancellable:Option[Cancellable] = None
  var actorRef:ActorRef = null
  def getComponentFromList(step:Step, componentList:List[Component]):Component

  //function to find the item of Equipment, for the given step
  def getComponentFromCollection(step:Step, componentCollection:ComponentCollection):Component

  // (Thermometer, Heater, thermostatRequiredTemperature, ThermostatEnabled)
  var thermostats:mutable.MutableList[(Component, Component, Double, Boolean)] = mutable.MutableList()

}

/***********************************************************************
 ComponentManagerK8055: sub-trait
***********************************************************************/
trait BrewComponentManager extends ComponentManager{
  val deviceConnector:DeviceConnector

  override def on(componentCollection: ComponentCollection, component:Component) = {
    println(component.description+ " switched on")
    component.deviceType match{
      case Component.DIGITAL_OUT =>
        component match{
          case d:Device => deviceConnector.setDigitalOut(d.port, true)
        }
    }
  }

  override def off(componentCollection: ComponentCollection, component:Component) = {
    println(component.description+ " switched off")
    component match{
      case d:Device => deviceConnector.setDigitalOut(d.port, false)
      case t:Monitor => setThermostatEnabled(componentCollection, t, false)
    }
  }
  override def isOn(component:Component):Boolean = {
    //println(component.description+ " is being examined")
    component.deviceType match{
      case Component.DIGITAL_OUT =>
        component match{
          case d:Device => deviceConnector.getDigitalOut(d.port)
          case _ => false
        }
      case _ => false
    }
  }


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

  override def componentFromId(componentCollection:ComponentCollection, id:Int):Component = {
    //println("componentCollection="+componentCollection)
    //println("step.device="+id)
    val components:List[Component] = componentCollection.devices ::: componentCollection.thermostats
    components.filter((component:Component) => component.id == id).head
  }

  override def reachedTemperatureHeating(component:Component, targetTemperature: Double):Boolean = {
    val risingTemp:Double = readTemperature(component).getOrElse(-273)
    //println(component.description + s" comparing temperature: target $targetTemperature with readTemperature: $risingTemp ... ")
    risingTemp >= targetTemperature
  }

  override def readTemperature(component:Component): Option[Double] = {
    //println(component.description+ " read temperature")
    component.deviceType match{
      case Component.ANALOGUE_IN =>
        component match{
          case d:Device => {
            val raw:Double = deviceConnector.getAnalogueIn(d.port)
            val unrounded:Double = raw * d.conversionFactor.getOrElse(1.0) + d.conversionOffset.getOrElse(0.0)
            val roundFactor:Double = math.pow(10, d.decimalPlaces.getOrElse(0).toInt)
            Some(math.round(unrounded*roundFactor)/roundFactor)
          }
          case _ => println(" Can't read temperature, not a component"); None
        }
      case _ => println(" Can't read temperature, not an Analogue In"); None
    }
  }

  override def reachedCount(component:Component, targetCount: Int):Boolean = {
    readCount(component).getOrElse(0) >= targetCount
  }

  override def readCount(component:Component): Option[Int] = {
    //println(component.description+ " read count")
    component.deviceType match{
      case Component.DIGITAL_IN =>
        component match{
          case d:Device => Some(deviceConnector.getCount(d.port))
          case _ => println(" Can't read counter, not a component"); None
        }
      case _ => println(" Can't read counter, not a Digital In"); None
    }
  }

  override def resetCount(component: Component):Unit = {
    component.deviceType match{
      case Component.DIGITAL_IN =>
        component match{
          case d:Device => Some(deviceConnector.resetCount(d.port))
          case _ => println(" Can't reset counter, not a component")
        }
      case _ => println(" Can't reset counter, not a Digital In")
    }
  }

//  override def waitTime(component:Component, duration: Int) = {
//    k8055.setTime(duration)
//    waitTime2(component)
//  }
//
//  @tailrec
//  private def waitTime2(component:Component):Unit = {
//    println(component.description+ " waiting for "+ k8055.getTime() + " seconds..."+ "sequencer running:"+Sequencer.running)
//    if((k8055.getTime() > 0) &&  Sequencer.running) {  //TODO tight coupling with Sequencer. :(
//      Thread.sleep(1000)
//      waitTime2(component)
//    }
//  }
//
//  override def getTime(component: Component):Int = {
//    k8055.getTime()
//  }


  override def setPower(component:Component, power: Int) = {
    component.deviceType match{
      case Component.ANALOGUE_OUT =>
        component match{
          case d:Device => {
            if(deviceConnector.getAnaloguePercentageOut(d.port) != power){
              println(component.description+ " updating power to " + power + " from "+deviceConnector.getAnaloguePercentageOut(d.port))
              Some(deviceConnector.setAnaloguePercentageOut(d.port, power))
            }
          }
          case _ => println(" Can't set power, not a component")
        }
      case _ => println(" Can't set power, not an Analogue Out")
    }
  }

  override def getPower(component:Component): Option[Int] = {
    //println(component.description+ " read power")
    component.deviceType match{
      case Component.ANALOGUE_OUT =>
        component match{
          case d:Device => Some(deviceConnector.getAnaloguePercentageOut(d.port))
          case _ => println(" Can't get power, not a component"); None
        }
      case _ => println(" Can't get power, not an Analogue Out"); None
    }
  }

  /*****************************************************************
    Thermostat Methods
  ******************************************************************/
  override def initThermostats(componentCollection: ComponentCollection): Unit = {
    componentCollection.thermostats.foreach(thermostat => {
      val thermometer = componentFromId(componentCollection, thermostat.thermometer)
      val heater = componentFromId(componentCollection, thermostat.heater)
      setThermostat(thermometer, heater, 0.0, false)
    })
  }

  override def setThermostatHeat(componentCollection:ComponentCollection, thermostat: Monitor, temperature:Double) = {
    val thermometer = componentFromId(componentCollection, thermostat.thermometer)
    val heater = componentFromId(componentCollection, thermostat.heater)
    cancellable match{
      case None => startThermostats(componentCollection)
      case _ =>
    }
    setThermostat(thermometer, heater, temperature, getThermostatEnabled(thermostat))
  }

  override def setThermostatEnabled(componentCollection:ComponentCollection, thermostat: Monitor, enabled:Boolean) = {
    val thermometer = componentFromId(componentCollection, thermostat.thermometer)
    val heater = componentFromId(componentCollection, thermostat.heater)
    val heat = getThermostatHeat(thermostat)
    thermostats = thermostats.filter(t => (t._1 != thermometer) && t._2 != heater) //remove old one
    thermostats += ((thermometer, heater, heat, enabled)) //add new one
  }

  private def addThermostat(thermometer: Component, heater: Component, targetTemperature:Double, enabled:Boolean): Unit ={
    thermostats += ((thermometer, heater, targetTemperature, enabled))
    //    println("Added thermostat..." + thermostats)
  }

  private def startThermostats(componentCollection:ComponentCollection) {
    populateThermostatList(componentCollection)
    actorRef = system.actorOf(Props(new ThermostatHeatActor(this, componentCollection)), name = "thermostat")
    val tickInterval  = new FiniteDuration(1, TimeUnit.SECONDS)
    cancellable = Some(system.scheduler.schedule(tickInterval, tickInterval, actorRef, "tick")) //initialDelay, delay, Actor, Message
  }

  private def populateThermostatList(componentCollection:ComponentCollection): Unit = {
    componentCollection.thermostats.foreach(thermostat => {
      val thermometer = componentFromId(componentCollection, thermostat.thermometer)
      val heater = componentFromId(componentCollection, thermostat.heater)
      addThermostat(thermometer, heater, -273, true) //low default target temp.
    })
  }
  private def setThermostat(thermometer: Component, heater: Component, targetTemperature:Double, enabled:Boolean): Unit = {
    thermostats = thermostats.filter(t => (t._1 != thermometer) && t._2 != heater) //remove old one (if exists)
    thermostats += ((thermometer, heater, targetTemperature, enabled)) //add new one
    //    println("set thermostats..." + thermostats)
  }

  override def getThermostatHeat(thermostat: Monitor):Double = {
    //thermostats.filter(t => (t._1.id == thermostat.thermometer) && t._2.id == thermostat.heater).head._3
    getThermostatData(thermostat)._3
  }

  override def getThermostatEnabled(thermostat: Monitor):Boolean = {
    //thermostats.filter(t => (t._1.id == thermostat.thermometer) && t._2.id == thermostat.heater).head._4
    getThermostatData(thermostat)._4
  }

  private def getThermostatData(thermostat: Monitor):(Component, Component, Double, Boolean) = {
    thermostats.filter(t => (t._1.id == thermostat.thermometer) && t._2.id == thermostat.heater).head
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
  def receive = {
    case "tick" => {
      //println("tick!")
      componentManager.thermostats.foreach( thermostat => {
        val heater = thermostat._2
        val enabled = thermostat._4
        if(enabled){
          val thermometer = thermostat._1
          val targetTemperature = thermostat._3
          componentManager.readTemperature(thermometer) match {
           case Some(currentTemp) => componentManager.setPower(heater, calculateHeatSetting(targetTemperature - currentTemp))
           case _ => componentManager.off(componentCollection, heater);println("no temperature:") //to be safe!
          }
        }
      })
    }
    case "stop" => {context.stop(self)}
    case _ => println("unknown message")
  }

  def calculateHeatSetting(tempDiff: Double): Int ={
    if(tempDiff > 2.0) 100
    else if(tempDiff < 0) 0
      else (tempDiff * 50).toInt
  }
}

