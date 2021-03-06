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
  def componentFromId(componentCollection:ComponentCollection, id:Int):Option[Component]
  def deviceFromId(componentCollection:ComponentCollection, id:Int):Option[Device]

  def reachedTargetIncreasing(component:Component, targetTemperature: Double):Boolean
  def reachedTargetDecreasing(component:Component, targetTemperature: Double):Boolean
  def readSensor(component:Component): Option[Double]
//  def waitTime(component:Component, duration: Int)
//  def getTime(component:Component): Int
  def reachedCount(component:Component, targetCount: Int):Boolean
  def readCount(component:Component): Option[Int]
  def resetCount(component: Component):Unit

  def setPower(component:Component, power: Int)
  def getPower(component:Component):Option[Int]
  def setMonitorTarget(componentCollection:ComponentCollection, monitor: Monitor, temperature:Double)
  def stopMonitors()
  def initMonitors(componentCollection: ComponentCollection): Unit
  def getMonitorTarget(monitor: Monitor):Double
  def getMonitorEnabled(monitor: Monitor):Boolean
  def setMonitorEnabled(componentCollection:ComponentCollection, monitor: Monitor, enabled:Boolean)


  var cancellable:Option[Cancellable] = None
  var actorRef:ActorRef = null
  def getComponentFromList(step:Step, componentList:List[Component]):Option[Component]

  //function to find the item of Equipment, for the given step
  def getComponentFromCollection(step:Step, componentCollection:ComponentCollection):Option[Component]

  // (sensor, Increaser, monitorTarget, MonitorEnabled)
  var monitors:mutable.MutableList[(Component, Component, Double, Boolean)] = mutable.MutableList()

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
      case t:Monitor => setMonitorEnabled(componentCollection, t, false)
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
      case Component.DIGITAL_IN =>
        component match{
          case d:Device => deviceConnector.getDigitalIn(d.port)
          case _ => false
        }
      case _ => false
    }
  }

  //function to find the (first) item of Equipment, for the given step
  override def getComponentFromList(step:Step, componentList:List[Component]):Option[Component] = {
    try{Some(componentList.filter(component => component.id == step.device).head)}
    catch{
      case e:NoSuchElementException => None
    }
  }

  //function to find the item of Equipment, for the given step
  override def getComponentFromCollection(step:Step, componentCollection:ComponentCollection):Option[Component] = {
    val components:List[Component] = componentCollection.devices ::: componentCollection.monitors
    getComponentFromList(step, components)
  }

//  val deviceFromIdFn = (componentCollection:ComponentCollection, id:Int) => {
//    componentCollection.devices.filter((device:Device) => device.id == id).head
//  }

  override def componentFromId(componentCollection:ComponentCollection, id:Int):Option[Component] = {
    //println("componentCollection="+componentCollection)
    //println("step.device="+id)
    val components:List[Component] = componentCollection.devices ::: componentCollection.monitors
    try{Some(components.filter((component:Component) => component.id == id).head)}
    catch{
      case e:NoSuchElementException => None
    }
  }


  override def deviceFromId(componentCollection:ComponentCollection, id:Int):Option[Device] = {
    try{Some(componentCollection.devices.filter((device:Device) => device.id == id).head)}
    catch{
      case e:NoSuchElementException => None
    }
  }

  override def reachedTargetIncreasing(component:Component, targetReading: Double):Boolean = {
    val risingReading:Double = readSensor(component).getOrElse(Double.MinValue)
    //println(component.description + s" comparing temperature: target $targetTemperature with readTemperature: $risingTemp ... ")
    risingReading >= targetReading
  }

  override def reachedTargetDecreasing(component:Component, targetReading: Double):Boolean = {
    val fallingReading:Double = readSensor(component).getOrElse(Double.MaxValue)
    fallingReading <= targetReading
  }

  override def readSensor(component:Component): Option[Double] = {
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
    Monitor Methods
  ******************************************************************/
  override def initMonitors(componentCollection: ComponentCollection): Unit = {
    componentCollection.monitors.foreach(monitor => {
      val sensor = componentFromId(componentCollection, monitor.sensor)
      val increaser = componentFromId(componentCollection, monitor.increaser)
      (sensor, increaser) match {
        case (Some(s), Some(i)) => setMonitor(s, i, 0.0, false)
        case (_,_) =>
      }

    })
  }

  override def setMonitorTarget(componentCollection:ComponentCollection, monitor: Monitor, target:Double) = {
    val sensor = componentFromId(componentCollection, monitor.sensor)
    val increaser = componentFromId(componentCollection, monitor.increaser)
    cancellable match{
      case None => startMonitors(componentCollection)
      case _ =>
    }
    (sensor, increaser) match {
      case (Some(s), Some(i)) => setMonitor(s, i, target, getMonitorEnabled(monitor))
      case (_,_) =>
    }
  }

  override def setMonitorEnabled(componentCollection:ComponentCollection, monitor: Monitor, enabled:Boolean) = {
    val sensor:Option[Component] = componentFromId(componentCollection, monitor.sensor)
    val increaser:Option[Component] = componentFromId(componentCollection, monitor.increaser)
    val target = getMonitorTarget(monitor)

    (sensor, increaser) match {
      case (Some(s), Some(i)) => {
        monitors = monitors.filter(t => (t._1 != s) && t._2 != i) //remove old one
        monitors += ((s, i, target, enabled)) //add new one
      }
      case (_,_) =>
    }
  }

  private def addMonitor(sensor: Component, increaser: Component, target:Double, enabled:Boolean): Unit ={
    monitors += ((sensor, increaser, target, enabled))
  }

  private def startMonitors(componentCollection:ComponentCollection) {
    populateMonitorList(componentCollection)
    actorRef = system.actorOf(Props(new MonitorActor(this, componentCollection)), name = "monitor")
    val tickInterval  = new FiniteDuration(1, TimeUnit.SECONDS)
    cancellable = Some(system.scheduler.schedule(tickInterval, tickInterval, actorRef, "tick")) //initialDelay, delay, Actor, Message
  }

  private def populateMonitorList(componentCollection:ComponentCollection): Unit = {
    componentCollection.monitors.foreach(monitor => {
      val sensor = componentFromId(componentCollection, monitor.sensor)
      val increaser = componentFromId(componentCollection, monitor.increaser)

      (sensor, increaser) match {
        case (Some(s), Some(i)) => addMonitor(s, i, -1000, true) //low default target.
        case (_,_) =>
      }
    })
  }
  private def setMonitor(sensor: Component, increaser: Component, targetTemperature:Double, enabled:Boolean): Unit = {
    monitors = monitors.filter(t => (t._1 != sensor) && t._2 != increaser) //remove old one (if exists)
    monitors += ((sensor, increaser, targetTemperature, enabled)) //add new one
  }

  override def getMonitorTarget(monitor: Monitor):Double = {  //getMonitorData(monitor)._3
    getMonitorData(monitor) match{
      case Some((_, _, target, _)) => target
      case _ => 0.0
    }
  }

  override def getMonitorEnabled(monitor: Monitor):Boolean = {  //getMonitorData(monitor)._4
    getMonitorData(monitor) match{
      case Some((_, _, _, enabled)) => enabled
      case _ => false
    }
  }

  private def getMonitorData(monitor: Monitor):Option[(Component, Component, Double, Boolean)] = {
    try{Some(monitors.filter(t => (t._1.id == monitor.sensor) && t._2.id == monitor.increaser).head)}
    catch{
      case e:NoSuchElementException => None
    }
  }


  override def stopMonitors()={
    actorRef ! "stop"
    //cancellable = None
  }
}

/***********************************************************************
 MonitorActor: Akka Actor
***********************************************************************/
class MonitorActor(componentManager: ComponentManager, componentCollection: ComponentCollection) extends Actor {
  def receive = {
    case "tick" => {
      //println("tick!")
      componentManager.monitors.foreach( monitor => {
        val increaser = monitor._2
        val enabled = monitor._4
        if(enabled){
          val sensor = monitor._1
          val targetTemperature = monitor._3

          //Using fold, instead of match... This compiles, but not tested it yet...
//          componentManager.setPower(increaser, calculateOutputSetting(targetTemperature - componentManager.readSensor(sensor).fold(0.0) (_ + 0.0)))

          componentManager.readSensor(sensor) match {
           case Some(currentTemp) => componentManager.setPower(increaser, calculateOutputSetting(targetTemperature - currentTemp))
           case _ => componentManager.off(componentCollection, increaser);println("no sensor reading:") //to be safe!
          }
        }
      })
    }
    case "stop" => {context.stop(self)}
    case _ => println("unknown message")
  }

  def calculateOutputSetting(tempDiff: Double): Int ={
    if(tempDiff > 2.0) 100
    else if(tempDiff < 0) 0
      else (tempDiff * 50).toInt
  }
}

