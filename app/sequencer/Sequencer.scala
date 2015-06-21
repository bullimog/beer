package sequencer

import connector.K8055
import controllers.{ComponentManagerK8055, ComponentManager}
import model._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import play.api.libs.json._

object Sequencer{

  //Some mutable state about the Sequencer
  var currentStep:Int = 0
  var running:Boolean = false
//  //function to find the (first) item of Equipment, for the given step
//  val getComponentFromList = (step:Step, componentList:List[Component]) => {
//    componentList.filter(component => component.id == step.device).head
//  }
//
//  //function to find the item of Equipment, for the given step
//  val getComponentFromCollection = (step:Step, componentCollection:ComponentCollection) => {
//    val components:List[Component] = componentCollection.devices ::: componentCollection.thermostats
//    getComponentFromList(step, components)
//  }


  def runSequence(componentManager: ComponentManager, componentCollection: ComponentCollection, sequence: Sequence):Unit = {
    Future {
      currentStep = 0
      running = true
      sequence.steps.foreach(step => {
        val component: Component = componentManager.getComponentFromCollection(step, componentCollection)
        println("step " + step + " to be serviced by " + component)
        currentStep = step.id

        (step.eventType, running) match {
          case (Step.ON, true) =>  componentManager.on(component)  //Digital Out
          case (Step.OFF, true) => componentManager.off(component) //Digital/Analogue Out
          case (Step.SET_HEAT, true) => runSetHeat(step, component, componentManager, componentCollection) //Thermostat
          case (Step.WAIT_HEAT, true) => runWaitHeat(step, component, componentManager) //Thermometer
          case (Step.WAIT_TIME, true) => runWaitTime(step, component, componentManager) //Any
          case _ => {println("Bad Step Type")} //TODO report/log
        }
      })
      running = false
    }
  }

//  def pauseSequence():Unit = {
//    Future {
//      componentCollection.devices.foreach( device => componentManager.pause(device))
//      componentCollection.thermostats.foreach( thermostat => componentManager.pause(thermostat))
//    }
//  }
//
//  def resumeSequence():Unit = {
//    Future {
//      componentCollection.devices.foreach( device => componentManager.resume(device))
//      componentCollection.thermostats.foreach( thermostat => componentManager.resume(thermostat))
//    }
//  }
//
  def abortSequence(componentManager: ComponentManager):Unit = {
    Future {
      running = false
      componentManager.stopThermostats
    }
  }

  def runSetHeat(step:Step, component:Component, componentManager: ComponentManager, componentCollection: ComponentCollection): Unit ={
//    println(s"runSetHeat on $component")
    step.temperature match {
      case Some(temperature) =>
        component match {
          case thermostat: Thermostat => componentManager.setThermostatHeat(componentCollection, thermostat, temperature)
          case _ => println("Can't set thermostat on a : " + component + "in step " + step)
        }
      case _ => println("No temperature specified,  can't set temperature for: "+step)
    }
  }


  def runWaitHeat(step:Step, component:Component, componentManager: ComponentManager): Unit ={
    step.temperature match {
      case Some(temperature) => componentManager.waitTemperatureHeating(component, temperature)
      case _ => println("No temperature specified,  can't wait for temperature for: "+step)
    }
  }

  def runWaitTime(step:Step, component:Component, componentManager: ComponentManager): Unit ={
    step.duration match {
      case Some(duration) => componentManager.waitTime(component, duration)
      case _ => println("No duration specified,  can't wait for: " + step)
    }
  }
}


//object StepIndexer {
//  var lastId: Int = 0
//
//  def getId: Int = {
//    lastId += 1
//    lastId
//  }
//}

