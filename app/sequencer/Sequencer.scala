package sequencer

import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, Props, Actor}
import async.BeerAppActorSystem._
import controllers.ComponentManager
import model._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.io.Source
import play.api.libs.json._

object Sequencer{

  //Some mutable state about the Sequencer
  val START_STEP = 0
  var currentStep:Int = START_STEP
  var running:Boolean = false
  var actorRef:ActorRef = null

  def runSequence(componentManager: ComponentManager, componentCollection: ComponentCollection, sequence: Sequence):Unit = {
    currentStep = 1
    running = true
    actorRef = system.actorOf(Props(new SequencerActor(sequence, componentManager, componentCollection)), name = "sequencer")
    val tickInterval  = new FiniteDuration(1, TimeUnit.SECONDS)
    val cancellable = Some(system.scheduler.schedule(tickInterval, tickInterval, actorRef, "sequence")) //initialDelay, delay, Actor, Message
  }


  def abortSequence(componentManager: ComponentManager):Unit = {
      running = false
      //componentManager.stopThermostats()
      Timer.reset()
      actorRef ! "stop"
      Sequencer.currentStep = Sequencer.START_STEP
  }

  def runSetHeat(step:Step, component:Component, componentManager: ComponentManager, componentCollection: ComponentCollection): Unit ={
//    println(s"runSetHeat on $component")
    step.temperature match {
      case Some(temperature) =>
        component match {
          case thermostat: Thermostat => {
              componentManager.setThermostatHeat(componentCollection, thermostat, temperature)
            componentManager.setThermostatEnabled(componentCollection, thermostat, true)
          }
          case _ => println("Can't set thermostat on a : " + component + "in step " + step)
        }
      case _ => println("No temperature specified,  can't set temperature for: "+step)
    }
  }


  def runWaitHeat(step:Step, component:Component, componentManager: ComponentManager): Unit ={
    step.temperature match {
      case Some(temperature) => {
        if (componentManager.reachedTemperatureHeating(component, temperature)) currentStep +=1
      }
      case _ => println("No temperature specified,  can't wait for temperature for: "+step)
    }
  }

  def runWaitTime(step:Step, component:Component): Unit ={
    if(Timer.waitingFor(step.id)) {
      if(Timer.finished(step.id)){currentStep += 1}
    }
    else{
      step.duration match {
        case Some (duration) => { Timer.setTimer( step.id, step.duration.getOrElse(0))}
        case _ => println ("No duration specified,  can't wait for: " + step)
      }
    }
  }

  def runWaitOn(step:Step, component:Component, componentManager: ComponentManager): Unit ={
    step.duration match {
      case Some(targetCount) => {
        if (componentManager.reachedCount(component, targetCount)) {
          currentStep +=1
          componentManager.resetCount(component)
        }
      }
      case _ => println("No count specified,  can't wait for temperature for: "+step)
    }
  }
}

/***********************************************************************
 SequencerActor: Akka Actor
 ***********************************************************************/
class SequencerActor(sequence: Sequence, componentManager: ComponentManager, componentCollection: ComponentCollection) extends Actor {
  def receive = {
    case "sequence" => {
      val maybeStep:Option[Step] = getStepFromList(Sequencer.currentStep, sequence.steps)
      maybeStep match{
        case Some(step) => performStep(step)
        case None => {
          context.stop(self)
          Sequencer.currentStep = Sequencer.START_STEP
          Sequencer.running = false
        }
      }
    }
    case "stop" => {
      context.stop(self)
      Sequencer.currentStep = Sequencer.START_STEP
    }
    case _ => println("unknown message")  //TODO report/log
  }

  def getStepFromList(id:Int, stepList:List[Step]):Option[Step] = {
    val steps:List[Step] = stepList.filter(step => step.id == id)
    steps match {
      case List() => None
      case _ => Some(steps.head)
    }
  }

  def performStep(step: Step): Unit ={
    val component: Component = componentManager.getComponentFromCollection(step, componentCollection)
    //println("step " + step + " to be serviced by " + component)

    step.eventType match {
      case (Step.ON) => componentManager.on(componentCollection, component); Sequencer.currentStep += 1 //Digital Out
      case (Step.OFF) => componentManager.off(componentCollection, component); Sequencer.currentStep += 1 //Digital/Analogue Out/Monitor
      case (Step.SET_HEAT) => {
        //Thermostat
        Sequencer.runSetHeat(step, component, componentManager, componentCollection)
        Sequencer.currentStep += 1
      }
      case (Step.WAIT_HEAT) => Sequencer.runWaitHeat(step, component, componentManager) //Thermometer
      case (Step.WAIT_TIME) => Sequencer.runWaitTime(step, component) //Any
      case (Step.WAIT_ON) => Sequencer.runWaitOn(step, component, componentManager) //Any
      case _ => println("Bad Step Type")//TODO report/log
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

