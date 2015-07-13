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

  def skipStep() = {currentStep += 1}
  def backStep() = {currentStep -= 1}


  def abortSequence(componentManager: ComponentManager):Unit = {
      running = false
      Timer.reset()
      actorRef ! "stop"
      Sequencer.currentStep = Sequencer.START_STEP
  }



  def runSetHeat(step:Step, component:Component, componentManager: ComponentManager, componentCollection: ComponentCollection): Unit ={
//    println(s"runSetHeat on $component")
    step.target match {
      case Some(target) =>
        component match {
          case monitor: Monitor => {
              componentManager.setMonitorTarget(componentCollection, monitor, target)
            componentManager.setMonitorEnabled(componentCollection, monitor, true)
          }
          case device:Device => {
            if(device.deviceType == Component.ANALOGUE_OUT){
              componentManager.setPower(component, target.toInt)
            }
          }
          case _ => println("Can't set monitor on a : " + component + "in step " + step)
        }
      case _ => println("No temperature specified,  can't set temperature for: "+step)
    }
  }


  def runWaitHeat(step:Step, component:Component, componentManager: ComponentManager): Unit ={
    step.target match {
      case Some(target) => {
        if (componentManager.reachedTemperatureHeating(component, target)) currentStep +=1
      }
      case _ => println("No target specified,  can't wait for target value for: "+step)
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
      case _ => println("No count specified,  can't wait for: "+step)
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
        case None => {  //End of Sequence
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
      case (Step.SET_HEAT) => { //Monitor or Analogue Out
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

