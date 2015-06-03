package sequencer

import controllers.ComponentManager
import model._
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import play.api.libs.json._

object Sequencer{

  /* ********************* */
  /* Read in Device Config */
  /* ********************* */
  val source = Source.fromFile("deviceSetup.json", "UTF-8")
  val json: JsValue = Json.parse(source.mkString)
  println("read json: "+Json.prettyPrint(json))

  def readComponents(json: JsValue):ComponentCollection = {
    json.validate[ComponentCollection] match {
      case s: JsSuccess[ComponentCollection] => {
        s.get
      }
      case e: JsError => null  //TODO Need better handling for this
    }
  }

  val componentCollection = readComponents(json)

  /* *********************** */
  /* Write Out Device Config */
  /* *********************** */
  val myVal = Json.toJson(componentCollection)
  val isThisIt = Json.prettyPrint(myVal)
  println("---------> toJson" + isThisIt)


  /* *********************** */
  /* Read In Step Sequence   */
  /* *********************** */

  //This method could be more generic, and less crap!!
  def readSteps(json: JsValue):Sequence = {
    json.validate[Sequence] match {
      case s: JsSuccess[Sequence] => {
        s.get
      }
      case e: JsError => println("jsError: "+e ); null  //TODO Need better handling for this
    }
  }

  val stepSource = Source.fromFile("sequence1.json", "UTF-8")
  val stepjson: JsValue = Json.parse(stepSource.mkString)
  println("read from file json: "+Json.prettyPrint(stepjson))

  val mySequence = readSteps(stepjson)
  println("mySequence: "+ mySequence)


  /* *********************** */
  /* Write Out Step Sequence */
  /* *********************** */

  val myJsonSequence = Json.toJson(mySequence)
  val prettyJson = Json.prettyPrint(myJsonSequence)
  println("---------> JsonSeq" + prettyJson)


  /* *************************************** */


  //function to find the item of Equipment, for the given step
  val getComponentFromList = (step:Step, componentList:List[Component]) => {
    componentList.filter((component:Component) => component.id == step.device).head
  }

  //function to find the item of Equipment, for the given step
  val getComponentFromCollection = (step:Step, componentCollection:ComponentCollection) => {
    val components:List[Component] = componentCollection.devices ::: componentCollection.thermostats
    getComponentFromList(step, components)
  }


  def runSequence():Unit = {
    Future {
      mySequence.steps.foreach(step => {
        val component: Component = getComponentFromCollection(step, componentCollection)
        println("step " + step + "to be serviced by " + component)

        step.eventType match {
          case Step.ON =>  ComponentManager.on(component)  //Digital Out
          case Step.OFF => ComponentManager.off(component) //Digital/Analogue Out
          case Step.SET_TEMP => runSetTemp(step, component) //Thermostat
          case Step.WAIT_TEMP => runWaitTemp(step, component) //Thermometer
          case Step.WAIT_TIME => runWaitTime(step, component) //Any
          case _ => {println("Bad Step Type")} //TODO report/log
        }
      })
    }
  }

  def pauseSequence():Unit = {
    Future {
      componentCollection.devices.foreach( device => ComponentManager.pause(device))
      componentCollection.thermostats.foreach( thermostat => ComponentManager.pause(thermostat))
    }
  }

  def resumeSequence():Unit = {
    Future {
      componentCollection.devices.foreach( device => ComponentManager.resume(device))
      componentCollection.thermostats.foreach( thermostat => ComponentManager.resume(thermostat))
    }
  }

  def abortSequence():Unit = {
    Future {}
  }

  def runSetTemp(step:Step, component:Component): Unit ={
    step.temperature match {
      case Some(temperature) =>{
        component match {
          case thermostat:Thermostat => ComponentManager.setThermostat(componentCollection, thermostat, temperature)
          case _ => println("Can't set thermostat on a : "+component + "in step "+ step)
        }
      }
      case _ => println("No temperature specified,  can't set temperature for: "+step)
    }
  }

  def runWaitTemp(step:Step, component:Component): Unit ={
    step.temperature match {
      case Some(temperature) => ComponentManager.waitTemperatureHeating(component, temperature)
      case _ => println("No temperature specified,  can't wait for temperature for: "+step)
    }
  }

  def runWaitTime(step:Step, component:Component): Unit ={
    step.duration match {
      case Some(duration) => ComponentManager.waitTime(component, duration)
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

