package sequencer

import akka.actor.Cancellable
import controllers.ComponentManager
import model._
import play.api.libs.json._
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

object Sequencer{

  //working towards json definition...
//  val json: JsValue = Json.parse("""{
//    "name": "First setup",
//    "description": "My first setup",
//    "devices": [
//      {"id" : 1, "description": "Thermometer", "deviceType": 1,  "port": 1},
//      {"id" : 2, "description": "Pump",        "deviceType": 4,  "port": 1},
//      {"id" : 3, "description": "Heater",      "deviceType": 2,  "port": 1}
//    ]
//  }""")

  val source = Source.fromFile("deviceSetup.json", "UTF-8")
  val json: JsValue = Json.parse(source.mkString)


  println("json: "+Json.prettyPrint(json))


  import play.api.libs.functional.syntax._

  implicit val thermostatReads: Reads[Thermostat] = (
    (JsPath \ "id").read[Int] and
    (JsPath \ "description").read[String] and
    (JsPath \ "deviceType").read[Int] and
    (JsPath \ "cancellable").read[Option[Cancellable]] and
    (JsPath \ "thermometer").read[Device] and
    (JsPath \ "port").read[Device]
    )(Thermostat.apply _)

  implicit val deviceReads: Reads[Device] = (
    (JsPath \ "id").read[Int] and
    (JsPath \ "description").read[String] and
    (JsPath \ "deviceType").read[Int] and
    (JsPath \ "cancellable").read[Option[Cancellable]] and
    (JsPath \ "port").read[Int]
    )(Device.apply _)

  implicit val componentCollectionReads: Reads[ComponentCollection] = (
    (JsPath \ "name").read[String] and
    (JsPath \ "description").read[String] and
    (JsPath \ "devices").read[List[Device]] and
    (JsPath \ "thermostat").read[List[Thermostat]]
    )(ComponentCollection.apply _)

  json.validate[ComponentCollection] match {
    case s: JsSuccess[ComponentCollection] => {
      val dc: ComponentCollection = s.get
      println("---------> DeviceCollection read:" + dc)
    }
    case e: JsError => {
      println("---------> Crap happened:" + e)
    }
  }



//  val deviceResult: JsResult[Device] = json.validate[Device]
//  println("deviceResult: "+deviceResult)


/* ----------------------------------------------------------- */

  // Static device definition for now....
  val device1 = Device(1,"Thermometer", Component.ANALOGUE_IN, None, 1)
  val device2 = Device(2, "Pump", Component.DIGITAL_OUT, None, 1)
  val device3 = Device(3, "Heater", Component.ANALOGUE_OUT, None, 1)

  val lb = new ListBuffer[Component]()
  lb += device1 += device2 += device3
  val devices = lb.toList

  val thermo = Thermostat(4, "Boiler", Component.MONITOR, None, device1, device3)
  val thermos = List(thermo)

  val componentCollection = ComponentCollection ("Masher 1", "My first set-up, for mashing", devices, thermos)
  println("componentCollection created: " + componentCollection)



  /*-------------------------------------------------*/

  import play.api.libs.json._
  implicit val deviceWrites = Json.writes[Device]
  implicit val thermostatWrites = Json.writes[Thermostat]
  implicit val componentCollectionWrites = Json.writes[ComponentCollection]

  val isThisIt = Json.toJson(componentCollectionWrites)
  println("---------> toJson" + isThisIt)

  /*-------------------------------------------------*/






  // Static step definition for now....
  //  val step  = Step(device, stepType, temp, duration)
  val stepDefn1 = Step(104, Step.SET_TEMP, Some(41), None)      // Set required thermostat temp to 41
  val stepDefn2 = Step(2, Step.ON, None, None)                // Turn pump on
  val stepDefn3 = Step(1, Step.WAIT_TEMP, Some(41), None)     // Wait for thermometer to reach 41
  val stepDefn4 = Step(104, Step.WAIT_TIME, None, Some(60*20))  // wait for 20 minutes
  val stepDefn5 = Step(104, Step.SET_TEMP, Some(68), None)      // Set thermostat temp to 68
  val stepDefn6 = Step(104, Step.WAIT_TIME, None, Some(60*20))  // wait for 20 minutes
  val stepDefn7 = Step(104, Step.OFF, None, None)               // turn boiler off
  val stepDefn8 = Step(2, Step.OFF, None, None)               // turn pump off

  val p = new ListBuffer[Step]()
  p += stepDefn1 += stepDefn2 += stepDefn3 += stepDefn4 += stepDefn5 += stepDefn6 += stepDefn7 += stepDefn8
  val mySequence = p.toList
  println("sequences created: " + p)



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
      mySequence.foreach(step => {
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
          case thermostat:Thermostat => ComponentManager.setThermostat(thermo,temperature)
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

