package sequencer

import akka.actor.Actor
import equipment.{Equipment, Pump, Boiler}
import model.{Device, Step}

import scala.collection.mutable.ListBuffer


//class SequencerActor(taskList: List[Step]) extends Actor {
//  def receive = {
//    case message: String => {
//      for (step:Step <- taskList)
//        yield {
//          println(step)
//          step.execute()
//        }
//    }
//    case _ => println("unknown message")
//  }
//}

object Sequencer{

  // Static definition for now....
  val stepDefn1 = Step(4, 3, Some(41), None)          // Set required boiler temp to 41
  val stepDefn2 = Step(2, 1, None, None)              // Turn pump on
  val stepDefn3 = Step(1, 4, Some(41), None)          // Wait for thermometer to reach 41
  val stepDefn4 = Step(4, 5, None, Some(60*20))       // wait for 20 minutes
  val stepDefn5 = Step(4, 3, Some(68), None)          // Set boiler temp to 68
  val stepDefn6 = Step(4, 5, None, Some(60*20))       // wait for 20 minutes
  val stepDefn7 = Step(4, 2, None, None)              // turn boiler off
  val stepDefn8 = Step(2, 2, None, None)               // turn pump off

  //  val step = ("device", "stepType", "meta_data")

  val p = new ListBuffer[Step]()
  p += stepDefn1
  p += stepDefn2
  p += stepDefn3
  p += stepDefn4
  p += stepDefn5
  p += stepDefn6
  p += stepDefn7
  p += stepDefn8
  val mySequence = p.toList
  println("sequences created: " + p)


  val device1 = Device(1,"Thermometer", Device.ANALOGUE_IN, 1)
  val device2 = Device(2, "Pump", Device.DIGITAL_OUT, 1)
  val device3 = Device(3, "Heater", Device.ANALOGUE_OUT, 1)
  val device4 = Device(4, "Boiler", Device.MONITOR, 0)

  val d = new ListBuffer[Device]()
  d += device1
  d += device2
  d += device3
  d += device4
  val devices = d.toList
  println("devices created: " + d)

  //function to find the item of Equipment, for the given step
  val getEquipment = (step:Step, deviceList:List[Device]) => {
    deviceList.filter((device:Device) => device.id == step.device).head
  }

//  println("getEquipment created: " + getEquipment)
//  println("Looking for equipment in: " + devices)


  mySequence.foreach( step => {
//    println("looking for equipment for : " + step)
    val device :Device = getEquipment(step, devices)
    println("step " + step + "to be serviced by "+ device )


    step.eventType match{
      case Step.ON => {device.on()}                       //Digital Out
      case Step.OFF => {device.off()}                     //Digital Out
      case Step.SET_TEMP => {                             //Boiler
        step.temperature match {
          case Some(temperature) => device.setThermostat (temperature)
          case _ => println("No temperature specified,  can't set temperature for: "+step)
        }
      }
      case Step.WAIT_TEMP => {                            //Thermometer
        step.temperature match {
          case Some(temperature) => device.waitTemperature(temperature)
          case _ => println("No temperature specified,  can't wait for temperature for: "+step)
        }
      }
      case Step.WAIT_TIME => {
        step.duration match {
          case Some(duration) => device.waitTime(duration)
          case _ => println("No duration specified,  can't wait for: "+step)
        }
      }
      case _ => {println("Bad Step Type")}  //TODO report/log
    }

  })
}


//object StepIndexer {
//  var lastId: Int = 0
//
//  def getId: Int = {
//    lastId += 1
//    lastId
//  }
//}

