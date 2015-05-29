package sequencer

import model.{Thermostat, Device, Step}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Sequencer{

  // Static device definition for now....
  val component1 = Device(1,"Thermometer", Device.ANALOGUE_IN, Some(1))
  val component2 = Device(2, "Pump", Device.DIGITAL_OUT, Some(1))
  val component3 = Device(3, "Heater", Device.ANALOGUE_OUT, Some(1))

  val lb = new ListBuffer[Device]()
  lb += component1 += component2 += component3
  val components = lb.toList

  def addThermostat(devices: List[Device], thermo: Option[Device]): List[Device] ={
    thermo match {
      case Some(thermostat) => thermostat :: devices
      case _ => devices
    }
  }
  val thermo = Thermostat(4, "Boiler", Device.MONITOR, None, component1, component3)
  val allDevices = addThermostat(components, thermo)

  println("devices created: " + allDevices)


  // Static step definition for now....
  //  val step  = Step(device, stepType, temp, duration)
  val stepDefn1 = Step(4, Step.SET_TEMP, Some(41), None)      // Set required thermostat temp to 41
  val stepDefn2 = Step(2, Step.ON, None, None)                // Turn pump on
  val stepDefn3 = Step(1, Step.WAIT_TEMP, Some(41), None)     // Wait for thermometer to reach 41
  val stepDefn4 = Step(4, Step.WAIT_TIME, None, Some(60*20))  // wait for 20 minutes
  val stepDefn5 = Step(4, Step.SET_TEMP, Some(68), None)      // Set thermostat temp to 68
  val stepDefn6 = Step(4, Step.WAIT_TIME, None, Some(60*20))  // wait for 20 minutes
  val stepDefn7 = Step(4, Step.OFF, None, None)               // turn boiler off
  val stepDefn8 = Step(2, Step.OFF, None, None)               // turn pump off

  val p = new ListBuffer[Step]()
  p += stepDefn1 += stepDefn2 += stepDefn3 += stepDefn4 += stepDefn5 += stepDefn6 += stepDefn7 += stepDefn8
  val mySequence = p.toList
  println("sequences created: " + p)



  //function to find the item of Equipment, for the given step
  val getEquipment = (step:Step, deviceList:List[Device]) => {
    deviceList.filter((device:Device) => device.id == step.device).head
  }


  def runSequence:Unit = {
    Future {
      mySequence.foreach(step => {
        val device: Device = getEquipment(step, allDevices)
        println("step " + step + "to be serviced by " + device)

        step.eventType match {
          case Step.ON => device.on() //Digital Out
          case Step.OFF => device.off() //Digital/Analogue Out
          case Step.SET_TEMP => runSetTemp(step, device) //Thermostat
          case Step.WAIT_TEMP => runWaitTemp(step, device) //Thermometer
          case Step.WAIT_TIME => runWaitTime(step, device) //Any
          case _ => {println("Bad Step Type")} //TODO report/log
        }
      })
    }
  }

  def runSetTemp(step:Step, device:Device): Unit ={
    step.temperature match {
      case Some(temperature) => device.setThermostat (temperature)
      case _ => println("No temperature specified,  can't set temperature for: "+step)
    }
  }

  def runWaitTemp(step:Step, device:Device): Unit ={
    step.temperature match {
      case Some(temperature) => device.waitTemperatureHeating(temperature)
      case _ => println("No temperature specified,  can't wait for temperature for: "+step)
    }
  }

  def runWaitTime(step:Step, device:Device): Unit ={
    step.duration match {
      case Some(duration) => device.waitTime(duration)
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

