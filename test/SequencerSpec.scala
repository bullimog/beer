import connector.{DeviceConnector, DeviceConnectorStub}
import controllers.{BrewComponentManager, ComponentManager}
import model._
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._
import sequencer.Sequencer

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

@RunWith(classOf[JUnitRunner])
class SequencerSpec extends Specification {

  val componentManager = new ComponentManager with BrewComponentManager {
    override val deviceConnector:DeviceConnector = new DeviceConnector with DeviceConnectorStub

    //Need to stub setting temperature on thermometer
    def setTemperature(component:Component, value:Double): Unit = {
      println(component.description+ " setting temperature on stub")
      component.deviceType match{
        case Component.ANALOGUE_IN =>
          component match{
            case d:Device => deviceConnector.setAnalogueIn(d.port, value)
            case _ => println(component.description+ " Can't fake set temperature on non-device")
          }
        case _ => println(component.description+ " Can only fake set temperature on a Analogue In")
      }
    }
  }


  //val sequencer = new Sequencer
  val thermometer = Device(1,"Thermometer", Component.ANALOGUE_IN, 1, None, None, None, None)
  val pump = Device(2, "Pump", Component.DIGITAL_OUT, 1, None, None, None, None)
  val heater = Device(3, "Heater", Component.ANALOGUE_OUT, 1, None, None, None, None)
  val timer = Device(4, "Clock", Component.TIMER, 0, None, None, None, None)
  val lb = new ListBuffer[Device]()
  lb += thermometer += pump += heater += timer
  val devices = lb.toList
  val thermo = Monitor(104, "Boiler", Component.MONITOR, 1, 3)
  val thermos = List(thermo)
  val componentCollection = ComponentCollection ("Masher 1", "My first set-up, for mashing", devices, thermos)

  val stepDefn1 = Step(1, 104, Step.SET_TARGET, Some(41), None)  // Set required monitor target to 41
  val stepDefn2 = Step(2, 2, Step.ON, None, None)                // Turn pump on
  val stepDefn3 = Step(3, 1, Step.WAIT_RISING, Some(41), None)     // Wait for thermometer to reach 41
  val stepDefn4 = Step(4, 4, Step.WAIT_TIME, None, Some(5))      // wait for 5 seconds
  val stepDefn5 = Step(5, 104, Step.SET_TARGET, Some(68), None)  // Set monitor target to 68
  val stepDefn6 = Step(6, 4, Step.WAIT_TIME, None, Some(5))      // wait for 5 seconds
  val stepDefn7 = Step(7, 104, Step.OFF, None, None)             // turn thermostat off
  val stepDefn8 = Step(8, 3, Step.OFF, None, None)               // turn heater off
  val stepDefn9 = Step(9, 2, Step.OFF, None, None)               // turn pump off

  val lb2 = new ListBuffer[Step]()
  lb2 += stepDefn1 += stepDefn2 += stepDefn3 += stepDefn4 += stepDefn5 += stepDefn6 += stepDefn7 += stepDefn8
  val sequence = new Sequence("Test Sequence", lb2.toList)



  "Sequencer" should {
    "run defined sequence" in {
      componentManager.setTemperature(thermometer, 0)  //set the temperature of the thermometer
      Sequencer.runSequence(componentManager, componentCollection, sequence) //Future
      Thread.sleep(3000)
      componentManager.getPower(heater) must equalTo(Some(100))  //Step1
      componentManager.isOn(pump) must equalTo(true)             //Step2
      Thread.sleep(1000)
      componentManager.setTemperature(thermometer, 41)  //set the temperature of the thermometer, to finish step 3
      Thread.sleep(6000) // Wait for WAIT_TIME Step 4
      componentManager.getPower(heater) must equalTo(Some(0))
      Thread.sleep(9000) // Wait for WAIT_TIME
      componentManager.getPower(heater) must equalTo(Some(100))
      componentManager.setTemperature(thermometer, 68)  //set the temperature of the thermometer, to finish the Wait
      Thread.sleep(9000)
      componentManager.stopMonitors()
      componentManager.getPower(heater) must equalTo(Some(0))
      componentManager.isOn(pump) must equalTo(false)
    }
  }

}
