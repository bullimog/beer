import connector.{K8055Stub, K8055}
import controllers.{ComponentManagerK8055, ComponentManager}
import model._
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._
import sequencer.Sequencer

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

@RunWith(classOf[JUnitRunner])
class SequencerSpec extends Specification {

  val componentManager = new ComponentManager with ComponentManagerK8055 {
    override val k8055:K8055 = new K8055 with K8055Stub

    //Need to stub setting temperature on thermometer
    def setTemperature(component:Component, value:Double): Unit = {
      println(component.description+ " setting temperature on stub")
      component.deviceType match{
        case Component.ANALOGUE_IN =>
          component match{
            case d:Device => k8055.setAnalogueIn(d.port, value)
            case _ => println(component.description+ " Can't fake set temperature on non-device")
          }
        case _ => println(component.description+ " Can only fake set temperature on a Analogue In")
      }
    }
  }


  //val sequencer = new Sequencer
  val thermometer = Device(1,"Thermometer", Component.ANALOGUE_IN, 1)
  val pump = Device(2, "Pump", Component.DIGITAL_OUT, 1)
  val heater = Device(3, "Heater", Component.ANALOGUE_OUT, 1)
  val timer = Device(4, "Timer", Component.TIMER, 0)
  val lb = new ListBuffer[Device]()
  lb += thermometer += pump += heater += timer
  val devices = lb.toList
  val thermo = Thermostat(104, "Boiler", Component.MONITOR, 1, 3)
  val thermos = List(thermo)
  val componentCollection = ComponentCollection ("Masher 1", "My first set-up, for mashing", devices, thermos)

  val stepDefn1 = Step(1, 104, Step.SET_HEAT, Some(41), None)    // Set required thermostat temp to 41
  val stepDefn2 = Step(2, 2, Step.ON, None, None)                // Turn pump on
  val stepDefn3 = Step(3, 1, Step.WAIT_HEAT, Some(41), None)     // Wait for thermometer to reach 41
  val stepDefn4 = Step(4, 4, Step.WAIT_TIME, None, Some(5))    // wait for 5 seconds
  val stepDefn5 = Step(5, 104, Step.SET_HEAT, Some(68), None)    // Set thermostat temp to 68
  val stepDefn6 = Step(6, 4, Step.WAIT_TIME, None, Some(5))    // wait for 5 seconds
  val stepDefn7 = Step(7, 104, Step.OFF, None, None)             // turn boiler off
  val stepDefn8 = Step(8, 2, Step.OFF, None, None)               // turn pump off

  val lb2 = new ListBuffer[Step]()
  lb2 += stepDefn1 += stepDefn2 += stepDefn3 += stepDefn4 += stepDefn5 += stepDefn6 += stepDefn7 += stepDefn8
  val sequence = new Sequence("Test Sequence", lb2.toList)



  "Sequencer" should {
    "run defined sequence" in {
      Sequencer.runSequence(componentManager, componentCollection, sequence) //Future
      Thread.sleep(3000)
      componentManager.getPower(heater) must equalTo(Some(100))
      componentManager.isOn(pump) must equalTo(true)
      Thread.sleep(1000)
      componentManager.setTemperature(thermometer, 41)  //set the temperature of the thermometer, to finish the Wait
      Thread.sleep(1000)
      componentManager.getPower(heater) must equalTo(Some(0))
      Thread.sleep(7000) // Wait for WAIT_TIME
      componentManager.getPower(heater) must equalTo(Some(100))
      componentManager.setTemperature(thermometer, 68)  //set the temperature of the thermometer, to finish the Wait
      Thread.sleep(7000)
      componentManager.stopThermostats()
      componentManager.getPower(heater) must equalTo(Some(0))
      componentManager.isOn(pump) must equalTo(false)
    }
  }

}
