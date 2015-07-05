import connector.{DeviceConnector, DeviceConnectorStub}
import controllers.{BrewComponentManager, ComponentManager}
import model.{ComponentCollection, Thermostat, Device, Component}
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._
import sequencer.Sequencer

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future


@RunWith(classOf[JUnitRunner])
class ComponentManagerSpec extends Specification {

  val componentManager = new ComponentManager with BrewComponentManager {
    //Need to stub setting temperature on thermometer
    override val deviceConnector:DeviceConnector = new DeviceConnector with DeviceConnectorStub //stub for now...
    def setTemperature(component:Component, value:Double): Unit = {
      println(component.description+ " setting temperature on stub to "+value)
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


  val timer = Device(1,"Clock", Component.TIMER, 0)
  val thermometer = Device(2,"Thermometer", Component.ANALOGUE_IN, 1)
  val pump = Device(3, "Pump", Component.DIGITAL_OUT, 1)
  val heater = Device(4, "Heater", Component.ANALOGUE_OUT, 1)
  val lb = new ListBuffer[Device]()
  lb += timer += thermometer += pump += heater
  val devices = lb.toList
  val thermo = Thermostat(5, "Boiler", Component.MONITOR, 2, 4)
  val thermos = List(thermo)
  val componentCollection = ComponentCollection ("Masher 1", "My first set-up, for mashing", devices, thermos)


  "ComponentManager" should {
    "turn on and off, when instructed to do so" in {
      componentManager.on(componentCollection, pump)
      componentManager.isOn(pump) must equalTo(true)
      componentManager.off(componentCollection, pump)
      componentManager.isOn(pump) must equalTo(false)
    }
  }

  "ComponentManager" should {
    "identify a device within a ComponentCollection, for a given id" in {
      val foundDevice = componentManager.componentFromId(componentCollection, 4)
      foundDevice.description must equalTo("Heater")
    }
  }

  "ComponentManager" should {
    "block the waitTemperatureHeating thread appropriately, until the desired heat has been reached" in {
      componentManager.setTemperature(thermometer, 22)  //set the temperature of the thermometer
      componentManager.reachedTemperatureHeating(thermometer, 22)
      componentManager.readTemperature(thermometer) must equalTo(Some(22))

      componentManager.setTemperature(thermometer, 22)  //set the temperature of the thermometer
      componentManager.reachedTemperatureHeating(thermometer, 21)
      componentManager.readTemperature(thermometer) must equalTo(Some(22))


      componentManager.setTemperature(thermometer, 20)  //set the temperature of the thermometer
      var finished:Boolean = false
      Future {
        componentManager.reachedTemperatureHeating(thermometer, 22)
        finished  = true
      }

      Thread.sleep(3000)
      componentManager.setTemperature(thermometer, 22)  //set the temperature of the thermometer, to finish the Wait
      Thread.sleep(1000)
      componentManager.readTemperature(thermometer) must equalTo(Some(22))
      finished mustEqual true
    }
  }

//  "ComponentManager" should {
//    "block the waitTime thread appropriately, until the desired times has elapsed" in {
//
//      var finished:Boolean = false
//      Sequencer.running = true
//      Future {
//        componentManager.waitTime(timer, 4)
//        finished  = true
//      }
//      Thread.sleep(1000)
//      finished mustEqual false
//      Thread.sleep(6000)
//      Sequencer.running = true
//      finished mustEqual true
//    }
//  }


  "ComponentManager" should {
    "adjust power, when instructed to do so" in {
      componentManager.setPower(heater, 100)
      componentManager.getPower(heater) must equalTo(Some(100))
      componentManager.setPower(heater, 50)
      componentManager.getPower(heater) must equalTo(Some(50))
      componentManager.setPower(heater, 0)
      componentManager.getPower(heater) must equalTo(Some(0))
    }
  }

  "ComponentManager" should {
    "maintain thermostat, when instructed to do so" in {
      componentManager.setThermostatHeat(componentCollection, thermo, 42)
      Thread.sleep(2000)
      componentManager.getPower(heater) must equalTo(Some(100))
      componentManager.setTemperature(thermometer, 22)
      Thread.sleep(2000)
      componentManager.getPower(heater) must equalTo(Some(100))
      componentManager.setTemperature(thermometer, 40)
      Thread.sleep(2000)
      componentManager.getPower(heater) must equalTo(Some(100))
      componentManager.setTemperature(thermometer, 41)
      Thread.sleep(2000)
      componentManager.getPower(heater) must equalTo(Some(50))
      componentManager.setTemperature(thermometer, 41.5)
      Thread.sleep(2000)
      componentManager.getPower(heater) must equalTo(Some(25))
      componentManager.setTemperature(thermometer, 42)
      Thread.sleep(2000)
      componentManager.getPower(heater) must equalTo(Some(0))
      componentManager.setTemperature(thermometer, 43)
      Thread.sleep(2000)
      componentManager.stopThermostats()
      componentManager.getPower(heater) must equalTo(Some(0))


    }
  }
}
