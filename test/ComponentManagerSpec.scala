import controllers.{ComponentManagerK8055, ComponentManager}
import model.{ComponentCollection, Thermostat, Device, Component}
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

@RunWith(classOf[JUnitRunner])
class ComponentManagerSpec extends Specification {

  val componentManager = new ComponentManager with ComponentManagerK8055 {

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

  val thermometer = Device(1,"Thermometer", Component.ANALOGUE_IN, 1)
  val pump = Device(2, "Pump", Component.DIGITAL_OUT, 1)
  val heater = Device(3, "Heater", Component.ANALOGUE_OUT, 1)
  val lb = new ListBuffer[Device]()
  lb += thermometer += pump += heater
  val devices = lb.toList
  val thermo = Thermostat(4, "Boiler", Component.MONITOR, 1, 3)
  val thermos = List(thermo)
  val componentCollection = ComponentCollection ("Masher 1", "My first set-up, for mashing", devices, thermos)


  "ComponentManager" should {
    "turn on and off, when instructed to do so" in {
      componentManager.on(pump)
      componentManager.isOn(pump) must equalTo(true)
      componentManager.off(pump)
      componentManager.isOn(pump) must equalTo(false)
    }
  }

  "ComponentManager" should {
    "identify a device within a ComponentCollection, for a given id" in {
      val foundDevice = componentManager.deviceFromId(componentCollection, 3)
      foundDevice.description must equalTo("Heater")
    }
  }

  "ComponentManager" should {
    "block the waitTemperatureHeating thread appropriately, until the desired heat has been reached" in {
      componentManager.setTemperature(thermometer, 22)  //set the temperature of the thermometer
      componentManager.waitTemperatureHeating(thermometer, 22)
      componentManager.readTemperature(thermometer) must equalTo(Some(22))

      componentManager.setTemperature(thermometer, 22)  //set the temperature of the thermometer
      componentManager.waitTemperatureHeating(thermometer, 21)
      componentManager.readTemperature(thermometer) must equalTo(Some(22))


      componentManager.setTemperature(thermometer, 20)  //set the temperature of the thermometer
      var finished:Boolean = false
      Future {
        componentManager.waitTemperatureHeating(thermometer, 22)
        finished  = true
      }

      Thread.sleep(3000)
      componentManager.setTemperature(thermometer, 22)  //set the temperature of the thermometer, to finish the Wait
      Thread.sleep(1000)
      componentManager.readTemperature(thermometer) must equalTo(Some(22))
      finished mustEqual true
    }
  }

  "ComponentManager" should {
    "block the waitTime thread appropriately, until the desired times has elapsed" in {

      var finished:Boolean = false
      Future {
        componentManager.waitTime(thermometer, 3)
        finished  = true
      }
      Thread.sleep(2000)
      finished mustEqual false
      Thread.sleep(2000)
      finished mustEqual true
    }
  }


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
      componentManager.getPower(heater) must equalTo(Some(0))
    }
  }
}
