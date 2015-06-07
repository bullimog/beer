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
//    override def waitTemperatureHeating(component: Component, targetTemperature: Double): Unit = {
//      print(component.description + " waiting for temperature: " + targetTemperature + "... ")
//    }
//    override def readTemperature(component: Component): Option[Double] = {
//      Some(22)
//    }
//    var isOnNow:Boolean = false
//    override def isOn(component: Component): Boolean = {
//      return isOnNow
//    }

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
//  val componentManager = new ComponentManager {override def setPower(component: Component, power: Int): Unit = ???
//
//    override def readTemperature(component: Component): Option[Double] = ???
//    override def setThermostatHeat(componentCollection: ComponentCollection, thermostat: Thermostat, temperature: Double): Unit = ???
//    override def on(component: Component): Unit = ???
//    override def waitTemperatureHeating(component: Component, targetTemperature: Double): Unit = ???
//    override def pause(component: Component): Unit = ???
//    override def waitTime(component: Component, duration: Int): Unit = ???
//    override def off(component: Component): Unit = ???
//    override def resume(component: Component): Unit = ???
//    override def isOn(component: Component): Boolean = ???
//  }

    val device1 = Device(1,"Thermometer", Component.ANALOGUE_IN, 1)
    val device2 = Device(2, "Pump", Component.DIGITAL_OUT, 1)
    val device3 = Device(3, "Heater", Component.ANALOGUE_OUT, 1)
    val lb = new ListBuffer[Device]()
    lb += device1 += device2 += device3
    val devices = lb.toList
    val thermo = Thermostat(4, "Boiler", Component.MONITOR, 1, 3)
    val thermos = List(thermo)
    val componentCollection = ComponentCollection ("Masher 1", "My first set-up, for mashing", devices, thermos)


    "ComponentManager" should {
      "turn on and off, when instructed to do so" in {
        componentManager.on(device2)
        componentManager.isOn(device2) must equalTo(true)
        componentManager.off(device2)
        componentManager.isOn(device2) must equalTo(false)
      }

    "ComponentManager" should {
      "identify a device within a ComponentCollection, for a given id" in {
        val foundDevice = componentManager.deviceFromId(componentCollection, 3)
        foundDevice.description must equalTo("Heater")
      }
    }

    "ComponentManager" should {
      "block the thread appropriately, until the desired heat has been reached" in {
        componentManager.setTemperature(device1, 22)  //set the temperature of the thermometer
        componentManager.waitTemperatureHeating(device1, 22)
        componentManager.readTemperature(device1) must equalTo(Some(22))

        componentManager.setTemperature(device1, 22)  //set the temperature of the thermometer
        componentManager.waitTemperatureHeating(device1, 21)
        componentManager.readTemperature(device1) must equalTo(Some(22))


        componentManager.setTemperature(device1, 20)  //set the temperature of the thermometer
        var finished:Boolean = false
        Future {
          componentManager.waitTemperatureHeating(device1, 22)
          finished  = true
        }

        Thread.sleep(3000)
        componentManager.setTemperature(device1, 22)  //set the temperature of the thermometer, to finish the Wait
        Thread.sleep(1000)
        componentManager.readTemperature(device1) must equalTo(Some(22))
        finished mustEqual(true)
      }
    }





//    "render the index page" in new WithApplication{
//      val home = route(FakeRequest(GET, "/")).get
//
//      status(home) must equalTo(OK)
//      contentType(home) must beSome.which(_ == "text/html")
//      contentAsString(home) must contain ("Your new application is ready.")
//    }
  }
}
