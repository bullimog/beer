import controllers.ComponentManager
import model.{ComponentCollection, Thermostat, Device, Component}
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

import scala.collection.mutable.ListBuffer

@RunWith(classOf[JUnitRunner])
class ComponentManagerSpec extends Specification {

  val componentManager = new ComponentManager {override def setPower(component: Component, power: Int): Unit = ???

    override def readTemperature(component: Component): Option[Double] = ???
    override def setThermostatHeat(componentCollection: ComponentCollection, thermostat: Thermostat, temperature: Double): Unit = ???
    override def on(component: Component): Unit = ???
    override def waitTemperatureHeating(component: Component, targetTemperature: Double): Unit = ???
    override def pause(component: Component): Unit = ???
    override def waitTime(component: Component, duration: Int): Unit = ???
    override def off(component: Component): Unit = ???
    override def resume(component: Component): Unit = ???
    override def isOn(component: Component): Boolean = ???
  }

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
    "turn on, when instructed to do so" in {
      ComponentManager.on(device2)
      ComponentManager.isOn(device2) must equalTo(true)
    }

    "ComponentManager" should {
      "turn off, when instructed to do so" in {
        ComponentManager.off(device2)
        ComponentManager.isOn(device2) must equalTo(false)
      }
    }

    "ComponentManager" should {
      "identify a device within a ComponentCollection, for a given id" in {
        val foundDevice = ComponentManager.deviceFromId(componentCollection, 3)
        foundDevice.description must equalTo("Heater")
      }
    }


    "ComponentManager" should {
      "block the thread until the desired heat is reached" in {
        ComponentManager.waitTemperatureHeating(device2, 22)

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
