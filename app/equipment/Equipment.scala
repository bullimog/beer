package equipment

import scala.collection.mutable.ListBuffer


class Equipment(val id: Int, val name: String, val equipmentType: String) {
  override def toString = "id:"+ id +" type:"+equipmentType +"; name:" + name

  //Default, do nothing implementations.
  def on() {} //TODO: method not supported warning
  def off() {}
  def setThermostat() {}
  def waitTemperature() {}
  def waitTime(): Unit = {
    //TODO: Implement this here, so any item of Equipment can be used to wait
    //looks like there is a wait function on Object
  }

}

object Equipment {
  // Static definition of equipment. Will be dynamic eventually.
  val thermometer1 = Thermometer(1,"myThermometer")
  val pump1 = Pump(2, "myPump")
  val heatingElement1 = HeatingElement(3, "myHeater")
  val boiler1 = Boiler(4, "myBoiler", heatingElement1, thermometer1, pump1)

  val p = new ListBuffer[Equipment]()
  p += thermometer1
  p += pump1
  p += heatingElement1
  p += boiler1
  val parts = p.toList
}

//object EquipmentIndexer {
//  var lastId: Int = 0
//
//  def getId: Int = {
//    lastId += 1
//    lastId
//  }
//}



trait Controllable {
  val MAX = 100
  val MIN = 0

  def setPower(power: Int): Unit = {
    val p = math.min(MAX, math.max(MIN, power))
    println("Controllable item has been set to: " + p + "%")
  }

  def getPower: Int = {
    println("Controllable item has been read")
    0
  }

  def off: Unit = {
    println("Controllable item has been turned off")
  }
}

trait Switchable {
  def on: Unit = {
    println("Switchable has been turned on")
    //on = true
  }

  def off: Unit = {
    println("Switchable has been turned off")
    //on = false
  }

  def isOn: Boolean = {
    println("Switchable has been turned off")
    false
  }
}


trait Readable{
  def read(): Double = {
    println("Reading from Readable")
    37.1
  }
}