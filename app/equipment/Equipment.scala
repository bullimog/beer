package equipment


abstract class Equipment(id: Int, name: String, equipmentType: String) {

  override def toString = "type:"+equipmentType +"; name:" + name
}

object EquipmentIndexer {
  var lastId: Int = 0

  val getId: Int = {
    lastId += 1
    lastId
  }
}



trait Controllable {
  def setPower(power: Int): Unit = {
    println("Controllable item has been set to: " + power + "%")
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

  def On: Unit = {
    println("Switchable has been turned on")
    //on = true
  }


  def Off: Unit = {
    println("Switchable has been turned off")
    //on = false
  }
}


trait Readable{

  def read: Double = {
    println("Rading from Readable")
    37.1
  }


}