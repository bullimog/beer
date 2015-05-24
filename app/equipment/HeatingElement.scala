package equipment

import EquipmentIndexer.getId

class HeatingElement(id: Int, name: String, equipmentType: String) extends Equipment(id, name, equipmentType) with Controllable {

  val MAX = 100
  val MIN = 0

  override def setPower(power: Int): Unit = {
    val p = math.min(MAX, math.max(MIN, power))
    println("Element has been set to: " + p + "%")
  }

  override def getPower: Int = {
    println("Element power has been read")
    //read power from K8055 board
    0
  }

}

object HeatingElement{
  def apply(name: String): HeatingElement ={
    new HeatingElement(getId, name, "Heating Element")
  }
}
