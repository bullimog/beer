package equipment

import EquipmentIndexer.getId


class Pump(id: Int, name: String, equipmentType: String) extends Equipment(id, name, equipmentType) with Switchable{

  def on = {}
  def off = {}
  def isOn:Boolean = {false}
}


object Pump{
  def apply(name: String): Pump ={
    new Pump(getId, name, "Pump")
  }
}