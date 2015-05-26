package equipment

//import EquipmentIndexer.getId


class Pump(id: Int, name: String, equipmentType: String)
  extends Equipment(id, name, equipmentType) // {}with Switchable{}

object Pump{
  def apply(id: Int, name: String): Pump ={new Pump(id, name, "Pump")}
}