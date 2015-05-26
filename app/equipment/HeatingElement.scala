package equipment

//import EquipmentIndexer.getId

class HeatingElement(id: Int, name: String, equipmentType: String)
  extends Equipment(id, name, equipmentType) // {} with Controllable {}


object HeatingElement{
  def apply(id: Int, name: String): HeatingElement ={new HeatingElement(id, name, "Heating Element")}
}
