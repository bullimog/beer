package equipment

//import EquipmentIndexer.getId

class Thermometer(id: Int, name: String, equipmentType: String)
  extends Equipment(id, name, equipmentType) with Readable{}

object Thermometer{
  def apply(id: Int, name: String): Thermometer ={new Thermometer(id, name, "Thermometer")}
}



