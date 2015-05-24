package equipment

import EquipmentIndexer.getId

class Thermometer(id: Int, name: String, equipmentType: String) extends Equipment(id, name, equipmentType) with Readable{


  def readTemperature(): Double = {
    println("Temperature has bean read")
    37.1
  }
}

object Thermometer{
  def apply(name: String): Thermometer ={
    new Thermometer(getId, name, "Thermometer")
  }
}



