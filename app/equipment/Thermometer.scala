package equipment


class Thermometer(name: String, family: String) extends Equipment(name, family) with Readable{

  val id = getId


  def readTemperature(): Double = {
    println("Temperature has bean read")
    37.1
  }
}

object Thermometer{
  def apply(name: String): Thermometer ={
    new Thermometer(name, "Thermometer")
  }
}



