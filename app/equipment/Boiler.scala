package equipment

// Heater with a pump and thermometer
class Boiler(name: String, family: String,
             heatingElement: HeatingElement, thermometer: Option[Thermometer],
             pump: Option[Pump]) extends Equipment(name, family){
  val id = getId
//  parts.foreach(i => println(i))

  //Just a Heater
  def this(name: String, family: String,
           heatingElement: HeatingElement) = this(name, family, heatingElement, None, None)

  // Heater with just a Thermometer
  def this(name: String, family: String,
           heatingElement: HeatingElement, thermometer: Thermometer) =
      this(name, family, heatingElement, Some(thermometer), None)

  //Heater with just a Pump
  def this(name: String, family: String,
           heatingElement: HeatingElement, pump: Pump) =
    this(name, family, heatingElement, None, Some(pump))


  def circulateOn = {

    pump match{
      case Some(p) => { p.on }
      case _ =>
    }
  }

  def circulateOff = {
    pump match{
      case Some(p) => { p.off }
      case _ =>
    }
  }

  def heatOn = {
    heatingElement.setPower(100)
  }
  def heatOff = {
    heatingElement.setPower(0)
  }

  def warm(power: Int) = {
    heatingElement.setPower(power)
  }

  def readTemperature: Option[Double] = {
    thermometer match{
      case Some(t) => { Some(t.readTemperature()) }
      case _ => None
    }
  }

  //Akka!?!
  def maintainTemperature(targetTemperature: Double, seconds: Int): Boolean ={
    thermometer match{
      case None => false  //No thermometer = no monitoring
      case _ => true
    }

  }
}

// Companion factory object
object Boiler {
//  def apply(name: String, parts: List[Equipment]): Boiler ={
//    new Boiler(name, "Boiler", parts)
//  }

  def apply(name: String, heatingElement: HeatingElement ): Boiler = {
    new Boiler(name, "Boiler", heatingElement)
  }

  def apply(name: String, heatingElement: HeatingElement, thermometer: Thermometer ): Boiler = {
    new Boiler(name, "Boiler", heatingElement, Some(thermometer), None)
  }

  def apply(name: String, heatingElement: HeatingElement, pump: Pump ): Boiler = {
    new Boiler(name, "Boiler", heatingElement, None, Some(pump) )
  }

  def apply(name: String, heatingElement: HeatingElement, thermometer: Thermometer,  pump: Pump ): Boiler = {
    new Boiler(name, "Boiler", heatingElement, Some(thermometer), Some(pump) )
  }

}