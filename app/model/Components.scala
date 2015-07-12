package model

import play.api.libs.json.{JsPath, Reads, Json}
import play.api.libs.functional.syntax._


trait Component{
  def id: Int
  def description: String
  def deviceType: Int

}

object Component {
  val TIMER = 0         // Clock
  val ANALOGUE_IN = 1   // Thermometer
  val ANALOGUE_OUT = 2  // Heater
  val DIGITAL_IN = 3    // Button
  val DIGITAL_OUT = 4   // Pump
  val MONITOR = 5       // Thermostat
}

case class Device(override val id: Int, override val description: String,
                  override val deviceType: Int, port:Int, units:Option[String], conversionFactor:Option[Double],
                   conversionOffset:Option[Double], decimalPlaces:Option[Int]) extends Component

object Device{
//  implicit val deviceFmt = Json.format[Device]
implicit val deviceReads: Reads[Device] = (
  (JsPath \ "id").read[Int] and
    (JsPath \ "description").read[String] and
    (JsPath \ "deviceType").read[Int] and
    (JsPath \ "port").read[Int] and
    (JsPath \ "units").readNullable[String] and
    (JsPath \ "conversionFactor").readNullable[Double] and
    (JsPath \ "conversionOffset").readNullable[Double] and
    (JsPath \ "decimalPlaces").readNullable[Int]
  )(Device.apply _)

  implicit val deviceWrites = Json.writes[Device]
}

case class Monitor(override val id: Int, override val description: String,
                      override val deviceType: Int, thermometer:Int, heater:Int) extends Component
object Monitor{
//  implicit val thermostatFmt = Json.format[Thermostat]
implicit val thermostatReads: Reads[Monitor] = (
  (JsPath \ "id").read[Int] and
    (JsPath \ "description").read[String] and
    (JsPath \ "deviceType").read[Int] and
    (JsPath \ "thermometer").read[Int] and
    (JsPath \ "heater").read[Int]
  )(Monitor.apply _)

  implicit val thermostatWrites = Json.writes[Monitor]
}


case class ComponentCollection(name: String, description: String, devices: List[Device],
                               thermostats: List[Monitor])
object ComponentCollection{
  //  implicit val componentCollectionFmt = Json.format[ComponentCollection]

  implicit val componentCollectionReads: Reads[ComponentCollection] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "description").read[String] and
      (JsPath \ "devices").read[List[Device]] and
      (JsPath \ "thermostats").read[List[Monitor]]
    )(ComponentCollection.apply _)

    /* Since this case class references Device and Thermostat, the Json.writes has to be defined last! */
    implicit val componentCollectionWrites = Json.writes[ComponentCollection]
}

case class ComponentStatus(componentId:Int, componentType:Int, componentValue:String, componentUnit:Option[String])
object  ComponentStatus {
  implicit val formats=Json.writes[ComponentStatus]
}

case class ThermostatStatus(componentId:Int, enabled:Boolean, temperature:Double,
                            thermometerStatus:ComponentStatus, heaterStatus:ComponentStatus)
object  ThermostatStatus {
  implicit val formats=Json.writes[ThermostatStatus]
}