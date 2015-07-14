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
  val ANALOGUE_OUT = 2  // Heater or Cooler
  val DIGITAL_IN = 3    // Button or Switch
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
                      override val deviceType: Int, sensor:Int, increaser:Int) extends Component
object Monitor{
//  implicit val monitorFmt = Json.format[Monitor]
implicit val monitorReads: Reads[Monitor] = (
  (JsPath \ "id").read[Int] and
    (JsPath \ "description").read[String] and
    (JsPath \ "deviceType").read[Int] and
    (JsPath \ "sensor").read[Int] and
    (JsPath \ "increaser").read[Int]
  )(Monitor.apply _)

  implicit val monitorWrites = Json.writes[Monitor]
}


case class ComponentCollection(name: String, description: String, devices: List[Device],
                               monitors: List[Monitor])
object ComponentCollection{
  //  implicit val componentCollectionFmt = Json.format[ComponentCollection]

  implicit val componentCollectionReads: Reads[ComponentCollection] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "description").read[String] and
      (JsPath \ "devices").read[List[Device]] and
      (JsPath \ "monitors").read[List[Monitor]]
    )(ComponentCollection.apply _)

    /* Since this case class references Device and Monitor, the Json.writes has to be defined last! */
    implicit val componentCollectionWrites = Json.writes[ComponentCollection]
}

case class ComponentStatus(componentId:Int, componentType:Int, componentValue:String, componentUnit:Option[String])
object  ComponentStatus {
  implicit val formats=Json.writes[ComponentStatus]
}

case class MonitorStatus(componentId:Int, enabled:Boolean, temperature:Double,
                            sensorStatus:ComponentStatus, increaserStatus:ComponentStatus)
object  MonitorStatus {
  implicit val formats=Json.writes[MonitorStatus]
}