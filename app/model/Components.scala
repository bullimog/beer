package model

import akka.actor.Cancellable
import play.api.libs.json.{JsPath, Reads, Json}
import play.api.libs.functional.syntax._


trait Component{
  def id: Int
  def description: String
  def deviceType: Int
//  var cancellable: Option[Cancellable]

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
                  override val deviceType: Int, port:Int) extends Component

object Device{
//  implicit val deviceFmt = Json.format[Device]
implicit val deviceReads: Reads[Device] = (
  (JsPath \ "id").read[Int] and
    (JsPath \ "description").read[String] and
    (JsPath \ "deviceType").read[Int] and
    (JsPath \ "port").read[Int]
  )(Device.apply _)

  implicit val deviceWrites = Json.writes[Device]
}

case class Thermostat(override val id: Int, override val description: String,
                      override val deviceType: Int, thermometer:Int, heater:Int) extends Component
object Thermostat{
//  implicit val thermostatFmt = Json.format[Thermostat]
implicit val thermostatReads: Reads[Thermostat] = (
  (JsPath \ "id").read[Int] and
    (JsPath \ "description").read[String] and
    (JsPath \ "deviceType").read[Int] and
    (JsPath \ "thermometer").read[Int] and
    (JsPath \ "heater").read[Int]
  )(Thermostat.apply _)

  implicit val thermostatWrites = Json.writes[Thermostat]
}


case class ComponentCollection(name: String, description: String, devices: List[Device],
                               thermostats: List[Thermostat])
object ComponentCollection{
  //  implicit val componentCollectionFmt = Json.format[ComponentCollection]

  implicit val componentCollectionReads: Reads[ComponentCollection] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "description").read[String] and
      (JsPath \ "devices").read[List[Device]] and
      (JsPath \ "thermostats").read[List[Thermostat]]
    )(ComponentCollection.apply _)

    /* Since this case class references Device and Thermostat, the Json.writes has to be defined last! */
    implicit val componentCollectionWrites = Json.writes[ComponentCollection]
}

case class ComponentStatus(componentId:Int, componentType:Int, componentValue:String)
object  ComponentStatus {
  implicit val formats=Json.writes[ComponentStatus]
}