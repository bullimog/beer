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

case class ReadableComponentCollection(name: String, description: String, devices: List[Device],
                               monitors: List[ReadableMonitor])

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

/* case classes for ajax data */
case class ComponentStatus(componentId:Int, componentType:Int, componentValue:String, componentUnit:Option[String])
object  ComponentStatus {
  implicit val formats=Json.writes[ComponentStatus]
}

