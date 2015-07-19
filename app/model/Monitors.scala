package model

import play.api.libs.json.{Json, JsPath, Reads}
import play.api.libs.functional.syntax._


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

case class ReadableMonitor(override val id: Int, override val description: String,
                           override val deviceType: Int, sensor:Device, increaser:Device) extends Component


case class MonitorStatus(componentId:Int, enabled:Boolean, temperature:Double,
                         sensorStatus:ComponentStatus, increaserStatus:ComponentStatus)
object  MonitorStatus {
  implicit val formats=Json.writes[MonitorStatus]
}