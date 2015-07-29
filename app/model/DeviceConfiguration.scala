package model

import play.api.libs.json.Json

case class DeviceConfiguration (currentSequence:String)

object DeviceConfiguration {
  implicit val formats=Json.format[DeviceConfiguration]

}

