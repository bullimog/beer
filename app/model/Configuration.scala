package model

import play.api.libs.json.Json

case class Configuration (currentSequence:String, currentConfig:String)

object Configuration {
  implicit val formats=Json.format[Configuration]

}

