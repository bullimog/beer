package model

import play.api.libs.json.Json

case class ConfigurationName (name:String)

object ConfigurationName {
  implicit val formats=Json.format[ConfigurationName]

}