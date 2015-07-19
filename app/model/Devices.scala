package model

import play.api.libs.json.{Json, JsPath, Reads}
import play.api.libs.functional.syntax._


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