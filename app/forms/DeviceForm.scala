package forms

import model.Device
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.format.Formats._


object DeviceForm {
  val deviceForm = Form(
    mapping(
      "id" -> number,
      "description" -> nonEmptyText,
      "deviceType" -> number(min=0, max=4),
      "port" -> number(min=0, max=8),
      "units" -> optional(text),
      "conversionFactor" -> optional(of[Double]),
      "conversionOffset" -> optional(of[Double]),
      "decimalPlaces" -> optional(number)
    )(Device.apply)(Device.unapply)
  )
}
