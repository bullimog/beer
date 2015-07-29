package forms

import model.DeviceConfiguration
import play.api.data.Form
import play.api.data.Forms._

object DeviceConfigurationForm {
  val deviceConfigurationForm:Form[DeviceConfiguration] = Form( mapping(
      "currentSequence" -> text)
      (DeviceConfiguration.apply) (DeviceConfiguration.unapply)
  )
}