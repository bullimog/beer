package forms

import model.Configuration
import play.api.data.Form
import play.api.data.Forms._

object ConfigurationForm {
  val configurationForm:Form[Configuration] = Form( mapping(
      "currentSequence" -> text,
      "currentConfig" -> text)
      (Configuration.apply) (Configuration.unapply)
  )
}