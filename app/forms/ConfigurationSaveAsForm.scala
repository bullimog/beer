package forms

import model.ConfigurationName
import play.api.data.Form
import play.api.data.Forms._

object ConfigurationSaveAsForm {
  val configurationSaveAsForm:Form[ConfigurationName] = Form( mapping(
      "name" -> text)
      (ConfigurationName.apply) (ConfigurationName.unapply)
  )
}