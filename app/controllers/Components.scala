package controllers

import controllers.Application._
import play.api.mvc.Action


object Components {
  val componentCollection = controllers.ConfigIO.readComponentCollection("deviceSetup.json")

  def present = Action {
    Ok(views.html.components(cCToReadableCc(componentCollection)))
  }

}
