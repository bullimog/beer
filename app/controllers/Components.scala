package controllers

import controllers.Application._
import model.Component
import play.api.mvc.Action


trait Components {
  val componentCollection = controllers.ConfigIO.readComponentCollection("deviceSetup.json")

  val type2Description = (componentType: Int) => componentType match {
    case Component.TIMER => "Timer"
    case Component.ANALOGUE_IN => "Analogue In"
    case Component.ANALOGUE_OUT => "Analogue Out"
    case Component.DIGITAL_IN => "Digital In"
    case Component.DIGITAL_OUT => "Digital Out"
    case Component.MONITOR => "Monitor"
    case _ => "Unknown Type"
  }

  def present = Action {
    Ok(views.html.components(cCToReadableCc(componentCollection), type2Description))
  }
}

object Components extends Components{}
