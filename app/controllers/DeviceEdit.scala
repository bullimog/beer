package controllers

import connector.{K8055Board, DeviceConnector}
//import controllers.Application._
import forms.DeviceForm.deviceForm
import model.Device
import play.api.mvc._

trait DeviceEdit extends Controller{

  def present(deviceId:Int) = Action {
    val componentManager = new ComponentManager with BrewComponentManager {
      override val deviceConnector: DeviceConnector = new DeviceConnector with K8055Board
    }
    val componentCollection = controllers.ConfigIO.readComponentCollection("deviceSetup.json")
    println("deviceId = "+deviceId)
    val device:Device = componentManager.deviceFromId(componentCollection, deviceId)
    Ok(views.html.device_edit(deviceForm.fill(device)))
  }

  def submit() = Action { implicit request =>
    deviceForm.bindFromRequest.fold(
      errors => Ok(views.html.device_edit(errors)),
      value => handleSomething(value))

  }
  private def handleSomething(device: Device ) : Result = {
    Ok(views.html.device_edit(deviceForm.fill(device)))
  }
}

object DeviceEdit extends DeviceEdit
