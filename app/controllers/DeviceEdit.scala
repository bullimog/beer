package controllers

import connector.{K8055Board, DeviceConnector}
//import controllers.Application._
import forms.DeviceForm.deviceForm
import model.{ComponentCollection, Device}
import play.api.mvc._

trait DeviceEdit extends Controller{

  def present(deviceId:Int) = Action {
    Ok(views.html.device_edit(deviceForm.fill(fetchDevice(deviceId))))
  }

  def fetchDevice(deviceId:Int): Device = {
    val componentManager = new ComponentManager with BrewComponentManager {
      override val deviceConnector: DeviceConnector = new DeviceConnector with K8055Board
    }
    val componentCollection:ComponentCollection = controllers.ConfigIO.readComponentCollection("deviceSetup.json")
    componentManager.deviceFromId(componentCollection, deviceId)
  }

  def submit() = Action{implicit request =>
    validateForm(request)
  }

  def validateForm(implicit request: Request[AnyContent]): Result = deviceForm.bindFromRequest.fold(
    errors => Ok(views.html.device_edit(errors)),
    device => handleSomething(device)
  )


  private def handleSomething(device: Device ) : Result = {
    Ok(views.html.device_edit(deviceForm.fill(device)))
  }
}

object DeviceEdit extends DeviceEdit
