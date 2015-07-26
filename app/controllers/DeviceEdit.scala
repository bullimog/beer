package controllers

import connector.{ConfigIO, K8055Board, DeviceConnector}

import scala.concurrent.Future

//import controllers.Application._
import forms.DeviceForm.deviceForm
import model.{ComponentCollection, Device}
import play.api.mvc._

trait DeviceEdit extends Controller{

  def present() = Action.async {
    //Ok(views.html.device_edit(deviceForm.fill(fetchDevice(deviceId))))
    implicit request =>  fillForm()
  }

  private def toInt(s: Option[String]): Option[Int] = {
    s match{
      case Some(x) => try {Some(x.toInt)
      } catch {case e: Exception => None}
      case _ => None
    }
  }

  def fillForm()(implicit request: Request[_]):Future[Result] = {
    val devices = request.session.get("devices")
    println("devices="+devices)
    toInt(request.getQueryString("deviceId")) match {
      case Some(deviceId) => {
        fetchDevice(deviceId) match {
          case Some(device) =>Future.successful(Ok(views.html.device_edit(deviceForm.fill(device))))
          case _ => Future.successful(Redirect(routes.ComponentsController.present()))
        }
      }
      case _ => Future.successful(Redirect(routes.ComponentsController.present()))
    }
  }

  def fetchDevice(deviceId:Int): Option[Device] = {
    val componentManager = new ComponentManager with BrewComponentManager {
      override val deviceConnector: DeviceConnector = new DeviceConnector with K8055Board
    }
    val componentCollection:ComponentCollection = ConfigIO.readComponentCollection("deviceSetup.json").get //TODO remove hard coded file
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
