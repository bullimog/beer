package controllers

import java.io.File

import connector.ConfigIO
import connector.ConfigIO.devicesFileExt
import controllers.StatusController.cCToReadableCc
import forms.DeviceConfigurationForm._
import model.{ComponentCollection, DeviceConfiguration, Component}
import play.api.mvc._

import scala.concurrent.Future


trait ComponentsController extends Controller{
//  val DevicesFileExt:String = "-devices.json"

//  val componentCollection = ConfigIO.readComponentCollection("deviceSetup.json").get  //TODO remove hardcoded file

  def componentCollection(deviceConfig:String):ComponentCollection = {
    ConfigIO.readComponentCollection(deviceConfig+devicesFileExt).getOrElse(StatusController.defaultComponentCollection)
  }

  val type2Description = (componentType: Int) => componentType match {
    case Component.TIMER => "Timer"
    case Component.ANALOGUE_IN => "Analogue In"
    case Component.ANALOGUE_OUT => "Analogue Out"
    case Component.DIGITAL_IN => "Digital In"
    case Component.DIGITAL_OUT => "Digital Out"
    case Component.MONITOR => "Monitor"
    case _ => "Unknown Type"
  }
  

  def present = Action.async {
    implicit request => {
      val deviceConfig = request.session.get("devices").getOrElse("")
      val deviceConfigs:List[String] = findFiles(devicesFileExt)
      Future.successful(Ok(views.html.components(deviceConfigurationForm.fill(DeviceConfiguration(deviceConfig)),
        deviceConfigs, cCToReadableCc(componentCollection(deviceConfig)), type2Description)))
    }
  }

  def findFiles(strFilter:String):List[String] ={
    val myDirectory = new File(".")
    val fileList =
      for (file <- myDirectory.listFiles if file.getName endsWith strFilter) yield {
        file.getName.replaceAll(strFilter, "")
      }
    fileList.toList
  }

  def submit = Action.async {
    implicit request => {
      val deviceConfigs:List[String] = findFiles(devicesFileExt)

      deviceConfigurationForm.bindFromRequest.fold(
        errors => Future.successful(Ok(views.html.components(errors, deviceConfigs,
          cCToReadableCc(componentCollection("")), type2Description))),
        deviceConfig => saveAndPopulate(deviceConfig.currentSequence, deviceConfigs))
    }
  }

  private def saveAndPopulate(deviceConfig:String, deviceConfigs:List[String]):Future[Result] = {
    Future.successful(Ok(views.html.components(deviceConfigurationForm.fill(DeviceConfiguration(deviceConfig)),
      deviceConfigs, cCToReadableCc(componentCollection(deviceConfig)), type2Description)).withSession("devices" -> deviceConfig))
  }
}

object ComponentsController extends ComponentsController{}
