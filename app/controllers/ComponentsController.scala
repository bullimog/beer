package controllers

import java.io.File

import connector.ConfigIO
import controllers.StatusController._
import forms.DeviceConfigurationForm._
import model.{DeviceConfiguration, Component}
import play.api.mvc.{Result, Action}

import scala.concurrent.Future


trait ComponentsController {
  val DevicesFileExt:String = "-devices.json"

  val componentCollection = ConfigIO.readComponentCollection("deviceSetup.json").get  //TODO remove hardcoded file

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
      println("### deviceConfig = "+deviceConfig)
      val deviceConfigs:List[String] = findFiles(DevicesFileExt)
      Future.successful(Ok(views.html.components(deviceConfigurationForm.fill(DeviceConfiguration(deviceConfig)),
        deviceConfigs, cCToReadableCc(componentCollection), type2Description)))
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
      val deviceConfigs:List[String] = findFiles(DevicesFileExt)

      deviceConfigurationForm.bindFromRequest.fold(
        errors => Future.successful(Ok(views.html.components(errors, deviceConfigs,
          cCToReadableCc(componentCollection), type2Description))),
        deviceConfig => saveAndPopulate(deviceConfig, deviceConfigs))
    }
  }

  private def saveAndPopulate(deviceConfig:DeviceConfiguration, deviceConfigs:List[String]):Future[Result] = {
    Future.successful(Ok(views.html.components(deviceConfigurationForm.fill(DeviceConfiguration(deviceConfig.currentSequence)),
      deviceConfigs, cCToReadableCc(componentCollection), type2Description)).withSession("devices" -> deviceConfig.currentSequence))
  }
}

object ComponentsController extends ComponentsController{}
