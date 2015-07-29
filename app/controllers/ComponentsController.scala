package controllers

import java.io.File

import connector.ConfigIO
import controllers.StatusController._
import forms.DeviceConfigurationForm._
import model.{DeviceConfiguration, Component}
import play.api.mvc.Action

import scala.concurrent.Future


trait ComponentsController {
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
      val deviceConfig = "second" // request.session.get("devices").getOrElse("")
      println("deviceConfig = "+deviceConfig)
      val deviceConfigs = findFiles("-devices.json")
      Future.successful(Ok(views.html.components(deviceConfigurationForm.fill(DeviceConfiguration(currentSequence=deviceConfig)),
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
}

object ComponentsController extends ComponentsController{}
