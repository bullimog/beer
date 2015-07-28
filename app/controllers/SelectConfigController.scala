package controllers

import java.io.File

import controllers.StatusController._
import play.api.Routes
import play.api.mvc.{Controller, Action}

import scala.concurrent.Future



trait SelectConfigController extends Controller{
  def present = Action.async {
    implicit request => {
      val sequence = request.getQueryString("sequence")
      val sequences = findFiles("-sequence.json")
      val deviceConfigs = findFiles("-devices.json")
      val state = (sequence, sequences, deviceConfigs)

      Future.successful(Ok(views.html.configSelect(sequences, deviceConfigs)))
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


object SelectConfigController extends SelectConfigController