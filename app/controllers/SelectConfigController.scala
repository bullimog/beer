package controllers

import java.io.File

import controllers.StatusController._
import play.api.Routes
import play.api.mvc.{Controller, Action}

import scala.concurrent.Future



trait SelectConfigController extends Controller{
  def present = Action.async {

    val sequences = findFiles("1.json")
    val deviceConfigs = findFiles(".json")

    Future.successful(Ok(views.html.configSelect(sequences, deviceConfigs)))

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