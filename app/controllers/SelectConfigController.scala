package controllers

import java.io.File

import controllers.StatusController._
import play.api.Routes
import play.api.mvc.{Controller, Action}

import scala.concurrent.Future



trait SelectConfigController extends Controller{
  def present = Action.async {

    val sequences = findFiles("sequence.json")
    val deviceConfigs = findFiles("devices.json")

    Future.successful(Ok(views.html.configSelect(sequences, deviceConfigs)))

  }

  def findFiles(filter:String):List[String] ={
    val myDirectory = new File(".")
    val fileList =
    for (file <- myDirectory.listFiles if file.getName endsWith filter) yield {
      file.getName
    }
    fileList.toList
  }
}


object SelectConfigController extends SelectConfigController