package controllers

import java.io.{File, PrintWriter}

import model.{Sequence, ComponentCollection}
import play.api.libs.json.{JsError, JsSuccess, Json, JsValue}
import scala.io.Source


object ComponentIO {


  def readComponentCollection(json: JsValue):ComponentCollection = {
    json.validate[ComponentCollection] match {
      case s: JsSuccess[ComponentCollection] => s.get
      case e: JsError => new ComponentCollection("None", "Collection not found", List(), List())
    }
  }

  def readComponentCollection(fileName:String):ComponentCollection = {
    val source = Source.fromFile(fileName, "UTF-8")
    val json: JsValue = Json.parse(source.mkString)
    readComponentCollection(json)
  }

  def writeComponentCollection(fileName: String, json:JsValue):Unit = {
    val writer = new PrintWriter(new File(fileName))
    writer.write(Json.prettyPrint(json))
    writer.close()
  }

  def writeComponentCollection(fileName: String, componentCollection: ComponentCollection):Unit = {
    writeComponentCollection(fileName, Json.toJson(componentCollection))
  }


}
