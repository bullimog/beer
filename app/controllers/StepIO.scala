package controllers

import java.io.{File, PrintWriter}

import model.Sequence
import play.api.libs.json.{Json, JsError, JsSuccess, JsValue}

import scala.io.Source


object StepIO {

  def readSteps(json: JsValue):Sequence = {
    json.validate[Sequence] match {
      case s: JsSuccess[Sequence] => s.get
      case e: JsError => println("jsError: "+e ); new Sequence("No Sequence Found", List())
    }
  }

  def readSteps(fileName:String):Sequence = {
    val source = Source.fromFile(fileName, "UTF-8")
    val json: JsValue = Json.parse(source.mkString)
    readSteps(json)
  }

  def writeSteps(fileName: String, json:JsValue):Unit = {
    val writer = new PrintWriter(new File(fileName))
    writer.write(Json.prettyPrint(json))
    writer.close()
  }

  def writeSteps(fileName: String, sequence:Sequence): Unit ={
    writeSteps(fileName, Json.toJson(sequence))
  }
}
