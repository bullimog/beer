package connector

import java.io.{FileNotFoundException, File, PrintWriter}

import model.{ComponentCollection, Sequence}
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.io.Source


object ConfigIO {

  val devicesFileExt:String = "-devices.json"
  val sequenceFileExt:String = "-sequence.json"

  def readComponentCollection(json: JsValue):Option[ComponentCollection] = {
    json.validate[ComponentCollection] match {
      case s: JsSuccess[ComponentCollection] => Some(s.get)
      case e: JsError => None //new ComponentCollection("None", "Collection not found", List(), List())
    }
  }

  def readComponentCollection(fileName:String):Option[ComponentCollection] = {
    try{
      val source = Source.fromFile(fileName, "UTF-8")
      val json: JsValue = Json.parse(source.mkString)
      readComponentCollection(json)
    }catch{
      case e:FileNotFoundException => None
    }
  }


  def writeComponentCollection(fileName: String, componentCollection: ComponentCollection):Unit = {
    writeFile(fileName, Json.toJson(componentCollection))
  }

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

  def writeSteps(fileName: String, sequence:Sequence): Unit ={
    writeFile(fileName, Json.toJson(sequence))
  }

  def writeFile(fileName: String, json:JsValue):Unit = {
    val writer = new PrintWriter(new File(fileName))
    writer.write(Json.prettyPrint(json))
    writer.close()
  }
}
