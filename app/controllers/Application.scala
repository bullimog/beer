package controllers

import akka.actor.ActorSystem
import equipment._
import play.api._
import play.api.mvc._

import scala.collection.mutable.ListBuffer

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
}

object Beer extends App{
  println("Hello")

  val thermometer1 = Thermometer("myThermometer")
  val pump1 = Pump("myPump")
  val heatingElement1 = HeatingElement("myHeater")


//  val parts = new ListBuffer[Equipment]()
//  parts += thermometer1
//  parts += pump1
//  parts += heatingElement1


  val boiler1 = Boiler("myBoiler", heatingElement1, thermometer1, pump1)
  boiler1.circulateOn
  println("m1")
  val possible = boiler1.maintainTemperature(42.0, 10) // 42 degrees for 10 seconds
  println("possible:" + possible)

}