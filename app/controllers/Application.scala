package controllers

import akka.actor.ActorSystem
import equipment._
import play.api._
import play.api.mvc._
import sequencer.{Sequencer}

import scala.collection.mutable.ListBuffer

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
}

object Beer extends App{
  println("Hello")
  //find the item of Equipment, for the given step
//  val getEquipment = (l:List[Int], equipmentList:List[Equipment]) => {
//    val equipmentId:Int = l.head
//    println("equipmentId="+equipmentId)
//    val fn = (equipment:Equipment) => (equipment.id == equipmentId)
//    println("fn="+fn)
//    val item:Equipment = parts.filter(fn).head
//    println("item="+item)
//    item
//
//    equipmentList.filter((e:Equipment) => e.id == l.head).head
//  }

  //instantiate Equipment singleton Object
  //val equipment = Equipment

  //instantiate Sequencer singleton Object
  val sequencer = Sequencer






  //val boiler1 = Boiler(4, "myBoiler", heatingElement1, thermometer1, pump1)
  //boiler1.circulateOn
  //println("m1")
  //val possible = boiler1.maintainTemperature(42.0) // 42 degrees
  //println("possible:" + possible)


}