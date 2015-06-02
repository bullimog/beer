package model

import akka.actor.Cancellable
import play.api.libs.json.Json


trait Component{
  def id: Int
  def description: String
  def deviceType: Int
//  var cancellable: Option[Cancellable]
}

object Component {
  val ANALOGUE_IN = 1
  val ANALOGUE_OUT = 2
  val DIGITAL_IN = 3
  val DIGITAL_OUT = 4
  val MONITOR = 5
}

case class ComponentCollection(name: String, description: String, devices: List[Device],
                               thermostats: List[Thermostat])
//object ComponentCollection{
//  implicit val componentCollectionFmt = Json.format[ComponentCollection]
//}

case class Device(override val id: Int, override val description: String,
                  override val deviceType: Int, port:Int) extends Component
//object Device{
//  implicit val deviceFmt = Json.format[Device]
//}

case class Thermostat(override val id: Int, override val description: String,
                      override val deviceType: Int, thermometer:Int, heater:Int) extends Component
//                      override val deviceType: Int, thermometer:Device, heater:Device) extends Component
//object Thermostat{
//  implicit val thermostatFmt = Json.format[Thermostat]
//}