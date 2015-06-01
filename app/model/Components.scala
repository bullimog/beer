package model

import akka.actor.Cancellable


trait Component{
  def id: Int
  def description: String
  def deviceType: Int
  var cancellable: Option[Cancellable]
}

object Component {
  val ANALOGUE_IN = 1
  val ANALOGUE_OUT = 2
  val DIGITAL_IN = 3
  val DIGITAL_OUT = 4
  val MONITOR = 5
}

case class ComponentCollection(name: String, description: String, devices: List[Component],
                               thermostats: List[Thermostat])

case class Device(override val id: Int, override val description: String,
                  override val deviceType: Int, override var cancellable:Option[Cancellable],
                  port:Int) extends Component

case class Thermostat(override val id: Int, override val description: String,
                      override val deviceType: Int, override var cancellable:Option[Cancellable],
                      thermometer:Component, heater:Component) extends Component

