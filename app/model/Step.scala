package model

import play.api.libs.json.{Json, JsPath, Reads}
import play.api.libs.functional.syntax._



case class Step(device: Int, eventType:Int, temperature: Option[Double], duration: Option[Int]) {
  override def toString: String ={
    "##Step device:"+device+", eventType:"+decode+", temperature:"+temperature+", duration:"+duration
  }

  def decode:String ={
    eventType match {
      case Step.ON => "On"
      case Step.OFF => "Off"
      case Step.SET_HEAT => "Set Required Heat to "
      case Step.WAIT_HEAT => "Wait for Temperature "
      case Step.WAIT_TIME => "Wait for Duration "
      case Step.SET_COOL => "Set Required Cool to "
      case Step.WAIT_ON => "Wait For"
    }
  }
}

object Step{
  //  Step(event) Types
  val ON = 1          //  1 = turn on
  val OFF = 2         //  2 = turn off
  val SET_HEAT = 3    //  3 = set thermostat (META-DATA = temp[Double])
  val WAIT_HEAT = 4   //  4 = Wait-Temp (META-DATA = temp[Double])
  val WAIT_TIME = 5   //  5 = Wait-Time  (META-DATA = duration[milliseconds])
  val SET_COOL = 6   //  6 = Not yet implemented
  val WAIT_COOL = 7   //  7 = Not yet implemented //TODO
  val WAIT_ON = 8   //  7 = Not yet implemented //TODO


    //  implicit val stepFmt = Json.format[Step]

  implicit val stepReads: Reads[Step] = (
    (JsPath \ "device").read[Int] and
    (JsPath \ "eventType").read[Int] and
    (JsPath \ "temperature").readNullable[Double] and
    (JsPath \ "duration").readNullable[Int]
   )(Step.apply _)

  implicit val stepWrites = Json.writes[Step]
}

case class Sequence(description:String, steps:List[Step])

object Sequence {
  implicit val sequenceReads: Reads[Sequence] = (
     (JsPath \ "description").read[String] and
     (JsPath \ "steps").read[List[Step]]
    )(Sequence.apply _)

  implicit val sequenceWrites = Json.writes[Sequence]
}

case class FriendlyStep(stepId:Int, deviceId: Int, deviceDesc: String, eventType:Int, eventDesc: String,
                        temperature: Option[Double], duration: Option[Int])

case class FriendlySequence(description:String, friendlySteps:List[FriendlyStep])


