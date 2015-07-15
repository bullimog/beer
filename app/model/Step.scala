package model

import play.api.libs.json.{Json, JsPath, Reads}
import play.api.libs.functional.syntax._



case class Step(id: Int, device: Int, eventType:Int, target: Option[Double], duration: Option[Int]) {
  override def toString: String ={
    "##Step device:"+device+", eventType:"+decode+", target:"+target+", duration:"+duration
  }

  def decode:String ={
    eventType match {
      case Step.ON => "On"
      case Step.OFF => "Off"
      case Step.SET_HEAT => "Set to "
      case Step.WAIT_HEAT => "Wait until reading rises to "
      case Step.WAIT_TIME => "Wait for "
      case Step.SET_COOL => "Set to "
      case Step.WAIT_ON => "Wait for "
    }
  }
}

object Step{
  //  Step(event) Types
  val ON = 1          //  1 = turn on
  val OFF = 2         //  2 = turn off
  val SET_HEAT = 3    //  3 = set monitor (META-DATA = temp[Double])
  val WAIT_HEAT = 4   //  4 = Wait-Temp   (META-DATA = temp[Double])
  val WAIT_TIME = 5   //  5 = Wait-Time   (META-DATA = duration[milliseconds])
  val SET_COOL = 6    //  6 = Not yet implemented
  val WAIT_COOL = 7   //  7 = Not yet implemented //TODO
  val WAIT_ON = 8     //  8 = Not yet implemented //TODO
  val WAIT_COUNT = 9  //  9 = Not yet implemented //TODO


    //  implicit val stepFmt = Json.format[Step]

  implicit val stepReads: Reads[Step] = (
    (JsPath \ "id").read[Int] and
    (JsPath \ "device").read[Int] and
    (JsPath \ "eventType").read[Int] and
    (JsPath \ "target").readNullable[Double] and
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

case class ReadableStep(stepId:Int, deviceId: Int, deviceDesc: String, eventType:Int, eventDesc: String,
                        temperature: Option[String], duration: Option[String])

case class ReadableSequence(description:String, friendlySteps:List[ReadableStep], currentStep:Int)


case class SequenceStatus(running:Boolean, currentStep:Int, componentStatuses:List[ComponentStatus],
                          monitorStatuses:List[MonitorStatus])
object  SequenceStatus {
  implicit val formats=Json.writes[SequenceStatus]
}
