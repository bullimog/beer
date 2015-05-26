package model


class Step(val device: Int, val eventType:Int, val temperature: Option[Double], val duration: Option[Int]) {
  override def toString(): String ={
    "##Step device:"+device+", eventType:"+eventType+", temperature:"+temperature+", duration:"+duration
  }
}

object Step{
  //  Step(event) Types
  val ON = 1          //  1 = turn on
  val OFF = 2         //  2 = turn off
  val SET_TEMP = 3    //  3 = set thermostat (META-DATA = temp[Double])
  val WAIT_TEMP = 4   //  4 = Wait-Temp (META-DATA = temp[Double])
  val WAIT_TIME = 5   //  5 = Wait-Time  (META-DATA = duration[milliseconds])

  def apply(device: Int, event:Int, temperature: Option[Double], duration: Option[Int]): Step ={
    new Step(device, event, temperature, duration)
  }
}