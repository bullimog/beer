package model

import org.joda.time.{Period, PeriodType, DateTime, Seconds}


object Timer {

  var finishTime:DateTime = DateTime.now
  var step = -1

  def waitingFor(stepId:Int):Boolean={
    //println("##waitingFor: "+step)
    stepId == step
  }

  def setTimer(stepId:Int, duration: Int): Unit ={
    finishTime = DateTime.now.plusSeconds(duration)
    //println(s"##setTimer: step=$stepId and duration=$duration and finishTime=$finishTime")
    step = stepId
  }

  def remainingTime():Int= {
    val period = new Period(DateTime.now, finishTime, PeriodType.seconds())
    //println("##remainingTime: "+period.getSeconds)
    if(period.getSeconds > 0) period.getSeconds
    else 0
  }

  def finished(stepId:Int):Boolean={
    if(waitingFor(stepId)) finishTime.isBeforeNow
    else true
  }

}
