package model

import org.joda.time.{Period, PeriodType, DateTime, Seconds}


object Timer {

  var finishTime:DateTime = DateTime.now
  var step = -1

  def waitingFor(stepId:Int):Boolean={stepId == step}

  def setTimer(stepId:Int, duration: Int): Unit ={
    finishTime = DateTime.now.plusSeconds(duration)
    println(s"##setTimer: step=$stepId and duration=$duration and finishTime=$finishTime")
    step = stepId
  }

  def remainingTime():Int= {
    new Period(DateTime.now, finishTime, PeriodType.seconds()).getSeconds
  }

  def finished(stepId:Int):Boolean={
    if(waitingFor(stepId)) finishTime.isAfterNow
    else true
  }

}
