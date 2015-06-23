package model

import org.joda.time.{Period, PeriodType, DateTime, Seconds}


object Timer {

  var finishTime:DateTime = DateTime.now
  var step = -1

  def waitingFor(stepId:Int):Boolean={
    stepId == step
  }

  def setTimer(stepId:Int, duration: Int): Unit ={
    finishTime = DateTime.now.plusSeconds(duration)
    step = stepId
  }

  def remainingTime():String= {
    val period = new Period(DateTime.now, finishTime, PeriodType.seconds())
    if(period.getSeconds > 0) period.getSeconds.toString
    else "0"
  }

  def finished(stepId:Int):Boolean={
    if(waitingFor(stepId)) finishTime.isBeforeNow
    else true
  }

  def reset():Unit = {
    finishTime = DateTime.now
    step = -1
  }

}
