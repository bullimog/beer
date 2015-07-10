package connector

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global



/***********************************************************************
 K8055Board: trait for the real thing
************************************************************************/
trait K8055Board extends DeviceConnector{

  // can't read output settings from card, so need to cache state here...
  var digitalOut:Byte = 0
  var analogueOut1:Double = 0.0
  var analogueOut2:Double = 0.0

  val percentToByteFactor:Double = 2.551
  val byteToPercentFactor:Double = 1/2.55


  val K8055_PORT = 0
  val K8055_TIME = 0
  val K8055_DIGITAL = 1
  val K8055_ANALOG_1 = 2
  val K8055_ANALOG_2 = 3
  val K8055_COUNTER_1 = 4
  val K8055_COUNTER_2 = 5

  def setAnalogueIn(d: Int, value: Double): Unit = ???
  def setDigitalIn(d: Int, state: Boolean): Unit = ???
  def getCount(d: Int): Int = ???
  def setCount(i: Int, value:Int): Unit = ???
  def resetCount(d: Int): Unit = ???


  override def getAnaloguePercentageOut(channel:Int): Int ={getAnAnalogueOut(channel, byteToPercentFactor)}
  override def getAnalogueOut(channel:Int): Int ={getAnAnalogueOut(channel, 1)}
  def getAnAnalogueOut(channel:Int, factor: Double): Int ={
    channel match {
      case 1 => (analogueOut1 * factor).toInt
      case 2 => (analogueOut2 * factor).toInt
      case _ => 0
    }
  }

  override def setAnaloguePercentageOut(channel:Int, value:Int): Unit ={setAnAnalogueOut(channel, value, percentToByteFactor)}
  override def setAnalogueOut(channel:Int, value:Int): Unit ={setAnAnalogueOut(channel, value, 1)}
  def setAnAnalogueOut(channel:Int, value:Int, factor:Double): Unit ={
    channel match{
      case 1 => analogueOut1 = (value * factor).toInt
      case 2 => analogueOut2 = (value * factor).toInt
      case _ =>
    }
    setStatus()
  }

  override def getAnalogueIn(i:Int): Double ={readAnalogueChannel(i)}

  def readAnalogueChannel(channel:Int):Int = {
    (channel, readStatus()) match{
      case (1, Some(status)) => status(K8055_ANALOG_1).toInt
      case (2, Some(status)) => status(K8055_ANALOG_2).toInt
      case _ => 0
    }
  }


  def byteMask(i:Int): Byte = {math.pow(2,i-1).toByte}

  override def getDigitalOut(channel:Int): Boolean ={
    (digitalOut & byteMask(channel)) > 0
  }

  override def getDigitalIn(channel:Int): Boolean ={
    readStatus() match {
      case Some(status) => andBits(status(K8055_DIGITAL).toByte, byteMask(channel))
      case None => false
    }
  }

  override def setDigitalOut(channel:Int, value:Boolean): Unit ={
    value match{
      case true => setDigitalChannel(channel)
      case _ =>    clearDigitalChannel(channel)
    }
  }


  def setDigitalChannel(channel:Int):Unit = {
    digitalOut = (digitalOut | byteMask(channel)).toByte
    setStatus()
  }
  def clearDigitalChannel(channel:Int):Unit = {
    digitalOut = (digitalOut & (255 - byteMask(channel))).toByte
    setStatus()
  }




  def andBits(source:Byte, mask:Byte): Boolean = {
    if((source & mask) > 0) true
    else false
  }



  def readStatus():Option[Array[String]] = {
    val result = executeCommand(s"k8055")
    val retVal = result.split(';')
    if(retVal.length > 5){Some(retVal)}
    else None
  }

  def resetStatus():String = {
    executeCommand(s"k8055 -d:0 -a1:0 -a2:0 -reset1 -reset2")
  }

  def setStatus():String = {
    val byteVal1:Int = analogueOut1.toInt
    val byteVal2:Int = analogueOut2.toInt
    executeCommand(s"k8055 -d:$digitalOut -a1:$byteVal1 -a2:$byteVal2")
  }

  def executeCommand(command:String): String = {
    println("executeCommand: "+command)
    import sys.process.Process
    val result = Process(""+command+"")
    result.!!
  }
}

