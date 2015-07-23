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
  var analogueOut1:Int = 0  // 0 to 25,500  (which is 100*255, so we can convert Integers without loss)
  var analogueOut2:Int = 0

  val percentToStoreFactor:Int = 255   // 1% = 255 in the (0 to 25,500) store
  val byteToStoreFactor:Int = 100      // 1 bit = 100 in the store

  val K8055_PORT = 0
  val K8055_TIME = 0
  val K8055_DIGITAL = 1
  val K8055_ANALOG_1 = 2
  val K8055_ANALOG_2 = 3
  val K8055_COUNTER_1 = 4
  val K8055_COUNTER_2 = 5

  def setAnalogueIn(d: Int, value: Double): Unit = ???
  def setDigitalIn(d: Int, state: Boolean): Unit = ???
  def setCount(i: Int, value:Int): Unit = ???

  /** *******************************************************
   * Analogue Out
   **********************************************************/
  override def getAnaloguePercentageOut(channel:Int): Int ={getAnAnalogueOut(channel, percentToStoreFactor)}
  override def getAnalogueOut(channel:Int): Int ={getAnAnalogueOut(channel, byteToStoreFactor)}
  def getAnAnalogueOut(channel:Int, factor: Double): Int ={
    channel match {
      case 1 => (analogueOut1 / factor).toInt
      case 2 => (analogueOut2 / factor).toInt
      case _ => 0
    }
  }

  override def setAnaloguePercentageOut(channel:Int, value:Int): Unit ={setAnAnalogueOut(channel, value, percentToStoreFactor)}
  override def setAnalogueOut(channel:Int, value:Int): Unit ={setAnAnalogueOut(channel, value, byteToStoreFactor)}
  def setAnAnalogueOut(channel:Int, value:Int, factor:Double): Unit ={
    channel match{
      case 1 => analogueOut1 = (value * factor).toInt
      case 2 => analogueOut2 = (value * factor).toInt
      case _ =>
    }
    setStatus()
  }

  /** *******************************************************
    * Analogue In
    **********************************************************/
  override def getAnalogueIn(i:Int): Double ={readAnalogueChannel(i)}

  def readAnalogueChannel(channel:Int):Int = {
    (channel, readStatus()) match{
      case (1, Some(status)) => status(K8055_ANALOG_1).toInt
      case (2, Some(status)) => status(K8055_ANALOG_2).toInt
      case _ => 0
    }
  }



  /** ********************************************************
    * Digital Out
    **********************************************************/
  override def getDigitalOut(channel:Int): Boolean ={
    (digitalOut & byteMask(channel)) > 0
  }

  override def setDigitalOut(channel:Int, value:Boolean): Unit ={
    value match{
      case true => setDigitalChannel(channel)
      case _ =>    clearDigitalChannel(channel)
    }
  }

  private def byteMask(i:Int): Byte = {math.pow(2,i-1).toByte}


  def setDigitalChannel(channel:Int):Unit = {
    digitalOut = (digitalOut | byteMask(channel)).toByte
    setStatus()
  }
  def clearDigitalChannel(channel:Int):Unit = {
    digitalOut = (digitalOut & (255 - byteMask(channel))).toByte
    setStatus()
  }


  /** ********************************************************
    * Digital In
    **********************************************************/

  private def andBitsTogether(source:Byte, mask:Byte): Boolean = {
    if((source & mask) > 0) true
    else false
  }
  override def getDigitalIn(channel:Int): Boolean ={
    readStatus() match {
      case Some(status) => andBitsTogether(status(K8055_DIGITAL).toByte, byteMask(channel))
      case None => false
    }
  }

  def getCount(channel: Int): Int = {
    (channel, readStatus()) match{
      case (1, Some(status)) => status(K8055_COUNTER_1).toInt
      case (2, Some(status)) => status(K8055_COUNTER_2).toInt
      case _ => 0
    }
  }

  def resetCount(channel: Int): Unit = {executeCommand(s"k8055 -reset$channel")}

  def getDigitalInLatch(channel: Int):Boolean = {
    val pressed:Boolean = getCount(channel) > 0
    resetCount(channel)
    pressed
  }



  /** ********************************************************
    * k8055 Communication
    **********************************************************/
  val expectedValCount = 6
  def readStatus():Option[Array[Int]] = {
    val retValues = executeCommand(s"k8055").replaceAll("\n","").split(';')
    try {
      if (retValues.length == expectedValCount) {Some(for (strValue <- retValues) yield {strValue.toInt})}
      else None
    }
    catch {
      case e:NumberFormatException =>  None
    }
  }

  def resetStatus():String = {
    executeCommand(s"k8055 -d:0 -a1:0 -a2:0 -reset1 -reset2")
  }

  def setStatus():String = {
    val byteVal1:Int = analogueOut1/byteToStoreFactor
    val byteVal2:Int = analogueOut2/byteToStoreFactor
    executeCommand(s"k8055 -d:$digitalOut -a1:$byteVal1 -a2:$byteVal2")
  }

  def executeCommand(command:String): String = {
    //println("executeCommand: "+command)
    import sys.process.Process
    try{
      val result = Process(""+command+"")
      result.!!
    }
    catch{
      case e:RuntimeException => {
        println("Communication with k8055 failed")
        ""
      }
    }
  }
}

