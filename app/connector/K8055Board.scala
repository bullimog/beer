package connector

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/***********************************************************************
 K8055: abstract base trait. Used to interface to external implementation-specific hardware

  Notes...
  Interfaces to JK8055 Java library:
  - public static JK8055 getInstance()
  - public synchronized OpenDevice( int cardAddress )
  - public synchronized CloseDevice()
  - public synchronized CloseAllOpenDevices()

  - public synchronized int ReadAnalogChannel( int channel )
  - public synchronized AllAnalog ReadAllAnalog()
  - public synchronized void OutputAnalogChannel( int channel, int data )
  - public synchronized void OutputAllAnalog( int data1, int data2 )
  - public synchronized void ClearAllAnalog()
  - public synchronized void ClearAnalogChannel( int channel )
  - public synchronized void SetAnalogChannel( int channel )
  - public synchronized void SetAllAnalog()

  - public synchronized void WriteAllDigital( int data )
  - public synchronized void ClearDigitalChannel( int channel )
  - public synchronized void ClearAllDigital()
  - public synchronized void SetDigitalChannel( int channel )
  - public synchronized void SetAllDigital()
  - public synchronized boolean ReadDigitalChannel( int channel )
  - public synchronized int ReadAllDigital()

  - public synchronized void ResetCount( int counterno )
  - public synchronized int ReadCounter( int counterno )
  - public synchronized void SetCounterDebounceTime( int counterno, int debouncetime )
  - public synchronized AllValues ReadAllValues()
  -	public synchronized void SetAllValues( int digitaldata, int analogdata1, int analogdata2 )

  -	public synchronized void SetCurrentDevice( int deviceno )
	- public synchronized int SearchDevices()
  -	public synchronized String Version()


  -	public class AllAnalog {
		  public int data1;
		  public int data2;
	  }

  -	public class AllValues {
	  	public int input;
		  public int analog1;
	  	public int analog2;
  		public int counter1;
  		public int counter2;
	  }

  ************************************************************************/
//trait K8055 {
////  var digitalIn: mutable.MutableList[Boolean]
////  var digitalOut: mutable.MutableList[Boolean]
////  var analogueIn: mutable.MutableList[Double]
////  var analogueOut: mutable.MutableList[Double]
////  var timer:Int = 0
//
//  def setDigitalOut(d: Int, state: Boolean): Unit
//  def getDigitalOut(d: Int): Boolean
//  def getAnalogueIn(d: Int): Double
//  def setAnalogueIn(d: Int, value: Double): Unit
//  def setDigitalIn(d: Int, state: Boolean): Unit
//  def getDigitalIn(d: Int): Boolean
//  def getAnalogueOut(d: Int): Int
//  def setAnalogueOut(d: Int, value: Int): Unit
//
//  def getCount(d: Int): Int
//  def setCount(i: Int, value:Int): Unit
//  def resetCount(d: Int): Unit
//}





/***********************************************************************
 K8055Board: trait for the real thing
************************************************************************/
trait K8055Board extends DeviceConnector{

  // can't read output settings from card, so need to cache state here...
//  var digitalOut:mutable.MutableList[Boolean] = mutable.MutableList(false,false,false,false,false,false,false,false)
  var digitalOut:Byte = 0
  //var analogueOut:mutable.MutableList[Int] = mutable.MutableList(0,0)
  var analogueOut1:Byte = 0
  var analogueOut2:Byte = 0


//  def getPort[T](port:Int):Option[T] ={
//    //println(s"K8055: about to get port: $port from $portList")
//    if((portList.length > port) && (port>=0)) {
//      val ret = Some(portList(port-1))
//      //println(s"K8055:got port: $port : $ret $portList")
//      ret
//    } else {println(s"K8055:couldn't get port: $port from List: $portList"); None}
//  }
  override def getDigitalOut(i:Int): Boolean ={
    val channel:Byte = math.pow(2,i-1).toByte
    //println("getDigitalOut: i="+i + "  channel:"+channel + "digitalOut & channel:"+(digitalOut & channel))
    (digitalOut & channel) > 0
  }
  override def getAnalogueOut(i:Int): Int ={
    i match {
      case 1 => analogueOut1
      case 2 => analogueOut2
      case _ => 0
    }
  }

  override def getDigitalIn(i:Int): Boolean ={readDigitalChannel(i)}
  override def getAnalogueIn(i:Int): Double ={readAnalogueChannel(i)}



// def setChannel[T](portList:mutable.MutableList[T], port:Int, value:T, output:(Int, T) => Unit):Unit ={
//  //println(s"K8055:setting port: $port : $value $portList")
//  if((portList.length > port) && (port>=0)) {
//   portList(port-1) = value
//   output(port, value)
//   //println(s"K8055.setPort: p:$port : v:$value on: $portList")
//  }else println(s"K8055:couldn't set port: $port in List: $portList")
// }

  val fSetDigitalOut = (channel:Int, value:Boolean) => {setDigitalChannel(channel)}
  val fClearDigitalOut = (channel:Int, value:Boolean) => {clearDigitalChannel(channel)}


  override def setDigitalOut(channel:Int, value:Boolean): Unit ={
    value match{
      case true => setDigitalChannel(channel)
      case _ =>    clearDigitalChannel(channel)
    }
  }

//  val fSetAnalogueOut = (channel:Int, value:Int) => {outputAnalogChannel(channel, value)}
  override def setAnalogueOut(i:Int, value:Int): Unit ={ outputAnalogueChannel(i, value)}

  def setAnalogueIn(d: Int, value: Double): Unit = ???
  def setDigitalIn(d: Int, state: Boolean): Unit = ???
  def getCount(d: Int): Int = ???
  def setCount(i: Int, value:Int): Unit = ???
  def resetCount(d: Int): Unit = ???

//
// def getCount(i: Int): Int = {getPort(digitalInCount, i).getOrElse(0)}
// def setCount(i: Int, value:Int): Unit = {setPort(digitalInCount,i, value)}
// def resetCount(i: Int): Unit = {setPort(digitalInCount,i, 0)}

//  def getCount(d: Int): Int
//  def setCount(i: Int, value:Int): Unit
//  def resetCount(d: Int): Unit

  def setDigitalChannel(i:Int):Unit = {
    val channel:Byte = math.pow(2,i-1).toByte
    println("setDigitalChannel: i="+i + "  channel:"+channel + "  digitalOut:"+digitalOut)
    digitalOut = (digitalOut | channel).toByte
    println("  digitalOut & channel:"+(digitalOut & channel))
    println("digitalOut:"+digitalOut)
    executeCommand(s"k8055 -d:$digitalOut")
  }
  def clearDigitalChannel(i:Int):Unit = {
    val channel:Byte = math.pow(2,i-1).toByte
    println("clearDigitalChannel: i="+i + "  channel:"+channel + "  digitalOut:"+digitalOut)
    digitalOut = (digitalOut - channel).toByte
    println("  digitalOut & channel:"+(digitalOut & channel))
    println("digitalOut:"+digitalOut)
    executeCommand(s"k8055 -d:$digitalOut")
  }
  def outputAnalogueChannel(channel:Int, value:Int):Unit = {
    executeCommand(s"k8055 -a$channel:$value")
  }
  def readDigitalChannel(channel:Int):Boolean = {
    true
  }
  def readAnalogueChannel(channel:Int):Int = {
    34
  }

  def executeCommand(command:String): String = {
    println("executeCommand: "+command)
    import sys.process.Process
    val result = Process(""+command+"")
    ""+result.!
  }
}

