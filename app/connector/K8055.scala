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
  var digitalOut:mutable.MutableList[Boolean] = mutable.MutableList(false,false,false,false,false,false,false,false)
  var analogueOut:mutable.MutableList[Int] = mutable.MutableList(0,0)

 def getPort[T](portList:mutable.MutableList[T], port:Int):Option[T] ={
  //println(s"K8055: about to get port: $port from $portList")
  if((portList.length > port) && (port>=0)) {
   val ret = Some(portList(port-1))
   //println(s"K8055:got port: $port : $ret $portList")
   ret
  } else {println(s"K8055:couldn't get port: $port from List: $portList"); None}
 }
// override def getDigitalOut(i:Int): Boolean ={getPort(digitalOut,i).getOrElse(false)}
// override def getDigitalIn(i:Int): Boolean ={getPort(digitalIn,i).getOrElse(false)}
// override def getAnalogueIn(i:Int): Double ={getPort(analogueIn,i).getOrElse(0)}
// override def getAnalogueOut(i:Int): Int ={getPort(analogueOut,i).getOrElse(0)}
//
// def setPort[T](portList:mutable.MutableList[T], port:Int, value:T):Unit ={
//  //println(s"K8055:setting port: $port : $value $portList")
//  if((portList.length > port) && (port>=0)) {
//   portList(port-1) = value
//   //println(s"K8055.setPort: p:$port : v:$value on: $portList")
//  }else println(s"K8055:couldn't set port: $port in List: $portList")
// }
// override def setDigitalOut(i:Int, value:Boolean): Unit ={ setPort(digitalOut, i, value)}
// override def setDigitalIn(i:Int, value:Boolean): Unit ={ setPort(digitalIn, i, value)}
// override def setAnalogueIn(i:Int, value:Double): Unit ={ setPort(analogueIn, i, value)}
// override def setAnalogueOut(i:Int, value:Int): Unit ={ setPort(analogueOut, i, value)}
//
// def getCount(i: Int): Int = {getPort(digitalInCount, i).getOrElse(0)}
// def setCount(i: Int, value:Int): Unit = {setPort(digitalInCount,i, value)}
// def resetCount(i: Int): Unit = {setPort(digitalInCount,i, 0)}

//  def getCount(d: Int): Int
//  def setCount(i: Int, value:Int): Unit
//  def resetCount(d: Int): Unit
}

