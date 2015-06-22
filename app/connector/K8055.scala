package connector

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/***********************************************************************
 K8055: abstract base trait
************************************************************************/
trait K8055 {
//  var digitalIn: mutable.MutableList[Boolean]
//  var digitalOut: mutable.MutableList[Boolean]
//  var analogueIn: mutable.MutableList[Double]
//  var analogueOut: mutable.MutableList[Double]
  var timer:Int = 0

  def setDigitalOut(d: Int, state: Boolean): Unit
  def getDigitalOut(d: Int): Boolean
  def getAnalogueIn(d: Int): Double
  def setAnalogueIn(d: Int, value: Double): Unit
  def setDigitalIn(d: Int, state: Boolean): Unit
  def getDigitalIn(d: Int): Boolean
  def getAnalogueOut(d: Int): Int
  def setAnalogueOut(d: Int, value: Int): Unit
//  def getTime(): Int ={timer}
//  def setTime(value:Int):Unit ={
//    timer = value
//    Future {runTimer()}
//  }
//
//  @tailrec  //TODO: Could Akka do this better?
//  final def runTimer():Unit = {
//    if(timer > 0) {
//      timer = timer - 1
//      Thread.sleep(1000)
//      runTimer()
//    }
//  }
}

/***********************************************************************
 K8055Stub: testing trait
************************************************************************/
trait K8055Stub extends K8055{
  var digitalIn:mutable.MutableList[Boolean] = mutable.MutableList(false,false,false,false,false)
  var digitalOut:mutable.MutableList[Boolean] = mutable.MutableList(false,false,false,false,false,false,false,false)
  var analogueIn:mutable.MutableList[Double] = mutable.MutableList(41.0,0.0)
  var analogueOut:mutable.MutableList[Int] = mutable.MutableList(0,0)


  def getPort[T](portList:mutable.MutableList[T], port:Int):Option[T] ={
    //println(s"K8055: about to get port: $port from $portList")
    if((portList.length > port) && (port>=0)) {
      val ret = Some(portList(port-1))
      //println(s"K8055:got port: $port : $ret $portList")
      ret
    } else {println(s"K8055:couldn't get port: $port from List: $portList"); None}
  }
  override def getDigitalOut(i:Int): Boolean ={getPort(digitalOut,i).getOrElse(false)}
  override def getDigitalIn(i:Int): Boolean ={getPort(digitalIn,i).getOrElse(false)}
  override def getAnalogueIn(i:Int): Double ={getPort(analogueIn,i).getOrElse(0)}
  override def getAnalogueOut(i:Int): Int ={getPort(analogueOut,i).getOrElse(0)}

  def setPort[T](portList:mutable.MutableList[T], port:Int, value:T):Unit ={
    //println(s"K8055:setting port: $port : $value $portList")
    if((portList.length > port) && (port>=0)) {
      portList(port-1) = value
      //println(s"K8055.setPort: p:$port : v:$value on: $portList")
    }else println(s"K8055:couldn't set port: $port in List: $portList")
  }
  override def setDigitalOut(i:Int, value:Boolean): Unit ={ setPort(digitalOut, i, value)}
  override def setDigitalIn(i:Int, value:Boolean): Unit ={ setPort(digitalIn, i, value)}
  override def setAnalogueIn(i:Int, value:Double): Unit ={ setPort(analogueIn, i, value)}
  override def setAnalogueOut(i:Int, value:Int): Unit ={ setPort(analogueOut, i, value)}

}



/***********************************************************************
 K8055Board: trait for the real thing
************************************************************************/
trait K8055Board extends K8055{
  //Real implementations here...
}

