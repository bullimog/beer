package connector

import scala.collection.mutable

/***********************************************************************
 K8055Stub: testing trait
  ************************************************************************/
trait DeviceConnectorStub extends DeviceConnector{
  var digitalIn:mutable.MutableList[Boolean] = mutable.MutableList(false,false,false,false,false)
  var digitalOut:mutable.MutableList[Boolean] = mutable.MutableList(false,false,false,false,false,false,false,false)
  var analogueIn:mutable.MutableList[Double] = mutable.MutableList(41.0,0.0)
  var analogueOut:mutable.MutableList[Int] = mutable.MutableList(0,0)
  var digitalInCount:mutable.MutableList[Int] = mutable.MutableList(0,0)

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
  override def getAnaloguePercentageOut(i:Int): Int ={getPort(analogueOut,i).getOrElse(0)}
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
  override def setAnaloguePercentageOut(i:Int, value:Int): Unit ={ setPort(analogueOut, i, value)}

  override def getCount(i: Int): Int = {getPort(digitalInCount, i).getOrElse(0)}
  override def setCount(i: Int, value:Int): Unit = {setPort(digitalInCount,i, value)}
  override def resetCount(i: Int): Unit = {setPort(digitalInCount,i, 0)}

}
