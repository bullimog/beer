package connector

import scala.collection.mutable

trait K8055 {
  var digitalIn: mutable.MutableList[Boolean]
  var digitalOut: mutable.MutableList[Boolean]
  var analogueIn: mutable.MutableList[Double]
  var analogueOut: mutable.MutableList[Double]

  def setDigitalOut(d: Int, state: Boolean): Unit

  def getDigitalOut(d: Int): Boolean

  def getAnalogueIn(d: Int): Double

  def setAnalogueIn(d: Int, value: Double): Unit
}

trait K8055Stub extends K8055{
  var digitalIn:mutable.MutableList[Boolean] = mutable.MutableList(false,false,false,false)
  var digitalOut:mutable.MutableList[Boolean] = mutable.MutableList(false,false,false,false,false,false,false,false)
  var analogueIn:mutable.MutableList[Double] = mutable.MutableList(0.0,0.0)
  var analogueOut:mutable.MutableList[Double] = mutable.MutableList(0.0,0.0)

  override def setDigitalOut(d:Int, state:Boolean): Unit ={
    println(s"K8055:setDigitalOut: $d : $state")
    digitalOut(d) = state
    println("K8055:setDigitalOut result= "+digitalOut(d))
  }

  override def getDigitalOut(d:Int): Boolean ={
    digitalOut(d)
  }

  override def getAnalogueIn(d:Int): Double ={
    analogueIn(d)
  }
  override def setAnalogueIn(d:Int, value:Double): Unit ={
    println(s"K8055:setAnalogueIn: $d : $value")
    analogueIn(d) =  value
    println("K8055:setAnalogueIn result= "+analogueIn(d))
  }

}

trait K8055Board extends K8055{
  //Real implementations here...
}

