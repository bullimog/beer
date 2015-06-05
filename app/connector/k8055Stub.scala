package connector

trait k8055{

}

trait k8055Stub extends k8055board{
  var digitalIn1:Boolean = false
  var digitalIn2:Boolean = false
  var digitalIn3:Boolean = false
  var digitalIn5:Boolean = false

  var digitalOut1:Boolean = false
  var digitalOut2:Boolean = false
  var digitalOut3:Boolean = false
  var digitalOut4:Boolean = false
  var digitalOut5:Boolean = false
  var digitalOut6:Boolean = false
  var digitalOut7:Boolean = false
  var digitalOut8:Boolean = false

  var analogueIn1:Double = 0
  var analogueIn2:Double = 0

  var analogueOut1:Double = 0
  var analogueOut2:Double = 0
}

trait k8055board extends k8055{
  //Real implementations here...
}

