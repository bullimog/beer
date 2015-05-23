package equipment


abstract class Equipment(name: String, family: String) {

  var lastId: Int = 0

  val getId: Int = {
    lastId += 1
    lastId
  }

  override def toString = "type:"+family +"; name:" + name

}




trait Controllable {
  def setPower(power: Int): Unit = {
    println("Controllable item has been set to: " + power + "%")
  }
}

trait Switchable {

  def On: Unit = {
    println("Switchable has been turned on")
    //on = true
  }


  def Off: Unit = {
    println("Switchable has been turned off")
    //on = false
  }
}


trait Readable{

  def read: Double = {
    println("Rading from Readable")
    37.1
  }


}