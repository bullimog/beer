package equipment


class Equipment(id: Int) {}


trait Controllable {
  def setPower(power: Int): Unit = {
    println("Controllable item has been set to: " + power + "%")
  }
}

class Switchable(id: Int, var on: Boolean) extends Equipment(id) {


  def On: Unit = {
    println("Switchable has been turned on")
    on = true
  }


  def Off: Unit = {
    println("Switchable has been turned off")
    on = false
  }
}


class Readable(id: Int, var value: Double) extends Equipment(id){

  def read: Double = {
    println("Rading from Readable")
    37.1
  }


}