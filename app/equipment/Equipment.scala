package equipment


class Equipment(id: Int) {}


trait Controllable {
  def setPower(power: Int): Unit = {
    println("Element has been set to: " + power + "%")
  }
}