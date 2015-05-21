package equipment


class Element(id: Int, power: Int) extends Equipment(id) with Controllable {


  override def setPower(power: Int): Unit = {
    println("Element has been set to: " + power + "%")
  }

}
