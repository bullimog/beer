package equipment


class HeatingElement(name: String, family: String) extends Equipment(name, family) with Controllable {

  val id = getId

  override def setPower(power: Int): Unit = {
    println("Element has been set to: " + power + "%")
  }
}

object HeatingElement{
  def apply(name: String): HeatingElement ={
    new HeatingElement(name, "Heating Element")
  }
}
