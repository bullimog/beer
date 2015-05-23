package equipment


class Pump(name: String, family: String) extends Equipment(name, family) with Switchable{

  val id = getId

  def on = {}
  def off = {}

}


object Pump{
  def apply(name: String): Pump ={
    new Pump(name, "Pump")
  }
}