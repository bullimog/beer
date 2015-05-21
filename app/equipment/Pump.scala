package equipment


class Pump extends Switchable(id, on){

  def On(): Unit = {
    println("Pump has been turned on")
    on = true
  }

  def Off(): Unit = {
    println("Pump has been turned off")
    on = false
  }
}
