package equipment


trait Pump {

  def turnOnPump(): Unit = {
    println("Pump has been turned on")
  }

  def turnOffPump(): Unit = {
    println("Pump has been turned off")
  }
}
