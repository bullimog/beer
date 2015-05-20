package Equipment


trait Element {

  def setElement(percentage: Int): Unit ={
    println("Element has been set to: " + percentage + "%")
  }
}
