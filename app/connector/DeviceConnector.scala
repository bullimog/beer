package connector

/**
 * abstract base trait. Used to interface to external implementation-specific hardware
 */
trait DeviceConnector {
    def setDigitalOut(d: Int, state: Boolean): Unit
    def getDigitalOut(d: Int): Boolean
    def getAnalogueIn(d: Int): Double
    def setAnalogueIn(d: Int, value: Double): Unit
    def setDigitalIn(d: Int, state: Boolean): Unit
    def getDigitalIn(d: Int): Boolean
    def getAnalogueOut(d: Int): Int
    def setAnalogueOut(d: Int, value: Int): Unit

    def getCount(d: Int): Int
    def setCount(i: Int, value:Int): Unit
    def resetCount(d: Int): Unit
}
