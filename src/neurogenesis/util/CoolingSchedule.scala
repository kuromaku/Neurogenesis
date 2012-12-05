package neurogenesis.util

abstract class CoolingSchedule {
  var mutProb:Double
  var flipProb:Double
  var step = 1L
  val dataSizes:Array[Int] = new Array[Int](4)
  val feedLengths = new Array[Int](4)
  def getFeedLength(idx:Int) : Int
  def update(f:Double) : Unit
  def getProb1 : Double = mutProb
  def getProb2 : Double = flipProb
  def getMax : Long
  def getCurrent: Long = step
  def setSizes(sizes:Array[Int]) : Unit = { 
    for (i <- 0 until sizes.length) dataSizes(i) = sizes(i) 
  }
  def makeClone : CoolingSchedule
  //override def clone : CoolingSchedule
}