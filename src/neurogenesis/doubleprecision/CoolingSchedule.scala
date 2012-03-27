package neurogenesis.doubleprecision

abstract class CoolingSchedule {
  var mutProb:Double
  var flipProb:Double
  var step = 1L
  def update(f:Double) : Unit
  def getProb1 : Double = mutProb
  def getProb2 : Double = flipProb
  def getMax : Long
  def getCurrent: Long = step
}