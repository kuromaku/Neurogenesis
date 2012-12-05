package neurogenesis.util

class SimpleSchedule(p0:Double,p1:Double,max:Long) extends CoolingSchedule {
  var mutProb = p0
  var flipProb = p1
  val minDataUse = 500
  //val maxSteps = max
  def update(f:Double): Unit = {
    mutProb = 1.0*p0*(max-step)/max
    flipProb = 1.0*p1*(max-step)/max
    step += 1
  }
  def getFeedLength(idx:Int) : Int = {
    val current = getCurrent.toInt
    val m = minDataUse+current
    if (m > dataSizes(idx)) {
      dataSizes(idx)
    }
    else {
      m
    }
  }
  def makeClone : AdaptiveSchedule = {
    val s2 = new AdaptiveSchedule(p0,p1,max)
    s2.setSizes(dataSizes)
    s2
  }
  def getMax : Long = max
  //def clone : SimpleSchedule = new SimpleSchedule(p0,p1,max)
  override def toString : String = "SimpleSchedule"
}