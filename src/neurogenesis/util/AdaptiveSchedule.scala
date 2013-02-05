package neurogenesis.util

class AdaptiveSchedule(p1:Double,p2:Double,max:Long) extends CoolingSchedule {
  var mutProb = p1
  var flipProb = p2
  val minDataUse = 500
  /*Modifies the mutation parameters based on current fitness value
   * 
   */
  def update(f:Double): Unit = { //TODO: think of a better scheme
    var adjustment = if (f < 1.0 && f > 0.0) 2.0 else if (f >= 1.0 && f <= 5) 2.0/f else 0.2
    
    mutProb = adjustment*p1*(max-step)/max
    if (mutProb > 0.9) {
      mutProb = 0.9
    }
    flipProb = adjustment*p2*(max-step)/max
    if (flipProb > 0.5) {
      flipProb = 0.5
    }
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
    val s2 = new AdaptiveSchedule(p1,p2,max)
    s2.setSizes(dataSizes)
    s2
  }
  def getMax: Long = max
  //def clone : AdaptiveSchedule = new AdaptiveSchedule(p1,p2,max)
  override def toString : String = "AdaptiveSchedule (p1: "+p1.toString+",p2: "+p2.toString+")"
}