package neurogenesis.util

class AdaptiveSchedule(p1:Double,p2:Double,max:Long) extends CoolingSchedule {
  var mutProb = p1
  var flipProb = p2
  def update(f:Double): Unit = {
    var adjustment = Math.log(1/f)
    if (adjustment < 0.1) { adjustment = 0.1 }
    else if (adjustment > 2.6) { adjustment = 2.6 }
    
    mutProb = adjustment*p1*(max-step)/max
    flipProb = adjustment*p2*(max-step)/max
    step += 1
  }

  def getMax: Long = max
  override def toString : String = "AdaptiveSchedule"
}