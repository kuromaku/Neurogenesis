package neurogenesis.util

class SimpleSchedule(p0:Double,p1:Double,max:Long) extends CoolingSchedule {
  var mutProb = p0
  var flipProb = p1
  //val maxSteps = max
  def update(f:Double): Unit = {
    mutProb = 1.0*p0*(max-step)/max
    flipProb = 1.0*p1*(max-step)/max
    step += 1
  }
  def getMax : Long = max
  //def clone : SimpleSchedule = new SimpleSchedule(p0,p1,max)
  override def toString : String = "SimpleSchedule"
}