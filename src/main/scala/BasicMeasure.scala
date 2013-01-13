package neurogenesis.util

import neurogenesis.doubleprecision.NeuralConnsD

class BasicMeasure(bias:Double) extends CComplexityMeasure {

  def calculateComplexity(c: NeuralConnsD): Double = { 
    var sum = 0.0
    for ((t,(w,b)) <- c.getConns2) {
      if (b) {
        sum += math.abs(w) 
      }
    }
    math.log(bias+sum)
  }
  override def toString : String = "BasicMeasure"

}