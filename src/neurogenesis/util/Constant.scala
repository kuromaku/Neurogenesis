package neurogenesis.util

import neurogenesis.doubleprecision.NeuralConnsD

/*The simplest measure which always returns the same value
 * 
 */
class Constant(v:Double) extends CComplexityMeasure {

  def calculateComplexity(c: NeuralConnsD): Double = { v }
  override def toString : String = "Constant"
}