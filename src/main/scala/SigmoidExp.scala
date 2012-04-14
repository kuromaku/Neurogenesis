package neurogenesis.util

class SigmoidExp extends Function1[Double,Double] {
  override
  def apply(x:Double) : Double = (1/(1+scala.math.exp(-x)))
  override
  def toString : String = "SigmoidExp"
}