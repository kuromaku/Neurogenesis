package neurogenesis.util

class SigmoidExp extends Function1[Double,Double] {
  override
  def apply(x:Double) : Double = (1/(1+Math.exp(-x)))
  override
  def toString : String = "SigmoidExp"
}