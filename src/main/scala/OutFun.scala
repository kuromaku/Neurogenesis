package neurogenesis.util

class OutFun extends Function1[Double,Double] {
  override def apply(x:Double) : Double = {
    val y = 2.0/(1+Math.exp(-x))-1
    y
  }
  override def toString : String = "BiSigmoid"
}