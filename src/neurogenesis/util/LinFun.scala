package neurogenesis.util

class LinFun(x:Double) extends Function1[Double,Double] {
  def apply(a:Double) : Double = a + x
}