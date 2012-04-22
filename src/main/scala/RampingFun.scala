package neurogenesis.util

class RampingFun extends Function1[Double, Double] {
  val s = .3
  def apply(x:Double) : Double = {
    if (x < (-1/s)) -1
    else if (x > (1/s)) 1
    else
      x*s
  }
}