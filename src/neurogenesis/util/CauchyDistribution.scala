package neurogenesis.util

class CauchyDistribution(scale0:Double) extends Distribution {
  var scale = scale0
  def getScale : Double = scale
  override def probability(x:Double) : Double = {
    getScale/(scala.math.Pi*scale*scale*x*x)
  }
  override def inverse(x:Double) : Double = {
	-scale/(scala.math.tan(scala.math.Pi*x))
  }
  def adjust(factor:Double) : Unit = {
    scale *= factor
  }
  override def toString : String = "CauchyDistribution (scale: "+scale+")"
}