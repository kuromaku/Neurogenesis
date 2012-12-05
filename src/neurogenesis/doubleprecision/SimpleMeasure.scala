package neurogenesis.doubleprecision

import scala.collection.immutable.List

class SimpleMeasure extends ComplexityMeasure {

  def calculateComplexity(allConns: List[NeuralConnsD], bias: Double): Double = { 
    var numConns = 0
    for (c <- allConns) {
      numConns += c.conns.size
    }
    math.log(bias+numConns)
  }

}