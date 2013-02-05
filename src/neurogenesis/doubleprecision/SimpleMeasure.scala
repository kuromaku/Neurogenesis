package neurogenesis.doubleprecision

import scala.collection.immutable.List

class SimpleMeasure extends ComplexityMeasure {

  def calculateComplexity(allConns: List[AbstractNeuralconnections], bias: Double): Double = { 
    var numConns = 0
    for (c <- allConns) {
      numConns += c.getConns.size
    }
    math.log(bias+numConns)
  }

}