package neurogenesis.doubleprecision

class SimpleMeasure extends ComplexityMeasure {

  def calculateComplexity(allConns: List[NeuralConnsD],bias:Double): Double = {
    var numConns = 0
    for (c <- allConns) {
      numConns += c.size
    }
    math.log(bias+numConns)
  }

}