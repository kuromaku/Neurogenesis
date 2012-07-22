package neurogenesis.doubleprecision

class TotalMeasure extends ComplexityMeasure {

  def calculateComplexity(allConns: List[NeuralConnsD], bias: Double): Double = {
    var numConns = 0.0
    var sumComplexity = 1.0
    for (c <- allConns) {
      numConns += c.size
      sumComplexity += c.calculateComplexity
    }
    math.log(math.log(numConns)+bias+sumComplexity)
  }

}