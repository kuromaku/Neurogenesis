package neurogenesis.doubleprecision

trait ComplexityMeasure {
  def calculateComplexity(allConns:List[NeuralConnsD],bias:Double) : Double
}