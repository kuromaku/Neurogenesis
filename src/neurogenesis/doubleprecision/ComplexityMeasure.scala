package neurogenesis.doubleprecision

trait ComplexityMeasure {
  def calculateComplexity(allConns:List[AbstractNeuralconnections],bias:Double) : Double
}