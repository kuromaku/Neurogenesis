package neurogenesis.doubleprecision

import scala.collection.immutable.List

class ConstantMeasure extends ComplexityMeasure {

  def calculateComplexity(allConns: List[AbstractNeuralconnections], bias: Double): Double = { 
    bias
  }
  override def toString : String = "ConstantMeasure"

}