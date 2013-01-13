package neurogenesis.doubleprecision

import scala.collection.immutable.List

class ConstantMeasure extends ComplexityMeasure {

  def calculateComplexity(allConns: List[NeuralConnsD], bias: Double): Double = { 
    bias
  }
  override def toString : String = "ConstantMeasure"

}