package neurogenesis.util
import neurogenesis.doubleprecision.NeuralConnsD
/*Used in an alternative object oriented way to calculate the network complexities.
 *
 */
abstract class CComplexityMeasure {
  def calculateComplexity(c:NeuralConnsD) : Double
}