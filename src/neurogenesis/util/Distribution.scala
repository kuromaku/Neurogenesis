package neurogenesis.util

abstract class Distribution {
	def probability(x:Double) : Double
	def inverse(x:Double) : Double
	def adjust(factor:Double) : Unit
}