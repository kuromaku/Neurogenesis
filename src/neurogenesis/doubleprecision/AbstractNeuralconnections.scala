package neurogenesis.doubleprecision
import scalala.library.random.MersenneTwisterFast
import neurogenesis.util.Distribution
import neurogenesis.util.CComplexityMeasure
import scala.xml.Elem
import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.math.Numeric
abstract class AbstractNeuralconnections(maxVal:Double) {
	var minNode:Int = 0
	var maxNode:Int = 0
	//var conns:Map[Int,(Double,Boolean)]
	//def getMap : Map[Int,(Double,Boolean)]
	def getConns : Map[Int,Double]
	def getIterator : Iterator[(Int,Double)]
	//def getFullIterator[T] : Iterator[(Int,(Double,T))]
    def getMin : Int = minNode
	def getMax : Int = maxNode
	
	def addConnection(dest:Int,weight:Double) : Boolean
    /*
    implicit def anyval2Byte(anyval:AnyVal) : Byte = {
	  //val c = anyval.getClass
	  0.toByte
    }
	implicit def anyval2Boolean(anyval:AnyVal) : Boolean = true
	*/
	def addConnection[T:Numeric](dest:Int,weight:Double,t:T) : Boolean
	def addRandomConnection(rnd:MersenneTwisterFast) : Boolean
	def addRandomConnections(num:Int,rnd:MersenneTwisterFast) : Int
	def addMutatedConnection[T:Numeric](d:Int,w:Double,xp:T,p:Double,dist:Distribution,rnd:MersenneTwisterFast) : Boolean
    //def addMutatedConnection(d:Int,w:Double,xp:Boolean,p:Double,dist:Distribution,rnd:MersenneTwisterFast) : Boolean
	def dist(nc2:AbstractNeuralconnections) : Double = {
	  var sum = 0.0d
	  val c = getConns
	  val c2 = nc2.getConns
	  for ((d,w) <- c) {
	    if (c2.contains(d)) {
	      sum += scala.math.pow(d-c2.apply(d),2.0)
	    }
	    else {
	      sum += scala.math.pow(d, 2)
	    }
	  }
	  for ((d,w) <- c2) {
	    if (!c.contains(d)) {
	      sum += scala.math.pow(d,2.0)
	    }
	  }
	  sum
	}
    def equals(other:AbstractNeuralconnections) : Boolean = {
	  val c2 = other.getConns
	  val c1 = getConns
	  if (c1.size != c2.size) {
	    false
	  }
	  else {
	    for ((d,w) <- c1) {
	      if (c2.contains(d)) {
	        if (w != c2.apply(d)) false
	      }
	      else {
	        false
	      }
	    }
	  }
	  true
	}
	def getDestinations : Set[Int] = getConns.keySet
	def makeClone : AbstractNeuralconnections
    //def set[T](conns2:Map[Int,(Double,T)]) : Unit
    def size : Int = getConns.size
    def calculateComplexity : Double = {
	  var c = 0.0d
	  for ((d,w) <- getConns) {
	    c += scala.math.pow(d,2.0)
	  }
	  c
	}
    def createConnections[T:Numeric](m0:Int,m1:Int,conns2:Map[Int,(Double,T)]) : AbstractNeuralconnections
    def burstMutate(prob:Double,dist:Distribution,rnd:MersenneTwisterFast) : AbstractNeuralconnections
    def combine(nc2:AbstractNeuralconnections,dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double) : AbstractNeuralconnections
    def complexify(in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean,rnd:MersenneTwisterFast) : AbstractNeuralconnections
    def removeConnection(dest:Int) : Unit
    def removeRandomConnection(rnd:MersenneTwisterFast) : Unit
    def setMax(m:Int) : Unit = { maxNode = m}
	def setMin(m:Int) : Unit = { minNode = m}
	def enforceMin(min2:Int) : Unit
	def enforceMax(max2:Int) : Unit
	def toXML : Elem
	def type2String: String = "Abstract"
}