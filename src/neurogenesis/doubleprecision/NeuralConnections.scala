package neurogenesis.doubleprecision
import scalala.library.random.MersenneTwisterFast
import neurogenesis.util.Distribution
import neurogenesis.util.CComplexityMeasure
import scala.xml.Elem
import scala.collection.immutable.Map
import scala.collection.immutable.TreeMap
import scala.collection.immutable.Set
import scala.math._
import scala.math.Numeric

class NeuralConnections[T](min:Int,max:Int,maxVal:Double)(implicit numerics:Numeric[T]) extends AbstractNeuralconnections(maxVal) {
  //(implicit numerics: Numeric[T]) import numerics._
  //val numerics = implicitly[Numeric[T]]
  minNode = min
  maxNode = max
  var conns:Map[Int,(Double,T)] = new TreeMap[Int,(Double,T)]()(Ordering.Int)
  
  def addConnection(dest:Int,weight:Double) : Boolean = {
    if (conns.contains(dest)) {
      false
    }
    else {
      conns = conns.+((dest,(weight,numerics.zero)))
      true
    }
  }
  def addConnection(dest:Int,weight:Double,t:T) : Boolean = {
    if (conns.contains(dest)) {
      false
    }
    else {
      if (scala.math.abs(weight) < maxVal) {
        conns = conns.+((dest,(weight,t)))
      }
      else if (weight < -maxVal) {
        conns = conns.+((dest,(-maxVal,t)))
      }
      else {
        conns = conns.+((dest,(maxVal,t)))
      }
      true
    }
  }
  def addConnection[A:Numeric](dest:Int,weight:Double,t:A) : Boolean = {
    if (conns.contains(dest)) {
      false
    }
    else {
      val num2 = implicitly[Numeric[A]]
      conns = conns.+((dest,(weight,numerics.fromInt(num2.toInt(t))))) //figure out a better way to do this
      true
    }
  }
  def addRandomConnection(rnd:MersenneTwisterFast) : Boolean = {
    val d = min + rnd.nextInt(max-min)
    val w = if (rnd.nextBoolean) rnd.nextDouble*maxVal else -rnd.nextDouble*maxVal
    addConnection(d,w)
  }
  def addRandomConnections(num:Int,rnd:MersenneTwisterFast) : Int = {
    var sum = 0
    for (i <- 0 until num) {
      if (addRandomConnection(rnd)) { sum += 1 }
    }
    sum
  }
  def addMutatedConnection[A:Numeric](d:Int,w:Double,xp:A,p:Double,dist:Distribution,rnd:MersenneTwisterFast) : Boolean = {
    addConnection(d,w+dist.inverse(rnd.nextDouble),xp)
  }
  
  def burstMutate(prob:Double,dist:Distribution,rnd:MersenneTwisterFast) : NeuralConnections[T] = {
    val nc2 = new NeuralConnections[T](minNode,maxNode,maxVal)
    for ((d,(w,t)) <- conns) {
      if (rnd.nextDouble < prob) {
        nc2.addConnection(d,w+dist.inverse(rnd.nextDouble),t)
      }
      else {
        nc2.addConnection(d,w,t)
      }
    }
    nc2
  }
  override def calculateComplexity : Double = {
	  var c = 0.0d
	  for ((d,w) <- conns) {
	    c += scala.math.pow(d,2.0)
	  }
	  c
	}  
  def combine(nc2:NeuralConnections[T],dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double = 0.75) : NeuralConnections[T] = {
    new NeuralConnections[T](nc2.minNode,nc2.maxNode,maxVal)
  }
  def combine(nc2:AbstractNeuralconnections,dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double) : AbstractNeuralconnections = {
    new NeuralConnections[T](nc2.minNode,nc2.maxNode,maxVal)
  }
  def complexify(in: Int, blocks: Int, memCells: Int, out: Int, addBlock: Boolean, rnd: MersenneTwisterFast): NeuralConnections[T] = { 
    complexifyWithTM(in,blocks,memCells,out,addBlock,rnd)
  }
  /*Complexifying RigidNeuralconnections also makes the old connections more rigid.
   *They will be changed less often afterwards.
   * 
   */
  def complexifyWithTM(in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean,rnd:MersenneTwisterFast) : NeuralConnections[T] = {
    val gates = memCells+3
    val mid = in + blocks*gates  
	if (addBlock) {
      val nc2 = new NeuralConnections[T](minNode,maxNode+gates,maxVal)
      for ((dest,(w,r)) <- conns) {
	    if (dest < mid) {
	      nc2.addConnection(dest,w,r)
	    }
	    else {
	      nc2.addConnection(dest+gates,w,r)
        }
      }
	  return nc2
	}
    else {
      val nc2 = new NeuralConnections[T](minNode,maxNode+blocks,maxVal)
      for ((dest,(w,r)) <- conns) {
        val r2 = r
        if (dest < in) {
	      nc2.addConnection(dest,w,r2)
        }
	    else if (dest < mid) {
          val aux = dest - in
          val gNum = aux % gates
          val bNum = aux / gates
          nc2.addConnection(dest+bNum,w,r2)
        }
        else {
          nc2.addConnection(dest+blocks,w,r2)
        }
      }
      return nc2
    }
	  
  }
  def getConns : Map[Int,Double] = {
    var m2 = Map[Int,Double]()
    for ((d,(w,t)) <- conns) {
      m2 = m2.+((d,w))
    }
    m2
  }
  def getIterator : Iterator[(Int,Double)] = {
    getConns.iterator
  }
  def getFullIterator : Iterator[(Int,(Double,T))] = conns.toIterator
  def createConnections[A:Numeric](min:Int,max:Int,conns2:Map[Int,(Double,A)]) : NeuralConnections[A] = {
    val c2 = new NeuralConnections[A](min,max,maxVal)
    c2.conns = conns2
    c2
  }
  //override
  def dist(nc2: NeuralConnections[T]): Double = { 
	var d = 0.0
	val c2 = nc2.conns
	for ((dst,(w2,b)) <- conns) {
	  if (c2.contains(dst)) {
	    d += scala.math.abs(w2-c2.apply(dst)._1)
	  }
	  else {
	    d += scala.math.abs(w2)
	  }
	}
	for ((dst,(w2,t2)) <- c2) {
	  if (!conns.contains(dst)) {
	    d += scala.math.abs(w2)
	  }
	}
	d    
  }  
  def enforceMin(min2:Int) : Unit = {
    conns = conns.filter(_._1 >= 2)    
  }
  def enforceMax(max2:Int) : Unit = {
    conns = conns.filter(_._1 <= max2)
  }
  def makeClone : NeuralConnections[T] = {
    val nc2 = new NeuralConnections[T](minNode,maxNode,maxVal)
    for ((d,(w,r)) <- conns) {
      nc2.addConnection(d,w,r)
    }
    nc2
  }
  def removeConnection(dest:Int) : Unit = {
    conns = conns.-(dest)
  }
  def removeRandomConnection(rnd:MersenneTwisterFast) : Unit = {
    val dest = rnd.nextInt(maxNode-minNode)+minNode
    removeConnection(dest)
  }
  override def size : Int = conns.size
  def toXML(): Elem = {
    <NConns><Min>{minNode}</Min><Max>{maxNode}</Max><MaxVal>{maxVal}</MaxVal>{for (c <- conns) yield <cnn><dest>{c._1}</dest><w>{c._2._1}</w><rval>{c._2._2}</rval></cnn>}</NConns>
  }
  override def type2String : String = "NeuralT"
  /*
	var Node:Int
	var maxNode:Int
	//
	//def getMap : Map[Int,(Double,Boolean)]
	def getConns : Map[Int,Double]
	def getIter : Iterator[(Int,Double)]
	
    def getMin : Int = Node
	def getMax : Int = maxNode
	

    def dist(nc2:NeuralConnections[T]) : Double
    def equals(other:NeuralConnections[T]) : Boolean
	def getDestinations : Set[Int]
	def makeClone : NeuralConnections[T]
    def set(conns2:Map[Int,(Double,T)]) : Unit
    def createConnections(min:Int,max:Int,conns2:Map[Int,(Double,T)]) : NeuralConnections[T]
    def burstMutate(prob:Double,dist:Distribution,rnd:MersenneTwisterFast) : NeuralConnections[T]
    def combine(nc2:NeuralConnections[T],dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double = 0.75) : NeuralConnections[T]
    def complexify(in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean,rnd:MersenneTwisterFast) : NeuralConnections[T]


	def toXML : Elem
	*/
}
/*
object NeuralConnections {
  def fromXML[T](elem:Elem) : NeuralConnections[T] = {
    val ctype = (elem \\ "ctype").head.text
    ctype match {
      case "Rigid" => {
        RigidNeuralConns.fromXML(elem)
      }
      case _ => NeuralConns.fromXML(elem)
    }
  }
}
*/