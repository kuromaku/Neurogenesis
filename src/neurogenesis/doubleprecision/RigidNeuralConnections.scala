package neurogenesis.doubleprecision

import scala.collection.Iterator
import scalala.library.random.MersenneTwisterFast
import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.xml.Elem
import scala.xml.NodeSeq
import neurogenesis.util.Distribution
import neurogenesis.util.XMLOperator
import scala.collection.immutable.TreeMap
import scala.math.Numeric._

/*A class that implements neural connections in a way that enables variable structural rigidity
 * for connections.
 */
class RigidNeuralConnections(min:Int,max:Int,maxVal:Double)(implicit numerics:Numeric[Byte]) extends NeuralConnections[Byte](min,max,maxVal)(numerics) {
  minNode = min
  maxNode = max
  conns = new TreeMap[Int,(Double,Byte)]()(Ordering.Int) //:Map[Int,(Double,Int)]
  val ByteOps = implicitly[Numeric[Byte]]
  import ByteOps._
  

  def getConns2: Iterator[(Int,Double)] = { 
    var m = Map[Int,Double]()
    for ((d,(w,r)) <- conns) {
      m = m + ((d,w))
    }
    m.toIterator
  }
  /*
  def getFullIterator : Iterator[(Int,(Double,Int))] = {
    conns.iterator
  }
  * 
  */
  override def addConnection(dest:Int,weight:Double) : Boolean = {
	  if (conns.contains(dest)) {
	    false
	  }
	  else {
	    if (scala.math.abs(weight) < maxVal) {
	      conns = conns + ((dest,(weight,0.toByte)))
	    }
	    else {
	      if (weight < 0) {
	        conns = conns + ((dest,(-maxVal,0.toByte)))
	      }
	      else {
	        conns = conns + ((dest,(maxVal,0.toByte)))
	      }
	    }
	    true
	  }
  }
  override def addConnection(dest: Int, weight: Double, rigidity: Byte): Boolean = {
    if (conns.contains(dest)) {
      false
    }
    else {
      if (scala.math.abs(weight) < maxVal) {
        conns = conns + ((dest,(weight,rigidity)))
      }
      else {
        if (weight < 0) {
          conns = conns + ((dest,(-maxVal,rigidity)))
        }
        else {
          conns = conns + ((dest,(maxVal,rigidity)))
        }
      }
      true
    }
  }  
  //override
  def addConnection[B <: Numeric[B]](dest: Int, weight: Double, rigidity: B): Boolean = {
    if (conns.contains(dest)) {
      false
    }
    else {
      val r = ByteOps.fromInt(rigidity.toInt(rigidity)) //TODO: there has to be a saner way to do this
      if (scala.math.abs(weight) < maxVal) {
        
        conns = conns + ((dest,(weight,r)))
      }
      else {
        if (weight < 0) {
          conns = conns + ((dest,(-maxVal,r)))
        }
        else {
          conns = conns + ((dest,(maxVal,r)))
        }
      }
      true
    }
  }
  /*
  def addConnection[T](dest: Int, weight: Double, rigidity: T): Boolean = {
    if (conns.contains(dest)) {
      false
    }
    else {
      if (scala.math.abs(weight) < maxVal) {
        conns = conns + ((dest,(weight,rigidity)))
      }
      else {
        if (weight < 0) {
          conns = conns + ((dest,(-maxVal,rigidity)))
        }
        else {
          conns = conns + ((dest,(maxVal,rigidity)))
        }
      }
      true
    }
  }
  
  def addRandomConnection(rnd: MersenneTwisterFast): Boolean = { 
	val dest = rnd.nextInt(maxNode-minNode)+minNode
	val weight = scala.math.random-0.5
	return addConnection(dest,weight)    
  }

  def addRandomConnections(num: Int, rnd: MersenneTwisterFast): Int = { 
    var sum = 0
	for (i <- 0.to(num)) {
      if (addRandomConnection(rnd)) {
        sum += 1
      }
    }
  sum    
  }
  * 
  */
  /*Mutations will tend to be smaller the higher the rigidity is
   * 
   */
  def addMutatedConnection(d: Int, w: Double, xp:Int, p: Double, dist: Distribution, rnd: MersenneTwisterFast): Boolean = { 
    addConnection(d,w+dist.inverse(scala.math.random)/(1+xp))  
  }
  def addMutatedConnection(d: Int, w: Double, xp:Boolean, p: Double, dist: Distribution, rnd: MersenneTwisterFast): Boolean = { 
    addConnection(d,w+dist.inverse(scala.math.random))  
  }  

  def equals(other: RigidNeuralConnections): Boolean = { isEqualTo(other) }
  
  def isEqualTo(other:RigidNeuralConnections) : Boolean = {
    if (conns.size != other.size) {
      false
    }
    else {
      val iter = other.getFullIterator
      while (iter.hasNext) {
        val (d,(w,r)) = iter.next
        if (conns.contains(d)) {
          val (w2,r2) = conns.apply(d)
          if (w2 != w) {
            false
          }
        }
        else {
          false
        }
      }
      true
    }
  }

  override def burstMutate(prob: Double, dist: Distribution, rnd: MersenneTwisterFast): RigidNeuralConnections = { 
    val n = new RigidNeuralConnections(minNode,maxNode,maxVal)
    for ((d,(w,r)) <- conns) {
      val factor = 1+r.toInt
      if (rnd.nextDouble < prob/factor) {
        val mut = dist.inverse(rnd.nextDouble)
        n.addConnection(d,w+mut,r)
      }
      else {
        n.addConnection(d,w,r)
      }
    }
	var modProb = prob
    while (rnd.nextDouble < modProb) {
      if (rnd.nextDouble < 0.5) {
        n.addRandomConnection(rnd)
      }
      else {
        n.removeRandomConnection(rnd)
      }
      modProb *= 0.75 //this should ensure that no infinite loop ever results
    }    
    n
  }

  override def combine(nc2: NeuralConnections[Byte], dist: Distribution, mutP: Double, flipP: Double, rnd: MersenneTwisterFast, discardRate: Double): RigidNeuralConnections = {
    val nc = new RigidNeuralConnections(minNode,maxNode,maxVal)
	  val iter1 = conns.iterator
	  val iter2 = nc2.getFullIterator
	  var node = (0,(0.0,0.toByte))
	  var node2ready = false
	  var node1ready = false
	  while (iter1.hasNext) {
	    if (node1ready) {
	      val (d,(w,r)) = node
	      var lower = true
	      var done = false
	      while (lower && iter2.hasNext) {
	        val (d2,(w2,r2)) = iter2.next
	        if (d2 < d) {
	          if (rnd.nextDouble > discardRate) {
	            if (rnd.nextDouble < mutP/(1+r2)) {
	              nc.addMutatedConnection(d2,w2,r2,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d2,w2,r2)
	            }
	          }
	        }
	        else if (d2 == d) {
	          val (dx,(wx,rx)) = if (rnd.nextDouble < 0.5) (d,(w,r)) else (d2,(w2,r2))
	          if (rnd.nextDouble < mutP/(1+rx.toInt)) {
	            nc.addMutatedConnection(dx,wx,rx,flipP,dist,rnd)
	          }
	          else {
	            nc.addConnection(dx,wx,rx)
	          }
	          lower = false
	          node1ready = false
	          done = true
	        }
	        else {
	          lower = false
	          if (rnd.nextDouble > discardRate) {
	            if (math.random < mutP/(1+r.toInt)) {
	              nc.addMutatedConnection(d,w,r,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d,w,r)

	              //
	            }
	          }
	          node = (d2,(w2,r2))
	          node2ready = true
	          node1ready = false
	          done = true
	        }
	        

	      }
	      if (!done && !iter2.hasNext) {
	        nc.addMutatedConnection(d,w,r,flipP,dist,rnd)
			while (iter1.hasNext) {
			val (d3,(w3,r3)) = iter1.next
			if (rnd.nextDouble > discardRate) {
			  nc.addMutatedConnection(d3,w3,r3,flipP,dist,rnd)
			}
		  }
	      node1ready = false
	      }
	    }
	    else if (node2ready) {
	      val (d,(w,r)) = node
	      var lower = true
	      var done = false
	      while (lower && iter1.hasNext) {
	        val (d2,(w2,r2)) = iter1.next
	        if (d2 < d) {
	          if (rnd.nextDouble > discardRate) {
	            if (scala.math.random < mutP) {
	              nc.addMutatedConnection(d2,w2,r2,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d2,w2,r2)
	            }
	          }
	        }
	        else if (d2 == d) {
	          val (dx,(wx,rx)) = if (rnd.nextDouble < 0.5) (d,(w,r)) else (d2,(w2,r2))
	          if (rnd.nextDouble < mutP/(1+rx)) {
	            nc.addMutatedConnection(dx,wx,rx,flipP,dist,rnd)
	          }
	          else {
	            nc.addConnection(dx,wx,rx)
	          }
	          lower = false
	          node2ready = false
	          done = true
	        }
	        else {
	          lower = false
	          if (rnd.nextDouble > discardRate) {
	            if (math.random < mutP/(1+r)) {
	              nc.addMutatedConnection(d,w,r,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d,w,r)
	            }
	          }
	          node2ready = false
	          node1ready = true
	          node = (d2,(w2,r2))
	          done = true
	        }
	      }
	      if (!done && !iter1.hasNext) {
		    nc.addMutatedConnection(d,w,r,flipP,dist,rnd)
		    while (iter2.hasNext) {
		      val (d3,(w3,r3)) = iter2.next
			  if (rnd.nextDouble > discardRate) {
			    nc.addMutatedConnection(d3,w3,r3,flipP,dist,rnd)
			  }
		    }
		    node2ready = false
	      }
	    }
	    else {
	      node = iter1.next
	      node1ready = true
	    }
	  }
	  if (node2ready || node1ready) {
		if (rnd.nextDouble > discardRate) {
		  if (math.random < mutP/(1+node._2._2)) {
	        nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist,rnd)
		  }
		  else {
		    nc.addConnection(node._1,node._2._1,node._2._2)
		  }
	    }
			
	  }
	  while (iter2.hasNext) {
	    node = iter2.next
	    if (math.random > discardRate) {
	      nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist,rnd)
	    }
	  }
	  //Let's add or remove a random connection every once in a while
	  var p = mutP
	  while (rnd.nextDouble < p) {
	    if (math.random < 0.50) {
	      nc.addRandomConnection(rnd)
	    }
	    else {
	      nc.removeRandomConnection(rnd)
	    }
	    p *= 0.9 //additional connections or deletions become ever more improbable
	  }
	  nc    
  
  }
  override def combine(nc2: AbstractNeuralconnections, dist: Distribution, mutP: Double, flipP: Double, rnd: MersenneTwisterFast, discardRate: Double): RigidNeuralConnections = {
    val nc = new RigidNeuralConnections(minNode,maxNode,maxVal)
	  val iter1 = conns.iterator
	  val iter2 = nc2.getIterator
	  var node = (0,(0.0,0.toByte))
	  var node2ready = false
	  var node1ready = false
	  while (iter1.hasNext) {
	    if (node1ready) {
	      val (d,(w,r)) = node
	      var lower = true
	      var done = false
	      while (lower && iter2.hasNext) {
	        val (d2,w2) = iter2.next
	        if (d2 < d) {
	          if (rnd.nextDouble > discardRate) {
	            if (rnd.nextDouble < mutP) {
	              nc.addMutatedConnection(d2,w2,0,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d2,w2,ByteOps.fromInt(0))
	            }
	          }
	        }
	        else if (d2 == d) {
	          val (dx,(wx,rx)) = if (rnd.nextDouble < 0.5) (d,(w,r)) else (d2,(w2,r))
	          if (rnd.nextDouble < mutP/(1+rx)) {
	            nc.addMutatedConnection(dx,wx,rx,flipP,dist,rnd)
	          }
	          else {
	            nc.addConnection(dx,wx,rx)
	          }
	          lower = false
	          node1ready = false
	          done = true
	        }
	        else {
	          lower = false
	          if (rnd.nextDouble > discardRate) {
	            if (math.random < mutP/(1+r)) {
	              nc.addMutatedConnection(d,w,r,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d,w,r)

	              //
	            }
	          }
	          node = (d2,(w2,0.toByte))
	          node2ready = true
	          node1ready = false
	          done = true
	        }
	        

	      }
	      if (!done && !iter2.hasNext) {
	        nc.addMutatedConnection(d,w,r,flipP,dist,rnd)
			while (iter1.hasNext) {
			val (d3,(w3,r3)) = iter1.next
			if (rnd.nextDouble > discardRate) {
			  nc.addMutatedConnection(d3,w3,r3,flipP,dist,rnd)
			}
		  }
	      node1ready = false
	      }
	    }
	    else if (node2ready) {
	      val (d,(w,r)) = node
	      var lower = true
	      var done = false
	      while (lower && iter1.hasNext) {
	        val (d2,(w2,r2)) = iter1.next
	        if (d2 < d) {
	          if (rnd.nextDouble > discardRate) {
	            if (scala.math.random < mutP) {
	              nc.addMutatedConnection(d2,w2,r2,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d2,w2,r2)
	            }
	          }
	        }
	        else if (d2 == d) {
	          val (dx,(wx,rx)) = if (rnd.nextDouble < 0.5) (d,(w,r)) else (d2,(w2,r2))
	          if (rnd.nextDouble < mutP/(1+rx)) {
	            nc.addMutatedConnection(dx,wx,rx,flipP,dist,rnd)
	          }
	          else {
	            nc.addConnection(dx,wx,rx)
	          }
	          lower = false
	          node2ready = false
	          done = true
	        }
	        else {
	          lower = false
	          if (rnd.nextDouble > discardRate) {
	            if (math.random < mutP/(1+r)) {
	              nc.addMutatedConnection(d,w,r,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d,w,r)
	            }
	          }
	          node2ready = false
	          node1ready = true
	          node = (d2,(w2,r2))
	          done = true
	        }
	      }
	      if (!done && !iter1.hasNext) {
		    nc.addMutatedConnection(d,w,r,flipP,dist,rnd)
		    while (iter2.hasNext) {
		      val (d3,w3) = iter2.next
			  if (rnd.nextDouble > discardRate) {
			    nc.addMutatedConnection(d3,w3,0,flipP,dist,rnd)
			  }
		    }
		    node2ready = false
	      }
	    }
	    else {
	      node = iter1.next
	      node1ready = true
	    }
	  }
	  if (node2ready || node1ready) {
		if (rnd.nextDouble > discardRate) {
		  if (math.random < mutP/(1+node._2._2)) {
	        nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist,rnd)
		  }
		  else {
		    nc.addConnection(node._1,node._2._1,node._2._2)
		  }
	    }
			
	  }
	  while (iter2.hasNext) {
	    val (d4,w4) = iter2.next
	    node = (d4,(w4,0))
	    if (math.random > discardRate) {
	      nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist,rnd)
	    }
	  }
	  //Let's add or remove a random connection every once in a while
	  var p = mutP
	  while (rnd.nextDouble < p) {
	    if (math.random < 0.50) {
	      nc.addRandomConnection(rnd)
	    }
	    else {
	      nc.removeRandomConnection(rnd)
	    }
	    p *= 0.9 //additional connections or deletions become ever more improbable
	  }
	  nc    
  
  }
  override def complexify(in: Int, blocks: Int, memCells: Int, out: Int, addBlock: Boolean, rnd: MersenneTwisterFast): RigidNeuralConnections = { 
    complexifyWithTM(in,blocks,memCells,out,addBlock,rnd)
  }
  /*Complexifying RigidNeuralConnections also makes the old connections more rigid.
   *They will be changed less often afterwards.
   * 
   */
  override def complexifyWithTM(in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean,rnd:MersenneTwisterFast) : RigidNeuralConnections = {
    val gates = memCells+3
    val mid = in + blocks*gates  
	if (addBlock) {
      val nc2 = new RigidNeuralConnections(minNode,maxNode+gates,maxVal)
      for ((dest,(w,r)) <- conns) {
	    if (dest < mid) {
	      nc2.addConnection(dest,w,ByteOps.fromInt(r+1))
	    }
	    else {
	      nc2.addConnection(dest+gates,w,(r+1).toByte)
        }
      }
	  return nc2
	}
    else {
      val nc2 = new RigidNeuralConnections(minNode,maxNode+blocks,maxVal)
      for ((dest,(w,r)) <- conns) {
        val r2 = (r + 1)
        if (dest < in) {
	      nc2.addConnection(dest,w,r2.toByte)
        }
	    else if (dest < mid) {
          val aux = dest - in
          val gNum = aux % gates
          val bNum = aux / gates
          nc2.addConnection(dest+bNum,w,r2.toByte)
        }
        else {
          nc2.addConnection(dest+blocks,w,r2.toByte)
        }
      }
      return nc2
    }
	  
  }
  /*
  def removeConnection(dest:Int) : Unit = {
    conns = conns.-(dest)
  }
  def removeRandomConnection(rnd:MersenneTwisterFast) : Unit = {
    val dest = (rnd.nextDouble*(maxNode-minNode)).toInt+minNode
    removeConnection(dest)
  }

  def enforceMin(min2: Int): Unit = {
    minNode = min2
    conns = conns.filter(_._1 >= minNode)
  }

  def enforceMax(max2: Int): Unit = { 
    maxNode = max2
    conns = conns.filter(_._1 <= maxNode)
  }
  * 
  */
  override def toString : String = conns.toString
  override def toXML: Elem = {
    <RNNConns><Min>{minNode}</Min><Max>{maxNode}</Max><MaxVal>{maxVal}</MaxVal>{for (c <- conns) yield <cnn><dest>{c._1}</dest><w>{c._2._1}</w><rval>{c._2._2}</rval></cnn>}</RNNConns>
  }
  override def type2String : String = "Rigid"
}

object RigidNeuralConnections {
  def fromXML(elem:Elem,maxVal:Double) : RigidNeuralConnections = {
    val minDest = (elem \\ "Min").text.toInt
	val maxDest = (elem \\ "Max").text.toInt
	//val maxVal = (elem \\ "MaxVal").text.toDouble
	val nc = new RigidNeuralConnections(minDest,maxDest,maxVal)
	elem match {
	  case <RNNConns>{therms @ _*}</RNNConns> => {
	    for (therm @ <cnn>{_*}</cnn> <- therms) {
	      val d = (therm \ "dest").text.toInt
	      val w = (therm \ "w").text.toDouble
	      val b = (therm \ "rval").text.toByte
	      nc.addConnection(d,w,b)
	    }
      }
	  case _ => {
	    println("RigidNeuralConnections received wrong sort of an xml representation.\n")
	  }
    }
    nc
  }
  def fromXML(ns:NodeSeq,maxVal:Double) : RigidNeuralConnections = {
    val mv = (ns \\ "Min").text.toInt
    val mv2 = (ns \\ "Max").text.toInt
    //val maxV = (ns \\ "MaxVal").text.toDouble
    val nc = new RigidNeuralConnections(mv,mv2,maxVal)
    val cnns = XMLOperator.filterNodeSeq(ns)
    for (c <- cnns) {
      nc.addConnection((c \ "dest").text.toInt,(c \ "w").text.toDouble,(c \ "rval").text.toByte)
    }
    nc
  }
}