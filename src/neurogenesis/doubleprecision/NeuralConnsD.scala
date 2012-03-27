package neurogenesis.doubleprecision

//import scala.collection.mutable.ListMap
import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSet
import scala.math.Ordering._
import scala.util.Random
import scala.collection.Map
import scala.collection.Set
import scala.xml.Elem

class NeuralConnsD(min:Int,max:Int) {
  //type T = Double
	var minNode = min
	var maxNode = max
	var conns:Map[Int,(Double,Boolean)] = new TreeMap[Int,(Double,Boolean)]()(Ordering.Int)
	def getConns : Map[Int,Double] = {
	  var tmap = Map[Int,Double]() //(conns
	  for (c <- conns) {
	    if (c._2._2) {
	      tmap = tmap + ((c._1,c._2._1))
	    }
	  }
	  tmap
	}
	def getConns2 : Iterator[(Int,(Double,Boolean))] = conns.toIterator
	def setConns(c:Map[Int,(Double,Boolean)]) : Unit = { conns = c }
	def getMin : Int = minNode
	def getMax : Int = maxNode
	def addConnection(dest:Int,weight:Double) : Boolean = {
	  if (conns.contains(dest)) {
	    false
	  }
	  else {
	    conns = conns + ((dest,(weight,true)))
	    true
	  }
	}
	def addConnection(dest:Int,weight:Double,expr:Boolean) : Boolean = {
	  if (conns.contains(dest)) {
	    false
	  }
	  conns = conns + ((dest,(weight,expr)))
	  true
	}
	def addRandomConnection(rnd:Random) : Boolean = {
		val dest = rnd.nextInt(maxNode-minNode)+minNode
		val weight = Math.random-0.5
		return addConnection(dest,weight)
	}
	def addRandomConnections(num:Int,rnd:Random) : Int = {
	  var sum = 0
	  for (i <- 0.to(num)) {
	    if (addRandomConnection(rnd)) {
	      sum += 1
	    }
	  }
	  sum
	}
	def addMutatedConnection(d:Int,w:Double,expr:Boolean,flipP:Double,dist:Distribution) : Boolean = {
		if (Math.random < flipP) {
		  addConnection(d,w+dist.inverse(Math.random),!expr)
		}
		else {
		  addConnection(d,w+dist.inverse(Math.random),expr)
		}
	}
	def addMutatedConnection(d:Int,w:Double,expr:Boolean,flipP:Double,dist:Distribution,rnd:Random) : Boolean = {
		if (Math.random < flipP) {
		  addConnection(d,w+dist.inverse(rnd.nextDouble),!expr)
		}
		else {
		  addConnection(d,w+dist.inverse(rnd.nextDouble),expr)
		}
	}
	def set(conns2:TreeMap[Int,(Double,Boolean)]) : Unit = {
		conns = conns2
	}
	def NeuralConnsD(m0:Int,m1:Int,conns2:TreeMap[Int,(Double,Boolean)]) : NeuralConnsD = {
		var nc = new NeuralConnsD(m0,m1)
		nc.set(conns2)
		return nc
	}
	def burstMutate(prob:Double,dist:Distribution,rnd:Random) : NeuralConnsD = {
	  val arr = conns.toArray
	  val n = new NeuralConnsD(minNode,maxNode)
	  for (i <- 0 until arr.length) {
	    if (rnd.nextDouble < prob) {
	      val mut = dist.inverse(rnd.nextDouble)
	      n.addConnection(arr(i)._1,arr(i)._2._1+mut,arr(i)._2._2)
	    }
	    else {
	      n.addConnection(arr(i)._1,arr(i)._2._1,arr(i)._2._2)
	    }
	    
	  }
	  while (rnd.nextDouble < prob) {
	    if (rnd.nextDouble < 0.5) {
	      n.addRandomConnection(rnd)
	    }
	    else {
	      n.removeRandomConnection(rnd)
	    }
	  }
	  n
	}
	def burstMutate2(prob:Double,flipP:Double,dist:Distribution) : NeuralConnsD = {
	  var nc = new NeuralConnsD(minNode,maxNode)
	  for (conn <- conns) {
	    if (Math.random < prob)
	      nc.addMutatedConnection(conn._1,conn._2._1,conn._2._2,flipP,dist)
	    else {
	      if (Math.random < flipP) {
	        nc.addConnection(conn._1,conn._2._1,!conn._2._2)
	      }
	      else {
	        nc.addConnection(conn._1,conn._2._1,conn._2._2)
	      }
	    }
	      
	  }
	  nc
	}
	def burstMutate2b(dist:Distribution) : Unit = {
	  var c2 = new TreeMap[Int,(Double,Boolean)]()(Ordering.Int)
	  for (conn <- conns) {
	    c2 = c2 + ((conn._1,(conn._2._1+dist.inverse(Math.random),true)))
	  }
	  conns = c2
	}
	/*
	def combine(nc2:NeuralConnsD,dist:Random) : NeuralConnsD = {
	  val c2 = nc2.getConns
	  val cnn2 = new NeuralConnsD(min,max)
	  
	  cnn2
	}
	
	def combine(nconn2:NeuralConns) : NeuralConns = {
		val size1 = conns.size
		val conns2 = nconn2.getConns
		val size2 = conns2.size
		var n = new NeuralConns(nconn2.getMin,nconn2.getMax)
		var k:Int = 0
		val hd0 = conns.head
		val utl = conns2.until(hd0._1)
		for (conn <- utl) {
			n.addConnection(conn._1,conn._2)
		}
		for (conn <- conns) {
		    k = conn._1
			val utl2 = conns2.from(k)
			if (utl2.size > 0) {
				val hd = utl2.head
				if (hd._1 == k) {
					if (Math.random < 0.5) {
						n.addConnection(conn._1,conn._2)
					}
					else {
						n.addConnection(hd._1,hd._2)
					}
				}
				else {
					n.addConnection(conn._1,conn._2)
				}
			}
			else {
				n.addConnection(conn._1,conn._2)
			}
		    val aux = conns.from(k+1)
		    val aux2 = utl2.from(k+1)
		    if (aux != Nil && aux.head != Nil) {
		    	val nk = aux.head._1
		    
		    	for (c <- aux2) {
		    		if (c._1 < nk) {
		    			n.addConnection(c._1,c._2)
		    		}
		    	}
		    }
		}
		val rest = conns2.from(k+1)
		for (conn <- rest) {
			n.addConnection(conn._1,conn._2)
		}
		n
	}
	
	def combine2(nconn2:NeuralConnsD,dist:Distribution) : NeuralConnsD = {
		var n = new NeuralConnsD(nconn2.getMin,nconn2.getMax)
		val hd0 = conns.head
		val utl = nconn2.getConns.until(hd0._1)
		var k = 0
		for (conn <- utl) {
			k = conn._1
			n.addMutatedConnection(k,conn._2._1,dist)
		}
		val rest = nconn2.getConns.from(k)
		for (conn <- conns) {
			k = conn._1
			if (rest.contains(k)) {
				if (Math.random < 0.5) {
					n.addMutatedConnection(k,conn._2._1,dist)
				}
				else {
					n.addMutatedConnection(k,rest.apply(k),dist)
				}
			}
			else {
				n.addMutatedConnection(k,conn._2,dist)
			}
		}
		val rest2 = rest.from(k)
		for (conn <- rest2) {
			n.addMutatedConnection(conn._1,conn._2,dist)
		}
		n
	}
	*/
	def combine(nc2:NeuralConnsD,dist:Distribution,mutP:Double,flipP:Double) : NeuralConnsD = {
		var nc = new NeuralConnsD(minNode,maxNode)
		val iter1 = conns.iterator
		val iter2 = nc2.getConns2
		var node = (0,(0.0,true))
		var node2ready = false
		var node1ready = false
		var c = (0,(0.0,true))
		while (iter1.hasNext) {
		  if (node1ready) {
		    c = node
		  }
		  else {
		    c = iter1.next
		  }
		  val k = c._1
		  if (node2ready) {
			  if (k < node._1) {
			    nc.addMutatedConnection(k,c._2._1,c._2._2,flipP,dist)
			  }
			  else if (k == node._2._1) {
			    if (Math.random < 0.5) {
			      nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
			    }
			    else {
			      nc.addMutatedConnection(k,node._2._1,node._2._2,flipP,dist)
			    }
			    node2ready = false
			  }
			  else {
			    nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist)
			    node2ready = false
			    node1ready = true
			    node = c
			  }
			  
		  }
		  else {
				var lower = true
				var done = false
				while (lower && iter2.hasNext) {
					val c2 = iter2.next
					if (c2._1 < k) {
						nc.addMutatedConnection(c2._1,c2._2._1,c2._2._2,flipP,dist)
					}
					else if (c2._1 == k) {
						if (Math.random < 0.5) {
							nc.addMutatedConnection(k,c._2._1,c._2._2,flipP,dist)
						}
						else {
							nc.addMutatedConnection(k,c2._2._1,c2._2._2,flipP,dist)
						}
						lower = false
						node1ready = false
						done = true
					}
					else {
						lower = false
						nc.addMutatedConnection(k,c._2._1,c._2._2,flipP,dist)
						node = c2
						node2ready = true
						node1ready = false
						done = true
					}
					
				}
				if (!done && !iter2.hasNext) {
				  nc.addMutatedConnection(k,c._2._1,c._2._2,flipP,dist)
				  while (iter1.hasNext && !node2ready) {
				    c = iter1.next
				    nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
				  }
				}
				else if (node1ready) {
				  nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
				  node1ready = false
				}
				
			}
		}
		if (node2ready || node1ready) {
		  nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist)
			
		}
		while (iter2.hasNext) {
				node = iter2.next
				nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist)
		}
		nc
	}
    def combine(nc2:NeuralConnsD,dist:Distribution,mutP:Double,flipP:Double,rnd:Random) : NeuralConnsD = {
		var nc = new NeuralConnsD(minNode,maxNode)
		val iter1 = conns.iterator
		val iter2 = nc2.getConns2
		var node = (0,(0.0,true))
		var node2ready = false
		var node1ready = false
		var c = (0,(0.0,true))
		while (iter1.hasNext) {
		  if (node1ready) {
		    c = node
		  }
		  else {
		    c = iter1.next
		  }
		  val k = c._1
		  if (node2ready) {
			  if (k < node._1) {
			    nc.addMutatedConnection(k,c._2._1,c._2._2,flipP,dist,rnd)
			  }
			  else if (k == node._2._1) {
			    if (Math.random < 0.5) {
			      nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist,rnd)
			    }
			    else {
			      nc.addMutatedConnection(k,node._2._1,node._2._2,flipP,dist,rnd)
			    }
			    node2ready = false
			  }
			  else {
			    nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist,rnd)
			    node2ready = false
			    node1ready = true
			    node = c
			  }
			  
		  }
		  else {
				var lower = true
				var done = false
				while (lower && iter2.hasNext) {
					val c2 = iter2.next
					if (c2._1 < k) {
						nc.addMutatedConnection(c2._1,c2._2._1,c2._2._2,flipP,dist,rnd)
					}
					else if (c2._1 == k) {
						if (Math.random < 0.5) {
							nc.addMutatedConnection(k,c._2._1,c._2._2,flipP,dist,rnd)
						}
						else {
							nc.addMutatedConnection(k,c2._2._1,c2._2._2,flipP,dist,rnd)
						}
						lower = false
						node1ready = false
						done = true
					}
					else {
						lower = false
						nc.addMutatedConnection(k,c._2._1,c._2._2,flipP,dist,rnd)
						node = c2
						node2ready = true
						node1ready = false
						done = true
					}
					
				}
				if (!done && !iter2.hasNext) {
				  nc.addMutatedConnection(k,c._2._1,c._2._2,flipP,dist)
				  while (iter1.hasNext && !node2ready) {
				    c = iter1.next
				    nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
				  }
				}
				else if (node1ready) {
				  nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
				  node1ready = false
				}
				
			}
		}
		if (node2ready || node1ready) {
		  nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist)
			
		}
		while (iter2.hasNext) {
				node = iter2.next
				nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist)
		}
		//Let's add a random connection every once in a while
		while (rnd.nextDouble < mutP) {
		  if (rnd.nextDouble < 0.51) {
		    nc.addRandomConnection(rnd)
		  }
		  else {
		    nc.removeRandomConnection(rnd)
		  }
		}
		nc
	}
	def complexify(in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean,rnd:Random) : NeuralConnsD = {
	  complexifyWithTM(in,blocks,memCells,out,addBlock,rnd)
	}
	def complexifyWithTM(in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean,rnd:Random) : NeuralConnsD = {
	  val gates = memCells+3
	  val mid = in + blocks*gates
	  
	  if (addBlock) {
	    val nc2 = new NeuralConnsD(minNode,maxNode+gates)
	    for ((dest,(w,b)) <- conns) {
	      if (dest < mid) {
	        nc2.addConnection(dest,w,b)
	      }
	      else {
	        nc2.addConnection(dest+gates,w,b)
	      }
	    }
	    return nc2
	  }
	  else {
	    val nc2 = new NeuralConnsD(minNode,maxNode+blocks)
	    for ((dest,(w,b)) <- conns) {
	      if (dest < in) {
	        nc2.addConnection(dest,w,b)
	      }
	      else if (dest < mid) {
	        val aux = dest - in
	        val gNum = aux % gates
	        val bNum = aux / gates
	        nc2.addConnection(dest+bNum,w,b)
	      }
	      else {
	        nc2.addConnection(dest+blocks,w,b)
	      }
	    }
	    return nc2
	  }
	  
	}
	def getDestinations : Set[Int] = {
	  conns.keySet
	}
	def calculateComplexity : Double = {
	  val size = conns.size
	  var sum = 0.0
	  for (c <- conns) {
	    sum += c._2._1*c._2._1
	  }
	  return size/sum
	}
	def dist(nc2:NeuralConnsD) : Double = {
	  var d = 0.0
	  val c2 = nc2.getConns
	  for ((dst,(w2,b)) <- conns) {
	    if (c2.contains(dst)) {
	      d += Math.abs(w2-c2.apply(dst))
	    }
	    else {
	      d += Math.abs(w2)
	    }
	  }
	  for ((dst,w2) <- c2) {
	    if (!conns.contains(dst)) {
	      d += Math.abs(w2)
	    }
	  }
	  d
	}
	def isEqualTo(nc2:NeuralConnsD) : Boolean = {
	  val iter = nc2.getConns2
	  if (nc2.getConns.size != conns.size) {
	    return false
	  }
	  while (iter.hasNext) {
	    val node = iter.next
	    val node2 = node._2
	    if (conns.contains(node._1)) {
	      val d = conns.apply(node._1)
	      if (d._1 != node2._1 || d._2 != node2._2) {
	        return false
	      }
	    }
	    else {
	      return false
	    }
	  }
	  return true
	}
	def makeClone : NeuralConnsD = {
	  val nc2 = new NeuralConnsD(minNode,maxNode)
	  for (c <- conns) {
	    nc2.addConnection(c._1,c._2._1,c._2._2)
	  }
	  nc2
	}
	def removeConnection(dest:Int) : Unit = {
	  conns = conns.-(dest)
	}
	def removeRandomConnection(rnd:Random) : Unit = {
	  val dest = (rnd.nextDouble*(maxNode-minNode)).toInt+minNode
	  removeConnection(dest)
	}
	def setMax(m:Int) : Unit = { maxNode = m}
	def setMin(m:Int) : Unit = { minNode = m}
	def connsToString : String = getConns.toString()
	def toString2() : String = "<NeuralConnsD>\n"+conns.toString+"\n</NeuralConnsD>"
	override def toString : String = conns.toString
	def toXML : Elem = {
	  <NeuralConnsD><Min>{minNode}</Min><Max>{maxNode}</Max>{for (c <- conns) yield <cnn><dest>{c._1}</dest><w>{c._2._1}</w><expr>{c._2._2}</expr></cnn>}</NeuralConnsD>
	  //new Elem("NeuralConnsD",conns.toString,null,null)
	}
	/*Sometimes the IDE seems to have trouble with this code
	 * 
	 */
	def fromXML(elem:Elem) : NeuralConnsD = {
	  val minVal = (elem \\ "Min").text.toInt
	  val maxVal = (elem \\ "Max").text.toInt
	  val nc = new NeuralConnsD(minVal,maxVal)
	  elem match {
	    case <NeuralConnsD>{therms @ _*}</NeuralConnsD> => {
	      for (therm @ <cnn>{_*}</cnn> <- therms) {
	        val d = (therm \ "dest").text.toInt
	        val w = (therm \ "w").text.toDouble
	        val b = (therm \ "expr").text.toBoolean
	        nc.addConnection(d,w,b)
	      }
	    }
	  }
	  nc
	}
}