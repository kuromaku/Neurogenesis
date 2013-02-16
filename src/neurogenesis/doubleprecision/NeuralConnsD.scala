package neurogenesis.doubleprecision

import scala.collection.immutable.Map
import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSet
import scala.collection.immutable.Set
import scala.math.Ordering._
import scala.math.Numeric
import scala.util.Random

import scala.xml.Elem
import scala.xml.NodeSeq
import neurogenesis.util.XMLOperator
import neurogenesis.util.Distribution
import neurogenesis.util.CComplexityMeasure
import scalala.library.random.MersenneTwisterFast

/*A class that represents the connections the artificial neurons can have
 * in a given network.
 */
class NeuralConnsD(min:Int,max:Int,maxVal:Double=3.0)(implicit tricks:Numeric[Byte]) extends NeuralConnections[Byte](min,max,maxVal)(tricks) {
    //val tricks = implicitly[Numeric[Byte]]
	minNode = min
	maxNode = max
	conns = new TreeMap[Int,(Double,Byte)]()(Ordering.Int)
	def getMap = conns
	
    def negate(b:Byte) : Byte = { if (b == 0) 1.toByte else 0.toByte }
	def getConns2 : Iterator[(Int,(Double,Byte))] = conns.toIterator
	def setConns(c:Map[Int,(Double,Byte)]) : Unit = { conns = c }
	
	
	override def addConnection(dest:Int,weight:Double) : Boolean = {
	  if (conns.contains(dest)) {
	    false
	  }
	  else {
	    if (scala.math.abs(weight) < maxVal) {
	      conns = conns + ((dest,(weight,1.toByte)))
	    }
	    else {
	      if (weight < 0) {
	        conns = conns + ((dest,(-maxVal,1.toByte)))
	      }
	      else {
	        conns = conns + ((dest,(maxVal,1.toByte)))
	      }
	    }
	    true
	  }
	}
	def addConnection[T<:Numeric[T]](dest:Int,weight:Double,t:T) : Boolean = {
	  addConnection(dest,weight,t)
	}
	def addMutatedConnection(d:Int,w:Double,expr:Byte,flipP:Double,dist:Distribution) : Boolean = {
		if (scala.math.random < flipP) {
		  addConnection(d,w+dist.inverse(scala.math.random),negate(expr))
		}
		else {
		  addConnection(d,w+dist.inverse(scala.math.random),expr)
		}
	}
	def addMutatedConnection(d:Int,w:Double,expr:Byte,flipP:Double,dist:Distribution,rnd:MersenneTwisterFast) : Boolean = {
		var e = expr
	    if (rnd.nextDouble < flipP) {
		  e = if (e == 1) 0.toByte else 1.toByte
		}
		if (rnd.nextDouble < 0.02) {
		  addConnection(d,rnd.nextDouble*2-1,e)
		}
		else {
		  addConnection(d,w+dist.inverse(rnd.nextDouble),e)
		}
	}
	def addMutatedConnection2(d:Int,w:Double,expr:Byte,flipP:Double,dist:Distribution,rnd:MersenneTwisterFast) : Boolean = {
		if (rnd.nextDouble < flipP) {
		  addConnection(d,w+dist.inverse(rnd.nextDouble),negate(expr))
		}
		else {
		  addConnection(d,w+dist.inverse(rnd.nextDouble),expr)
		}
	}
	def set[T](conns2:TreeMap[Int,(Double,Byte)]) : Unit = {
		setConns(conns2) //conns = conns2
	}
	def NeuralConnsD(m0:Int,m1:Int,conns2:TreeMap[Int,(Double,Byte)]) : NeuralConnsD = {
		var nc = new NeuralConnsD(m0,m1)
		nc.set(conns2)
		return nc
	}
	override def burstMutate(prob:Double,dist:Distribution,rnd:MersenneTwisterFast) : NeuralConnsD = {
	  val arr = conns.toArray
	  val n = new NeuralConnsD(minNode,maxNode,maxVal)
	  for (i <- 0 until arr.length) {
	    if (rnd.nextDouble < prob) {
	      val mut = dist.inverse(rnd.nextDouble)
	      n.addConnection(arr(i)._1,arr(i)._2._1+mut,arr(i)._2._2)
	    }
	    else {
	      n.addConnection(arr(i)._1,arr(i)._2._1,arr(i)._2._2)
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
	    modProb *= 0.75
	  }
	  n
	}
	def burstMutate2(prob:Double,flipP:Double,dist:Distribution) : NeuralConnsD = {
	  var nc = new NeuralConnsD(minNode,maxNode)
	  for (conn <- conns) {
	    if (scala.math.random < prob)
	      nc.addMutatedConnection(conn._1,conn._2._1,conn._2._2,flipP,dist)
	    else {
	      if (scala.math.random < flipP) {
	        nc.addConnection(conn._1,conn._2._1,negate(conn._2._2))
	      }
	      else {
	        nc.addConnection(conn._1,conn._2._1,conn._2._2)
	      }
	    }
	      
	  }
	  nc
	}
	def burstMutate2b(dist:Distribution) : Unit = {
	  var c2 = new TreeMap[Int,(Double,Byte)]()(Ordering.Int)
	  for (conn <- conns) {
	    c2 = c2 + ((conn._1,(conn._2._1+dist.inverse(scala.math.random),tricks.one)))
	  }
	  setConns(c2)//conns = c2
	}
  def createConnections[T<:Numeric[T]](m0: Int, m1: Int, conns2: Map[Int,(Double,T)]): NeuralConnsD = { 
    val rnc2 = new NeuralConnsD(m0,m1,maxVal)
    for ((d,(w,r)) <- conns2) {
      rnc2.addConnection(d,w,r)
    }
    rnc2
  }	
	/*
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
			    if (scala.math.random < 0.5) {
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
						if (scala.math.random < 0.5) {
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
	*/
	override def combine(nc2:NeuralConnections[Byte],dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double) : NeuralConnsD = {
	  var nc = new NeuralConnsD(minNode,maxNode)
	  val iter1 = conns.iterator
	  val iter2 = nc2.getFullIterator
	  var node = (0,(0.0,0.toByte))
	  var node2ready = false
	  var node1ready = false
	  //var c = (0,(0.0,true))
	  while (iter1.hasNext) {
	    if (node1ready) {
	      val (d,(w,b)) = node
	      var lower = true
	      var done = false
	      while (lower && iter2.hasNext) {
	        val (d2,(w2,b2)) = iter2.next
	        if (d2 < d) {
	          if (rnd.nextDouble > discardRate) {
	            if (rnd.nextDouble < mutP) {
	              nc.addMutatedConnection(d2,w2,b2,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d2,w2,b2)
	            }
	          }
	        }
	        else if (d2 == d) {
	          val (dx,(wx,bx)) = if (rnd.nextDouble < 0.5) (d,(w,b)) else (d2,(w2,b2))
	          if (rnd.nextDouble < mutP) {
	            nc.addMutatedConnection(dx,wx,bx,flipP,dist,rnd)
	          }
	          else {
	            nc.addConnection(dx,wx,bx)
	          }
	          lower = false
	          node1ready = false
	          done = true
	        }
	        else {
	          lower = false
	          if (rnd.nextDouble > discardRate) {
	            if (math.random < mutP) {
	              nc.addMutatedConnection(d,w,b,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d,w,b)

	              //
	            }
	          }
	          node = (d2,(w2,b2))
	          node2ready = true
	          node1ready = false
	          done = true
	        }
	        

	      }
	      if (!done && !iter2.hasNext) {
	        nc.addMutatedConnection(d,w,b,flipP,dist)
			while (iter1.hasNext) {
			val c = iter1.next
			if (rnd.nextDouble > discardRate) {
			  nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
			}
		  }
	      node1ready = false
	      }
	    }
	    else if (node2ready) {
	      val (d,(w,b)) = node
	      var lower = true
	      var done = false
	      while (lower && iter1.hasNext) {
	        val (d2,(w2,b2)) = iter1.next
	        if (d2 < d) {
	          if (rnd.nextDouble > discardRate) {
	            if (scala.math.random < mutP) {
	              nc.addMutatedConnection(d2,w2,b2,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d2,w2,b2)
	            }
	          }
	        }
	        else if (d2 == d) {
	          val (dx,(wx,bx)) = if (rnd.nextDouble < 0.5) (d,(w,b)) else (d2,(w2,b2))
	          if (rnd.nextDouble < mutP) {
	            nc.addMutatedConnection(dx,wx,bx,flipP,dist,rnd)
	          }
	          else {
	            nc.addConnection(dx,wx,bx)
	          }
	          lower = false
	          node2ready = false
	          done = true
	        }
	        else {
	          lower = false
	          if (rnd.nextDouble > discardRate) {
	            if (math.random < mutP) {
	              nc.addMutatedConnection(d,w,b,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d,w,b)
	            }
	          }
	          node2ready = false
	          node1ready = true
	          node = (d2,(w2,b2))
	          done = true
	        }
	      }
	      if (!done && !iter1.hasNext) {
		    nc.addMutatedConnection(d,w,b,flipP,dist)
		    while (iter2.hasNext) {
		      val c = iter2.next
			  if (rnd.nextDouble > discardRate) {
			    nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
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
		  if (math.random < mutP) {
	        nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist)
		  }
		  else {
		    nc.addConnection(node._1,node._2._1,node._2._2)
		  }
	    }
			
	  }
	  while (iter2.hasNext) {
	    node = iter2.next
	    if (math.random > discardRate) {
	      nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist)
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
  override def combine(nc2:AbstractNeuralconnections,dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double) : NeuralConnsD = {
	  var nc = new NeuralConnsD(minNode,maxNode,maxVal)
	  val iter1 = conns.iterator
	  val iter2 = nc2.getIterator
	  var node = (0,(0.0,0.toByte))
	  var node2ready = false
	  var node1ready = false
	  //var c = (0,(0.0,true))
	  while (iter1.hasNext) {
	    if (node1ready) {
	      val (d,(w,b)) = node
	      var lower = true
	      var done = false
	      while (lower && iter2.hasNext) {
	        val (d2,w2) = iter2.next
	        if (d2 < d) {
	          if (rnd.nextDouble > discardRate) {
	            if (rnd.nextDouble < mutP) {
	              nc.addMutatedConnection(d2,w2,1.toByte,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d2,w2,1.toByte)
	            }
	          }
	        }
	        else if (d2 == d) {
	          val (dx,(wx,bx)) = if (rnd.nextDouble < 0.5) (d,(w,b)) else (d2,(w2,1.toByte))
	          if (rnd.nextDouble < mutP) {
	            nc.addMutatedConnection(dx,wx,bx,flipP,dist,rnd)
	          }
	          else {
	            nc.addConnection(dx,wx,bx)
	          }
	          lower = false
	          node1ready = false
	          done = true
	        }
	        else {
	          lower = false
	          if (rnd.nextDouble > discardRate) {
	            if (math.random < mutP) {
	              nc.addMutatedConnection(d,w,b,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d,w,b)

	              //
	            }
	          }
	          node = (d2,(w2,1.toByte))
	          node2ready = true
	          node1ready = false
	          done = true
	        }
	        

	      }
	      if (!done && !iter2.hasNext) {
	        nc.addMutatedConnection(d,w,b,flipP,dist)
			while (iter1.hasNext) {
			val c = iter1.next
			if (rnd.nextDouble > discardRate) {
			  nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
			}
		  }
	      node1ready = false
	      }
	    }
	    else if (node2ready) {
	      val (d,(w,b)) = node
	      var lower = true
	      var done = false
	      while (lower && iter1.hasNext) {
	        val (d2,(w2,b2)) = iter1.next
	        if (d2 < d) {
	          if (rnd.nextDouble > discardRate) {
	            if (scala.math.random < mutP) {
	              nc.addMutatedConnection(d2,w2,b2,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d2,w2,b2)
	            }
	          }
	        }
	        else if (d2 == d) {
	          val (dx,(wx,bx)) = if (rnd.nextDouble < 0.5) (d,(w,b)) else (d2,(w2,b2))
	          if (rnd.nextDouble < mutP) {
	            nc.addMutatedConnection(dx,wx,bx,flipP,dist,rnd)
	          }
	          else {
	            nc.addConnection(dx,wx,bx)
	          }
	          lower = false
	          node2ready = false
	          done = true
	        }
	        else {
	          lower = false
	          if (rnd.nextDouble > discardRate) {
	            if (math.random < mutP) {
	              nc.addMutatedConnection(d,w,b,flipP,dist,rnd)
	            }
	            else {
	              nc.addConnection(d,w,b)
	            }
	          }
	          node2ready = false
	          node1ready = true
	          node = (d2,(w2,b2))
	          done = true
	        }
	      }
	      if (!done && !iter1.hasNext) {
		    nc.addMutatedConnection(d,w,b,flipP,dist)
		    while (iter2.hasNext) {
		      val (d3,w3) = iter2.next
		      val c = (d3,(w3,1.toByte))
			  if (rnd.nextDouble > discardRate) {
			    nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
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
		  if (math.random < mutP) {
	        nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist)
		  }
		  else {
		    nc.addConnection(node._1,node._2._1,node._2._2)
		  }
	    }
			
	  }
	  while (iter2.hasNext) {
	    val (d3,w3) = iter2.next
	    node = (d3,(w3,1.toByte))
	    if (math.random > discardRate) {
	      nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist)
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
  /*
	def combine3(nc2:NeuralConnsD,dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double = 0.75) : NeuralConnsD = {
		var nc = new NeuralConnsD(minNode,maxNode)
		//val discardRate = 0.5
		val iter1 = conns.iterator
		val iter2 = nc2.getConns2
		var node = (0,(0.0,1.toByte))
		var node2ready = false
		var node1ready = false
		var c = (0,(0.0,true))
		while (iter1.hasNext) {
		  if (node1ready) {
		    c = node
		    val k = c._1
			var lower = true
		    var done = false
			while (lower && iter2.hasNext) {
			  val c2 = iter2.next
		      if (c2._1 < k) {
		  	    if (rnd.nextDouble > discardRate) {
				  nc.addMutatedConnection(c2._1,c2._2._1,c2._2._2,flipP,dist,rnd)
			    }
		      }
			  else if (c2._1 == k) {
				if (rnd.nextDouble < 0.5) {
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
				  if (rnd.nextDouble > discardRate) {
					nc.addMutatedConnection(k,c._2._1,c._2._2,flipP,dist,rnd)
				  }
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
				if (rnd.nextDouble > discardRate) {
				  nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
				}
			  }
			}
			else if (node1ready) {
			  if (rnd.nextDouble > discardRate) {
				nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
			  }
			  node1ready = false
			}
		  }
		  else if (node2ready) {
		    c = iter1.next
		    val k = c._1
			if (k < node._1) {
			  if (rnd.nextDouble > discardRate) {
			    nc.addMutatedConnection(k,c._2._1,c._2._2,flipP,dist,rnd)
			  }
			}
			else if (k == node._2._1) {
			  if (scala.math.random < 0.5) {
			    nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist,rnd)
			  }
			  else {
			    nc.addMutatedConnection(k,node._2._1,node._2._2,flipP,dist,rnd)
			  }
			  node2ready = false
			}
			else {
			  if (rnd.nextDouble > discardRate) {
			    nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist,rnd)
			  }
			  node2ready = false
			  node1ready = true
			  node = c
			}
			  
		    
		  }
		  else {
		    c = iter1.next
		    val k = c._1
				var lower = true
				var done = false
				while (lower && iter2.hasNext) {
					val c2 = iter2.next
					if (c2._1 < k) {
					  if (rnd.nextDouble > discardRate) {
					    nc.addMutatedConnection(c2._1,c2._2._1,c2._2._2,flipP,dist,rnd)
					  }
					}
					else if (c2._1 == k) {
						if (scala.math.random < 0.5) {
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
						if (rnd.nextDouble > discardRate) {
						  nc.addMutatedConnection(k,c._2._1,c._2._2,flipP,dist,rnd)
						}
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
				    if (rnd.nextDouble > discardRate) {
				      nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
				    }
				  }
				}
				else if (node1ready) {
				  if (rnd.nextDouble > discardRate) {
				    nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
				  }
				  node1ready = false
				}
				
			}
		}
		if (node2ready || node1ready) {
		  if (rnd.nextDouble > discardRate) {
		    nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist)
		  }
			
		}
		while (iter2.hasNext) {
		  node = iter2.next
		  if (rnd.nextDouble > discardRate) {
		    nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist)
		  }
		}
		//Let's add or remove a random connection every once in a while
		while (rnd.nextDouble < mutP) {
		  if (rnd.nextDouble < 0.50) {
		    nc.addRandomConnection(rnd)
		  }
		  else {
		    nc.removeRandomConnection(rnd)
		  }
		}
		nc
	}
    def combine2(nc2:NeuralConnsD,dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double = 0.75) : NeuralConnsD = {
		var nc = new NeuralConnsD(minNode,maxNode)
		//val discardRate = 0.5
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
			    if (rnd.nextDouble > discardRate) {
			      nc.addMutatedConnection(k,c._2._1,c._2._2,flipP,dist,rnd)
			    }
			  }
			  else if (k == node._2._1) {
			    if (scala.math.random < 0.5) {
			      nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist,rnd)
			    }
			    else {
			      nc.addMutatedConnection(k,node._2._1,node._2._2,flipP,dist,rnd)
			    }
			    node2ready = false
			  }
			  else {
			    if (rnd.nextDouble > discardRate) {
			      nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist,rnd)
			    }
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
					  if (rnd.nextDouble > discardRate) {
					    nc.addMutatedConnection(c2._1,c2._2._1,c2._2._2,flipP,dist,rnd)
					  }
					}
					else if (c2._1 == k) {
						if (scala.math.random < 0.5) {
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
						if (rnd.nextDouble > discardRate) {
						  nc.addMutatedConnection(k,c._2._1,c._2._2,flipP,dist,rnd)
						}
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
				    if (rnd.nextDouble > discardRate) {
				      nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
				    }
				  }
				}
				else if (node1ready) {
				  if (rnd.nextDouble > discardRate) {
				    nc.addMutatedConnection(c._1,c._2._1,c._2._2,flipP,dist)
				  }
				  node1ready = false
				}
				
			}
		}
		if (node2ready || node1ready) {
		  if (rnd.nextDouble > discardRate) {
		    nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist)
		  }
			
		}
		while (iter2.hasNext) {
		  node = iter2.next
		  if (rnd.nextDouble > discardRate) {
		    nc.addMutatedConnection(node._1,node._2._1,node._2._2,flipP,dist)
		  }
		}
		//Let's add or remove a random connection every once in a while
		while (rnd.nextDouble < mutP) {
		  if (rnd.nextDouble < 0.50) {
		    nc.addRandomConnection(rnd)
		  }
		  else {
		    nc.removeRandomConnection(rnd)
		  }
		}
		nc
	}
	* 
	*/
	override def complexify(in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean,rnd:MersenneTwisterFast) : NeuralConnsD = {
	  complexifyWithTM2(in,blocks,memCells,out,addBlock,rnd)
	}
	def complexifyWithTM2(in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean,rnd:MersenneTwisterFast) : NeuralConnsD = {
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
	
	/*
	def calculateComplexity(msr:CComplexityMeasure) : Double = {
	  val size = conns.size
	  var sum = 0.0
	  for (c <- conns) {
	    sum += c._2._1*c._2._1
	  }
	  scala.math.log(5+sum)
	  /*
	  val mod = scala.math.log(size)
	  if (mod >= 1) {
	    sum/mod
	  }
	  else {
	    sum
	  }
	  */
	}
	*/
	
	def equals(other:NeuralConnsD) : Boolean = {
	  isEqualTo(other)
	}
	def isEqualTo(nc2:NeuralConnsD) : Boolean = {
	  if (nc2.conns.size != conns.size) {
	    return false
	  }
	  val iter = nc2.getConns2

	  while (iter.hasNext) {
	    val (d,(w,b)) = iter.next
	    if (conns.contains(d)) {
	      val (w2,b2) = conns.apply(d)
	      if (w != w2 || b != b2) {
	        return false
	      }
	    }
	    else {
	      return false
	    }
	  }
	  return true
	}
	override def makeClone : NeuralConnsD = {
	  val nc2 = new NeuralConnsD(minNode,maxNode,maxVal)
	  for (c <- conns) {
	    nc2.addConnection(c._1,c._2._1,c._2._2)
	  }
	  nc2
	}
	
	def toString2() : String = "<NeuralConnsD>\n"+conns.toString+"\n</NeuralConnsD>"
	override def toString : String = conns.toString
	override def toXML : Elem = {
	  <NeuralConnsD><Min>{minNode}</Min><Max>{maxNode}</Max>{for (c <- conns) yield <cnn><dest>{c._1}</dest><w>{c._2._1}</w><expr>{c._2._2}</expr></cnn>}</NeuralConnsD>
	  //new Elem("NeuralConnsD",conns.toString,null,null)
	}
	override def type2String : String = "Basic"

}
object NeuralConnsD {
  def fromXML(elem:Elem,maxVal:Double) : NeuralConnsD = {
    val minDst = (elem \\ "Min").text.toInt
	val maxDst = (elem \\ "Max").text.toInt
	val nc = new NeuralConnsD(minDst,maxDst,maxVal)
	elem match {
	  case <NeuralConnsD>{therms @ _*}</NeuralConnsD> => {
	    for (therm @ <cnn>{_*}</cnn> <- therms) {
	      val d = (therm \ "dest").text.toInt
	      val w = (therm \ "w").text.toDouble
	      val b = (therm \ "expr").text.toByte
	      nc.addConnection(d,w,b)
	    }
      }
	  case _ => {
	    println("NeuralConnsD received wrong sort of an xml representation.\n")
	  }
    }
    nc
  }
  def fromXML(ns:NodeSeq,maxVal:Double) : NeuralConnsD = {
    val mv = (ns \\ "Min").text.toInt
    val mv2 = (ns \\ "Max").text.toInt
    val nc = new NeuralConnsD(mv,mv2,maxVal)
    val cnns = XMLOperator.filterNodeSeq(ns)
    for (c <- cnns) {
      nc.addConnection((c \ "dest").text.toInt,(c \ "w").text.toDouble,(c \ "expr").text.toByte)
    }
    nc
  }
}