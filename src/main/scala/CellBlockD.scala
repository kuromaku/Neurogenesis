package neurogenesis.doubleprecision

import scalala.library.random.MersenneTwisterFast
import scala.xml._
import neurogenesis.util.XMLOperator
import neurogenesis.util.Distribution

class CellBlockD(b:Double,fConns: Array[NeuralConnsD],rConns: Array[NeuralConnsD]) extends EvolvableD {
 val memState = new Array[Double](fConns.length)
 val stims = new Array[Double](fConns.length+3)
 val gateBits = new Array[Boolean](3)
 for (i <- 0 until 3) gateBits(i) = true
 val bias = b
 val numCells = memState.length
 //Recent addition was the ability to disable the effect of gates
 def activate : Array[Double] = {
   val actv = new Array[Double](numCells)
   for (i <- 0.to(numCells-1)) {
     stims(i) = -2*tanh(stims(i)/2)
   }
   val gates = new Array[Double](3)
   for (i <- numCells until (numCells+3)) {
     if (i != (numCells+1)) {
       if (gateBits(i-numCells)) {
         gates(i-numCells) = sigmoidExp(stims(i))
       }
       else {
         gates(i-numCells) = 1
       }
     }
     else {
       if (gateBits(i-numCells)) {
         gates(i-numCells) = sigmoidExp(stims(i)-bias)
       }
       else {
         gates(i-numCells) = 1
       }
     }
   }
   for (i <- 0 until memState.length) {
     memState(i) = memState(i)*gates(1)+stims(i)*gates(0)
     actv(i) = tanh(memState(i)*gates(2))
   }
   actv
 }
 def getBias : Double = bias
 def getSize : Int = stims.length
 def getNumOfCells : Int = memState.length
 def getForward(idx:Int) : NeuralConnsD = fConns(idx)
 def getRecurrent(idx:Int) : NeuralConnsD = rConns(idx)
 def flipBits(p:Double,rnd:MersenneTwisterFast) : Unit = for (i <- 0 until 3) { if (rnd.nextDouble < p) gateBits(i) = !gateBits(i) }
 def addMersenneTwisterFastConnections(num:Int,rnd:MersenneTwisterFast) : Int = {
   var sum = 0
   for (i <- 0 until fConns.length) {
     for (j <- 0.to(num)) {
       if (fConns(i).addRandomConnection(rnd))
         sum += 1
     }
     for (j <- 0.to(num)) {
       if (rConns(i).addRandomConnection(rnd))
         sum += 1
     }
   }
   sum
 }
 def burstMutate(prob:Double,dist:Distribution,rnd:MersenneTwisterFast) : CellBlockD = {
   val fc = new Array[NeuralConnsD](fConns.length)
   val rc = new Array[NeuralConnsD](rConns.length)
   for (i <- 0 until fConns.length) {
     fc(i) = fConns(i).burstMutate(prob,dist,rnd)
     rc(i) = rConns(i).burstMutate(prob,dist,rnd)
   }
   val b2 = new CellBlockD(bias,fc,rc)
   b2.flipBits(prob/5,rnd)
   return b2
 }
 def combine(block2:CellBlockD,dist:Distribution,mutP:Double,flipP:Double) : CellBlockD = {
   var f = new Array[NeuralConnsD](memState.length)
   var r = new Array[NeuralConnsD](memState.length)
   for (i <- 0 until f.length) {
     f(i) = fConns(i).combine(block2.getForward(i),dist,mutP,flipP)
     r(i) = rConns(i).combine(block2.getRecurrent(i),dist,mutP,flipP)
   }
   new CellBlockD(bias,f,r)
 }
 def combine(block2:CellBlockD,dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double=0.75) : CellBlockD = {
   var f = new Array[NeuralConnsD](memState.length)
   var r = new Array[NeuralConnsD](memState.length)
   for (i <- 0 until f.length) {
     f(i) = fConns(i).combine(block2.getForward(i),dist,mutP,flipP,rnd,discardRate)
     r(i) = rConns(i).combine(block2.getRecurrent(i),dist,mutP,flipP,rnd,discardRate)
   }
   val b2 = new CellBlockD(bias,f,r)
   for (i <- 0 until 3) {
     if (rnd.nextDouble < 0.5) {
       b2.gateBits(i) = this.gateBits(i)
     }
     else {
       b2.gateBits(i) = block2.gateBits(i)
     }
   }
   b2
 }
 def complexify(in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean,rnd:MersenneTwisterFast) : CellBlockD = {
   if (addBlock) {
     val ncf = new Array[NeuralConnsD](memState.length)
     val ncr = new Array[NeuralConnsD](memState.length)
     
     for (i <- 0 until fConns.length) {
       ncf(i) = fConns(i).complexify(in,blocks,memCells,out,addBlock,rnd)
       ncr(i) = fConns(i).complexify(in,blocks,memCells,out,addBlock,rnd)
     }
     val b2 = new CellBlockD(bias,ncf,ncr)
     for (i <- 0 until 3) {
       b2.gateBits(i) = this.gateBits(i)
     }
     b2
   }
   else {
     val ncf = new Array[NeuralConnsD](memState.length+1)
     val ncr = new Array[NeuralConnsD](memState.length+1)
     //var i = 0
     for (i <- 0 until fConns.length) {
       
       ncf(i) = fConns(i).complexify(in,blocks,memCells,out,addBlock,rnd)
       ncr(i) = fConns(i).complexify(in,blocks,memCells,out,addBlock,rnd)
       
     }
     val mid =in+blocks*(memCells+1+3)
     val i = fConns.length
     if (i > 1) {
       val ridx = rnd.nextInt(i-1)
       ncf(i) = ncf(ridx).makeClone//using this trick because we can't easily create a new NeuralConnsD(mid,mid+out)
       ncf(i).setMin(mid)
       ncf(i).setMax(mid+out)
       val ridx2 = rnd.nextInt(i-1)
       ncr(i) = rConns(ridx2).makeClone//same as above
       ncr(i).setMax(mid+out)
     }
     else {
       ncf(i) = ncf(0).makeClone//using this trick because we can't easily create a new NeuralConnsD(mid,mid+out)
       ncf(i).setMin(mid)
       ncf(i).setMax(mid+out)
       ncr(i) = rConns(0).makeClone//same as above
       ncr(i).setMax(mid+out)
     }
     for (j <- 0.to((Math.random*out).toInt)) {
       ncf(i).addRandomConnection(rnd)
     }
     for (j <- 0.to((Math.random*3).toInt)) {
       ncr(i).addRandomConnection(rnd)
     }
     
     val b2 = new CellBlockD(bias,ncf,ncr)
     for (i <- 0 until 3) {
       b2.gateBits(i) = this.gateBits(i)
     }
     b2
   }
 }
 def equals(other:CellBlockD) : Boolean = {
   if (bias != other.bias) {
     false
   }
   else {
     for (i <- 0 until fConns.length) {
       if (fConns(i) != other.getForward(i)) {
         false
       }
       if (rConns(i) != other.getRecurrent(i)) {
         false
       }
     }
   }
   true
 }
 def makeClone : CellBlockD = {
   val f = new Array[NeuralConnsD](fConns.length)
   val r = new Array[NeuralConnsD](rConns.length)
   for (i <- 0 until f.length) {
     f(i) = fConns(i).makeClone
     r(i) = rConns(i).makeClone
   }
   val b2 = new CellBlockD(bias,f,r)
   for (i <- 0 until 3) {
     b2.gateBits(i) = this.gateBits(i)
   }
   b2
 }
 def reset : Unit = {
   for (i <- 0 until memState.length) {
     memState(i) = 0
   }
 }
 override def setFitness(f:Double) : Unit = {
   var c = 0.0
   for (i <- 0 until fConns.length) {
     c += fConns(i).calculateComplexity + rConns(i).calculateComplexity
   }
   val fCand = f/c
   if (fCand > fitness) {
     fitness = fCand
   }
 }
 def stimulate2(addStims:Array[Double]) : Unit = {
   for (i <- 0.to(stims.length)) {
     stims(i) += addStims(i)
   }
 }
 def stimulate(stimVal:Double,idx:Int) : Unit = {
   stims(idx) += stimVal
 }
 def tanh(x:Double) : Double = {
   (Math.exp(2*x)-1)/(Math.exp(2*x)+1)
 }
 def sigmoidExp(x:Double) : Double = {
   1/(1+Math.exp(-x))
 }
 override def toString : String = {
   var srep = "<CellBlockD>"
   for (i <- 0.until(fConns.length)) {
     srep += "<fconn"+i+">"+fConns(i)+"<fconn"+i+">"
     srep += "<rconn"+i+">"+rConns(i)+"<rconn"+i+">"
   }
   srep += "</CellBlockD>"
   srep
 }
 def toXML : Elem = {
   val f = new Array[Elem](fConns.length)
   val r = new Array[Elem](rConns.length)
   val num = <MemCells>{rConns.length}</MemCells>
   val bs = <Bias>{bias}</Bias>
   var bSum = if (this.gateBits(0)) 4 else 0
   if (this.gateBits(1)) {
     bSum += 2 
   }
   if (this.gateBits(2)) {
     bSum += 1
   }
   val gateBits = <GateBits>{bSum}</GateBits>
   val top = TopScope
   for (i <- 0 until fConns.length) {
     f(i) = new Elem(null,"f"+i,null,top,fConns(i).toXML)
     r(i) = new Elem(null,"r"+i,null,top,rConns(i).toXML)
   }
   val e = <CellBlockD>{bs}{num}{gateBits}<Forward>{for (i <- 0 until fConns.length) yield f(i)}</Forward><Recurrent>{for (i <- 0 until fConns.length) yield r(i)}</Recurrent></CellBlockD>
   e
 }

 def setBits(n:Int) : Unit = {
   var t = n
   if (t >= 4) {
     gateBits(0) = true
     t -= 4
   }
   if (t >= 2) {
     gateBits(1) = true
     t -= 2
   }
   if (t == 1) {
     gateBits(2) = true
   }
 }
}
object CellBlockD {
  def fromXML(e:Elem) : CellBlockD = {
   val num = (e \\ "MemCells").text.toInt
   val rbias = (e \\ "Bias").text.toDouble
   val fwd = (e \\ "Forward")
   val rec = (e \\ "Recurrent")
   val f = new Array[NeuralConnsD](num)
   val r = new Array[NeuralConnsD](num)
   val minF = (fwd \\ "Min").apply(0).text.toInt
   val maxF = (fwd \\ "Max").apply(0).text.toInt
   val minR = (rec \\ "Min").apply(0).text.toInt
   val maxR = (rec \\ "Max").apply(0).text.toInt
   val bitRep = (e \\ "GateBits").text.toInt
   val op = XMLOperator
   var idx = 0
   
   for (i <- 0 until num) {
     val sf = "f"+i
     val sr = "r"+i
     f(i) = new NeuralConnsD(minF,maxF)
     r(i) = new NeuralConnsD(minR,maxR)
     val fi = (fwd \\ sf)
     val ri = (rec \\ sr)
     val seq = op.filterNodeSeq(fi)
     for (s <- seq) {
       f(i).addConnection((s \ "dest").text.toInt,(s \ "w").text.toDouble,(s \ "expr").text.toBoolean)
     }
     val seq2 = op.filterNodeSeq(ri)
     for (s <- seq2) {
       r(i).addConnection((s \ "dest").text.toInt,(s \ "w").text.toDouble,(s \ "expr").text.toBoolean)
     }
   }
   val c2 = new CellBlockD(rbias,f,r)
   c2.setBits(bitRep)
   return c2
 }
  def fromXML(e:NodeSeq) : CellBlockD = {
   val num = (e \\ "MemCells").text.toInt
   val rbias = (e \\ "Bias").text.toDouble
   val fwd = (e \\ "Forward")
   val rec = (e \\ "Recurrent")
   val f = new Array[NeuralConnsD](num)
   val r = new Array[NeuralConnsD](num)
   val minF = (fwd \\ "Min").apply(0).text.toInt
   val maxF = (fwd \\ "Max").apply(0).text.toInt
   val minR = (rec \\ "Min").apply(0).text.toInt
   val maxR = (rec \\ "Max").apply(0).text.toInt
   val bitRep = (e \\ "GateBits").text.toInt
   val op = XMLOperator
   var idx = 0
   
   for (i <- 0 until num) {
     val sf = "f"+i
     val sr = "r"+i
     f(i) = new NeuralConnsD(minF,maxF)
     r(i) = new NeuralConnsD(minR,maxR)
     val fi = (fwd \\ sf)
     val ri = (rec \\ sr)
     val seq = op.filterNodeSeq(fi)
     for (s <- seq) {
       f(i).addConnection((s \ "dest").text.toInt,(s \ "w").text.toDouble,(s \ "expr").text.toBoolean)
     }
     val seq2 = op.filterNodeSeq(ri)
     for (s <- seq2) {
       r(i).addConnection((s \ "dest").text.toInt,(s \ "w").text.toDouble,(s \ "expr").text.toBoolean)
     }
   }
   val c2 = new CellBlockD(rbias,f,r)
   c2.setBits(bitRep)
   return c2
 }
}