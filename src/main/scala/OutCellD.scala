package neurogenesis.doubleprecision

import neurogenesis.util.XMLOperator
import scalala.library.random.MersenneTwisterFast

import scala.xml.Elem
import scala.xml.NodeSeq
import neurogenesis.util.Distribution
class OutCellD(bias:Double,rConns:NeuralConnsD) extends EvolvableD {
  var stim: Double = 0
  var activation : Double = 0
  def activate(actFun: Function1[Double,Double]) : Double = { activation = actFun(stim+bias); activation }
  def addMersenneTwisterFastConnections(num:Int,rnd:MersenneTwisterFast) : Int = {
    rConns.addRandomConnections(num,rnd)
  }
  def burstMutate(prob:Double,dist:Distribution,rnd:MersenneTwisterFast) : OutCellD = {
    val nr = rConns.burstMutate(prob,dist,rnd) //new NeuralConns(rConns.getMin,rConns.getMax)
    new OutCellD(bias,nr)
  }
  def getRecurrent : NeuralConnsD = rConns
  def getActivation : Double = activation
  def getBias : Double = bias
  //def setFitness(f:Double) : Unit = { fitness = f }
  def stimulate(s:Double) : Unit = { stim += s }
  def combine(nc2:OutCellD,dist:Distribution,mutP:Double,flipP:Double) : OutCellD = {
    val r = rConns.combine(nc2.getRecurrent,dist,mutP,flipP)
    new OutCellD(bias,r)
  }
  def combine(nc2:OutCellD,dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double=0.75) : OutCellD = {
    val r = rConns.combine(nc2.getRecurrent,dist,mutP,flipP,rnd,discardRate)
    new OutCellD(bias,r)
  }
  def complexify(in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean,rnd:MersenneTwisterFast) : OutCellD = {
    val nc = new OutCellD(bias,rConns.complexify(in,blocks,memCells,out,addBlock,rnd))
    nc
  }
  def distance(ocell2:OutCellD) : Double = {
    rConns.dist(ocell2.getRecurrent)
  }
  def makeClone : OutCellD = {
    new OutCellD(bias,rConns.makeClone)
  }
  def equals(other:OutCellD) : Boolean = {
    (bias == other.getBias && rConns == other.getRecurrent)
  }
  def gatherConnections : List[NeuralConnsD] = { List(rConns) }
  override def setFitness(f:Double,measure:ComplexityMeasure,cBias:Double) : Unit = {
    val c = measure.calculateComplexity(List(rConns),cBias)
    if (c != 0.0) {
      if (f/c > fitness) {
        fitness = f/c
      }
    }
    else {
      if (f > fitness) {
        fitness = f
      }
    }
  }
  /*Constructs an XML representation of this OutCellD
   * 
   */
  def toXML : Elem = {
    val b = <Bias>{bias}</Bias>
    val e = <OutCellD>{b}{rConns.toXML}</OutCellD>
    e
  }

  override def toString : String = "<OutCellD>"+rConns+"\n</OutCellD>"
}
object OutCellD {
  def fromXML(e:Elem) : OutCellD = {
    val bs = (e \\ "Bias").text.toDouble
    val cxml = (e \\ "NeuralConnsD")
    val nc = NeuralConnsD.fromXML(cxml)
    new OutCellD(bs,nc)
  }
  def fromXML(ns:NodeSeq) : OutCellD = {
    val bs = (ns \\ "Bias").text.toDouble
    val rc = (ns \\ "NeuralConnsD")
    new OutCellD(bs,NeuralConnsD.fromXML(rc))
  }
}