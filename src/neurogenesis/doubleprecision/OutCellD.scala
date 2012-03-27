package neurogenesis.doubleprecision

import neuroprogressor._
import scala.util.Random
import scala.xml._

class OutCellD(bias:Double,rConns:NeuralConnsD) extends EvolvableD {
  var stim: Double = 0
  var activation : Double = 0
  def activate(actFun: Function1[Double,Double]) : Double = { activation = actFun(stim+bias); activation }
  def addRandomConnections(num:Int,rnd:Random) : Int = {
    rConns.addRandomConnections(num,rnd)
  }
  def burstMutate(prob:Double,dist:Distribution,rnd:Random) : OutCellD = {
    val nr = rConns.burstMutate(prob,dist,rnd) //new NeuralConns(rConns.getMin,rConns.getMax)
    new OutCellD(bias,nr)
  }
  def getRecurrent : NeuralConnsD = rConns
  def getActivation : Double = activation
  //def setFitness(f:Double) : Unit = { fitness = f }
  def stimulate(s:Double) : Unit = { stim += s }
  def combine(nc2:OutCellD,dist:Distribution,mutP:Double,flipP:Double) : OutCellD = {
    val r = rConns.combine(nc2.getRecurrent,dist,mutP,flipP)
    new OutCellD(bias,r)
  }
  def combine(nc2:OutCellD,dist:Distribution,mutP:Double,flipP:Double,rnd:Random) : OutCellD = {
    val r = rConns.combine(nc2.getRecurrent,dist,mutP,flipP,rnd)
    new OutCellD(bias,r)
  }
  def complexify(in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean,rnd:Random) : OutCellD = {
    val nc = new OutCellD(bias,rConns.complexify(in,blocks,memCells,out,addBlock,rnd))
    nc
  }
  def makeClone : OutCellD = {
    new OutCellD(bias,rConns.makeClone)
  }
  override def setFitness(f:Double) : Unit = {
    val c = rConns.calculateComplexity
    if (c != 0.0) {
      fitness = f/c
    }
    else {
      fitness = f
    }
  }
  /*Constructs an XML representation of this OutCellT
   * 
   */
  def toXML : Elem = {
    val b = <Bias>{bias}</Bias>
    val e = <OutCellT>{b}{rConns.toXML}</OutCellT>
    e
  }
  def fromXML(e:Elem) : OutCellD = {
    val bs = (e \\ "Bias").text.toDouble
    val cxml = (e \\ "NeuralConnsD")
    val nc = new NeuralConnsD(0,0)
    new OutCellD(bs,nc.fromXML(XMLOperator.toElem(cxml)))
  }
  override def toString : String = "<OutCellT>"+rConns+"\n</OutCellT>"
}