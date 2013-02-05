package neurogenesis.doubleprecision

import neurogenesis.util.XMLOperator
import scalala.library.random.MersenneTwisterFast

import scala.xml.Elem
import scala.xml.NodeSeq
import neurogenesis.util.Distribution
import neurogenesis.util.CComplexityMeasure

class OutCellD(bias:Double,rConns:AbstractNeuralconnections) extends EvolvableD {
  var stim: Double = 0
  var activation : Double = 0
  def activate(actFun: Function1[Double,Double]) : Double = { activation = actFun(stim+bias); activation }
  def addRandomConnections(num:Int,rnd:MersenneTwisterFast) : Int = {
    rConns.addRandomConnections(num,rnd)
  }
  def burstMutate(prob:Double,dist:Distribution,rnd:MersenneTwisterFast) : OutCellD = {
    val nr = rConns.burstMutate(prob,dist,rnd) //new NeuralConns(rConns.getMin,rConns.getMax)
    new OutCellD(bias,nr)
  }
  def burstMutate(prob:Double,dist:Distribution,rnd:MersenneTwisterFast,cpop:CellPopulationD) : OutCellD = {
    val nr = rConns.burstMutate(prob,dist,rnd) //new NeuralConns(rConns.getMin,rConns.getMax)
    val out2 = new OutCellD(bias,nr)
    out2.setID(cpop.getCounter)
    cpop.add2Counter
    out2
  }
  def getRecurrent : AbstractNeuralconnections = rConns
  def getActivation : Double = activation
  def getBias : Double = bias
  //def setFitness(f:Double) : Unit = { fitness = f }
  def stimulate(s:Double) : Unit = { stim += s }
  def reset : Unit = { stim = 0; activation = 0 }
  /*
  def combine(nc2:OutCellD,dist:Distribution,mutP:Double,flipP:Double) : OutCellD = {
    val r = rConns.combine(nc2.getRecurrent,dist,mutP,flipP)
    new OutCellD(bias,r)
  }
  */
  def combine(nc2:OutCellD,dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double=0.75) : OutCellD = {
    val r = rConns.combine(nc2.getRecurrent,dist,mutP,flipP,rnd,discardRate)
    new OutCellD(bias,r)
  }
  def combine(nc2:OutCellD,dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double,cellpop:CellPopulationD) : OutCellD = {
    val r = rConns.combine(nc2.getRecurrent,dist,mutP,flipP,rnd,discardRate)
    val out2 = new OutCellD(bias,r)
    out2.setID(cellpop.getCounter)
    cellpop.add2Counter
    out2
  }
  def complexify(in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean,rnd:MersenneTwisterFast) : OutCellD = {
    val nc = new OutCellD(bias,rConns.complexify(in,blocks,memCells,out,addBlock,rnd))
    nc
  }
  def distance(ocell2:OutCellD) : Double = {
    rConns.dist(ocell2.getRecurrent)
  }
  def makeClone : OutCellD = {
    val oc2 = new OutCellD(bias,rConns.makeClone)
    oc2.setID(getID)
    oc2
  }
  def equals(other:OutCellD) : Boolean = {
    (bias == other.getBias && rConns == other.getRecurrent)
  }
  def gatherConnections : List[AbstractNeuralconnections] = { List(rConns) }
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
    //val ctype = <CType>{if (rConns.isInstanceOf[RigidNeuralConnections]) "Rigid" else "Basic"}</CType>
    //{ctype}
    val e = <OutCellD>{b}{rConns.toXML}</OutCellD>
    e
  }

  override def toString : String = "<OutCellD>"+rConns+"\n</OutCellD>"
  //def this(e:Elem) = this((e \\ "Bias").text.toDouble,NeuralConnsD.fromXML((e \\ "NeuralConnsD")))
}
object OutCellD {
  def fromXML(e:Elem,maxVal:Double,ctype:String) : OutCellD = {
    val bs = (e \\ "Bias").text.toDouble
    
    val ctype = (e \\ "CType").text
    var cnn: AbstractNeuralconnections = null
    ctype match {
      case "Rigid" => cnn = RigidNeuralConnections.fromXML((e \\ "RNNConns"),maxVal)
      case "Basic" => cnn = NeuralConnsD.fromXML((e \\ "NeuralConnsD"),maxVal)
      case _ => //add more cases when necessary
    }
    new OutCellD(bs,cnn)
  }
  def fromXML(ns:NodeSeq,maxVal:Double,ctype:String) : OutCellD = {
    val bs = (ns \\ "Bias").text.toDouble
    ctype match {
      case "Rigid" => new OutCellD(bs,RigidNeuralConnections.fromXML((ns \\ "RNNConns"),maxVal))
      case "Basic" => new OutCellD(bs,NeuralConnsD.fromXML((ns \\ "NeuralConnsD"),maxVal))
      case _ => new OutCellD(bs,null)
    }
  }
}