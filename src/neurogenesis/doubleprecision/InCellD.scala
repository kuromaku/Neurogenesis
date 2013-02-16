package neurogenesis.doubleprecision
import neurogenesis.util.XMLOperator
import neurogenesis.util.Distribution
import neurogenesis.util.CComplexityMeasure
import scalala.library.random.MersenneTwisterFast
import scala.xml._

/*A simple input cell which can have both forward and recurrent connections
 * 
 */
class InCellD(fConns:AbstractNeuralconnections,rConns:AbstractNeuralconnections) extends EvolvableD {
  var stim = 0d
  var activation = 0d
  def activate(actFun: Function1[Double,Double]) : Double = {activation = actFun(stim); activation }
  def addRandomConnections(num:Int,rnd:MersenneTwisterFast) : Int = {
    fConns.addRandomConnections(num,rnd)
  }
  def burstMutate(prob:Double,dist:Distribution,rnd:MersenneTwisterFast) : InCellD = {
    val fc = fConns.burstMutate(prob,dist,rnd)
    var rc = rConns.burstMutate(prob,dist,rnd)
    new InCellD(fc,rc)
  }
  def burstMutate(prob:Double,dist:Distribution,rnd:MersenneTwisterFast,cpop:CellPopulationD) : InCellD = {
    val fc = fConns.burstMutate(prob,dist,rnd)
    var rc = rConns.burstMutate(prob,dist,rnd)
    val out2 = new InCellD(fc,rc)
    out2.setID(cpop.getCounter)
    cpop.add2Counter
    out2
  }
  def equals(other:InCellD) : Boolean = {
    (fConns == other.getForward && rConns == other.getRecurrent)
  }
  def getForward : AbstractNeuralconnections = fConns
  def getRecurrent : AbstractNeuralconnections = rConns
  def getActivation : Double = activation
  def gatherConnections : List[AbstractNeuralconnections] = {
    List(fConns,rConns)
  }
  def getMaxWeight : Double = fConns.getMaxWeight
  
  override def setFitness(f:Double,measure:ComplexityMeasure,cBias:Double) : Unit = {
    val c = measure.calculateComplexity(List(fConns,rConns),cBias)
    if (c != 0) {
      val fCand = f/c
      if (fCand > fitness) {
        fitness = fCand
      }
    }
    else {
      if (f > fitness) {
        fitness = f
      }
    }
  }
  def reset : Unit = { stim = 0; activation = 0 }
  def stimulate(s:Double) : Unit = { stim += s }
  
  /*
  def combine(e2: InCellD,dist:Distribution,mutP:Double,flipP:Double) : InCellD = {
    //val cops = implicitly[InCellD]
    val f = fConns.combine(e2.getForward,dist,mutP,flipP)
    val r = rConns.combine(e2.getRecurrent,dist,mutP,flipP)
    new InCellD(f,r)
  }
  */
  def combine(e2: InCellD,dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double=0.75) : InCellD = {
    //val cops = implicitly[InCellD]
    val f = fConns.combine(e2.getForward,dist,mutP,flipP,rnd,discardRate)
    val r = rConns.combine(e2.getRecurrent,dist,mutP,flipP,rnd,discardRate)
    new InCellD(f,r)
  }
  def combine(e2: InCellD,dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double,cellpop:CellPopulationD) : InCellD = {
    //val cops = implicitly[InCellD]
    val f = fConns.combine(e2.getForward,dist,mutP,flipP,rnd,discardRate)
    val r = rConns.combine(e2.getRecurrent,dist,mutP,flipP,rnd,discardRate)
    val cell2 = new InCellD(f,r)
    cell2.setID(cellpop.getCounter)
    cellpop.add2Counter
    cell2
  }
  def complexify(in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean,rnd:MersenneTwisterFast) : InCellD = {
    new InCellD(fConns.complexify(in,blocks,memCells,out,addBlock,rnd),rConns.complexify(in,blocks,memCells,out,addBlock,rnd))
  }
  def distance(cell2:InCellD) : Double = {
    val d1 = fConns.dist(cell2.getForward)
    val d2 = rConns.dist(cell2.getRecurrent)
    d1+d2
  }
  def makeClone : InCellD = {
    val fw = getForward
    val rc = getRecurrent
    val nc = new InCellD(fw.makeClone,rc.makeClone)
    nc.setID(getID)
    nc
  }
  def toXML : Elem = {
    val fwd = <Forward>{fConns.toXML}</Forward>
    val rec = <Recurrent>{rConns.toXML}</Recurrent>
    //val ctype = <CType>{if (rConns.isInstanceOf[RigidNeuralConnections]) "Rigid" else "Basic"}</CType>
    val e = <InCellD>{fwd}{rec}</InCellD>
    e
  }

  
  override def toString : String = "<InCellD><Forward>"+fConns+"</Forward><Recurrent>\n"+rConns+"</Recurrent></InCellD>"
}
object InCellD {
  def fromXML(elem:Elem,maxWeight:Double,ctype:String) : InCellD = {
    val fwd = elem \\ "Forward"
    val rec = elem \\ "Recurrent"
    if (ctype == "Basic") {
    val fc = new NeuralConnsD((fwd \\ "Min").text.toInt,(fwd \\ "Max").text.toInt,maxWeight)
    val rc = new NeuralConnsD((rec \\ "Min").text.toInt,(rec \\ "Max").text.toInt,maxWeight)
    val seq = XMLOperator.filterNodeSeq(fwd)
    for (s <- seq) {
      fc.addConnection((s \ "dest").text.toInt,(s \ "w").text.toDouble,(s \ "expr").text.toByte)
    }
    val seq2 = XMLOperator.filterNodeSeq(rec)
    for (s <- seq2) {
      rc.addConnection((s \ "dest").text.toInt,(s \ "w").text.toDouble,(s \ "expr").text.toByte)
    }
    new InCellD(fc,rc)
    }
    else {
      val fc = RigidNeuralConnections.fromXML(fwd,maxWeight)
      val rc = RigidNeuralConnections.fromXML(rec,maxWeight)
      new InCellD(fc,rc)
    }
  }
  def fromXML(ns:NodeSeq,maxWeight:Double,ctype:String) : InCellD = {
    val fwd = ns \\ "Forward"
    if (ctype == "Basic") {
      val fc = NeuralConnsD.fromXML(fwd,maxWeight)
      val rc = NeuralConnsD.fromXML(ns \\ "Recurrent",maxWeight)
      new InCellD(fc,rc)}
    else {
      new InCellD(RigidNeuralConnections.fromXML(fwd,maxWeight),RigidNeuralConnections.fromXML((ns \\ "Recurrent"),maxWeight))
    }
  }
}