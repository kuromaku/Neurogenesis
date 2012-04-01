package neurogenesis.doubleprecision
import neurogenesis.util.XMLOperator
import neurogenesis.util.Distribution
import scala.util.Random
import scala.xml._

class InCellD(fConns:NeuralConnsD,rConns:NeuralConnsD) extends EvolvableD {
  var stim = 0d
  var activation = 0d
  def activate(actFun: Function1[Double,Double]) : Double = {activation = actFun(stim); activation }
  def addRandomConnections(num:Int,rnd:Random) : Int = {
    fConns.addRandomConnections(num,rnd)
  }
  def burstMutate(prob:Double,dist:Distribution,rnd:Random) : InCellD = {
    val fc = fConns.burstMutate(prob,dist,rnd)
    var rc = rConns.burstMutate(prob,dist,rnd)
    new InCellD(fc,rc)
  }
  def getForward : NeuralConnsD = fConns
  def getRecurrent : NeuralConnsD = rConns
  def getActivation : Double = activation
  override def setFitness(f:Double) : Unit = {
    val c = fConns.calculateComplexity+rConns.calculateComplexity
    val fCand = f/c
    if (fCand > fitness) {
      fitness = fCand
    }
  }
  def stimulate(s:Double) : Unit = { stim += s }
  
  def combine(e2: InCellD,dist:Distribution,mutP:Double,flipP:Double) : InCellD = {
    //val cops = implicitly[InCellD]
    val f = fConns.combine(e2.getForward,dist,mutP,flipP)
    val r = rConns.combine(e2.getRecurrent,dist,mutP,flipP)
    new InCellD(f,r)
  }
  def combine(e2: InCellD,dist:Distribution,mutP:Double,flipP:Double,rnd:Random) : InCellD = {
    //val cops = implicitly[InCellD]
    val f = fConns.combine(e2.getForward,dist,mutP,flipP,rnd)
    val r = rConns.combine(e2.getRecurrent,dist,mutP,flipP,rnd)
    new InCellD(f,r)
  }
  def complexify(in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean,rnd:Random) : InCellD = {
    new InCellD(fConns.complexify(in,blocks,memCells,out,addBlock,rnd),rConns.complexify(in,blocks,memCells,out,addBlock,rnd))
  }
  def makeClone : InCellD = {
    val fw = getForward
    val rc = getRecurrent
    new InCellD(fw.makeClone,rc.makeClone)
  }
  def toXML : Elem = {
    val fwd = <Forward>{fConns.toXML}</Forward>
    val rec = <Recurrent>{rConns.toXML}</Recurrent>
    val e = <InCellD>{fwd}{rec}</InCellD>
    e
  }
  
  def fromXML(elem:Elem) : InCellD = {
    val fwd = elem \\ "Forward"
    val rec = elem \\ "Recurrent"
    val fc = new NeuralConnsD((fwd \\ "Min").text.toInt,(fwd \\ "Max").text.toInt)
    val rc = new NeuralConnsD((rec \\ "Min").text.toInt,(rec \\ "Max").text.toInt)
    val seq = XMLOperator.filterNodeSeq(fwd)
    for (s <- seq) {
      fc.addConnection((s \ "dest").text.toInt,(s \ "w").text.toDouble,(s \ "expr").text.toBoolean)
    }
    val seq2 = XMLOperator.filterNodeSeq(rec)
    for (s <- seq2) {
      rc.addConnection((s \ "dest").text.toInt,(s \ "w").text.toDouble,(s \ "expr").text.toBoolean)
    }
    new InCellD(fc,rc)
  }
  
  override def toString : String = "<InCellD><Forward>"+fConns+"</Forward><Recurrent>\n"+rConns+"</Recurrent></InCellD>"
}