package neurogenesis.doubleprecision
import scala.util.Random
object EvoLauncher {
  val rnd = new Random
  def createCellBlock : CellBlockD = {
    val f = new Array[NeuralConnsD](2)
    val r = new Array[NeuralConnsD](2)
    for (i <- 0 until 2) {
      f(i) = new NeuralConnsD(10,20)
      f(i).addRandomConnections(4,rnd)
      r(i) = new NeuralConnsD(0,20)
      r(i).addRandomConnections(2,rnd)
    }
    return new CellBlockD(1,f,r)
  }
  def createInCell : InCellD = {
    val f = new NeuralConnsD(10,20)
    val r = new NeuralConnsD(0,20)
    f.addRandomConnections(3,rnd)
    r.addRandomConnections(3,rnd)
    new InCellD(f,r)
  }
  def createOutCell : OutCellD = {
    val r = new NeuralConnsD(0,20)
    r.addRandomConnections(3,rnd)
    new OutCellD(0,r)
  }
  def createRNN : RNND = {
    val in = new Array[InCellD](2)
    for (i <- 0 until in.length) {
      in(i) = createInCell
    }
    val cb = new Array[CellBlockD](2)
    for (i <- 0 until cb.length) {
      cb(i) = createCellBlock
    }
    val o = new Array[OutCellD](2)
    for (i <- 0 until o.length) {
      o(i) = createOutCell
    }
    new RNND(in,cb,o)
  }
  /**
   * This creates a simple network with known connections
   * that can be used for testing
   */
  def createBasicRNN : RNND = {
    val in = new Array[InCellD](2)
    val inFwd = new NeuralConnsD(2,14)
    val inRec = new NeuralConnsD(0,14)
    inFwd.addConnection(3,1)
    inFwd.addConnection(5,1)
    inFwd.addConnection(8,1)
    val cb = new Array[CellBlockD](2)
    val cbFwd1 = new Array[NeuralConnsD](2)
    val cbFwd2 = new Array[NeuralConnsD](2)
    val cbRec = new Array[NeuralConnsD](2)
    for (i <- 0 until 2) {
      cbFwd1(i) = new NeuralConnsD(12,14)
      cbFwd2(i) = new NeuralConnsD(12,14)
      cbRec(i) = new NeuralConnsD(0,14)
    }
    
    cb(0) = new CellBlockD(3,cbFwd1,cbRec)
    cb(1) = new CellBlockD(3,cbFwd1,cbRec)
    val out = new Array[OutCellD](2)
    for (i <- 0 until 2) {
      out(i) = new OutCellD(0,new NeuralConnsD(0,14))
      
    }
    new RNND(in,cb,out)
  }
  def main(args: Array[String]): Unit = {}
    val iface = new EvolverInterface
    iface.startup(new Array[String](1))
  
}