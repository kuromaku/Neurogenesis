package neurogenesis.doubleprecision
import scalala.library.random.MersenneTwisterFast
import neurogenesis.util.OutFun

object EvoLauncher {
  
  def createCellBlock(rnd:MersenneTwisterFast) : CellBlockD = {
    val f = new Array[NeuralConnsD](2)
    val r = new Array[NeuralConnsD](2)
    for (i <- 0 until 2) {
      f(i) = new NeuralConnsD(12,14)
      f(i).addRandomConnections(3,rnd)
      r(i) = new NeuralConnsD(0,14)
      r(i).addRandomConnections(2,rnd)
    }
    return new CellBlockD(1,f,r)
  }
  def createInCell(rnd:MersenneTwisterFast) : InCellD = {
    val f = new NeuralConnsD(2,14)
    val r = new NeuralConnsD(0,14)
    f.addRandomConnections(3,rnd)
    r.addRandomConnections(3,rnd)
    new InCellD(f,r)
  }
  def createOutCell(rnd:MersenneTwisterFast) : OutCellD = {
    val r = new NeuralConnsD(0,14)
    r.addRandomConnections(3,rnd)
    new OutCellD(0,r)
  }
  def createRNN(rnd:MersenneTwisterFast) : RNND = {
    val in = new Array[InCellD](2)
    for (i <- 0 until in.length) {
      in(i) = createInCell(rnd)
    }
    val cb = new Array[CellBlockD](2)
    for (i <- 0 until cb.length) {
      cb(i) = createCellBlock(rnd)
    }
    val o = new Array[OutCellD](2)
    for (i <- 0 until o.length) {
      o(i) = createOutCell(rnd)
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
    inRec.addConnection(10,0.5)
    in(0) = new InCellD(inFwd,inRec)
    val inFwd2 = new NeuralConnsD(2,14)
    val inRec2 = new NeuralConnsD(0,14)
    inFwd2.addConnection(4,1)
    inFwd2.addConnection(6,-1)
    inFwd2.addConnection(9,-0.5)
    inFwd2.addConnection(10,-0.5)
    inRec2.addConnection(7,1)
    in(1) = new InCellD(inFwd2,inRec2)
    val cb = new Array[CellBlockD](2)
    val cbFwd1 = new Array[NeuralConnsD](2)
    val cbFwd2 = new Array[NeuralConnsD](2)
    val cbRec = new Array[NeuralConnsD](2)
    for (i <- 0 until 2) {
      cbFwd1(i) = new NeuralConnsD(12,14)
      cbFwd2(i) = new NeuralConnsD(12,14)
      cbRec(i) = new NeuralConnsD(0,14)
    }
    cbFwd1(0).addConnection(12,1)
    cbFwd1(0).addConnection(13,-0.5)
    cbFwd1(1).addConnection(12,0.2)
    cbFwd1(1).addConnection(12,0.5)
    cbRec(1).addConnection(1,0.5)
    cbRec(0).addConnection(0,0.2)
    cb(0) = new CellBlockD(3,cbFwd1,cbRec)
    cb(1) = new CellBlockD(3,cbFwd1,cbRec)
    val out = new Array[OutCellD](2)
    val outR0 = new NeuralConnsD(0,14)
    val outR1 = new NeuralConnsD(0,14)
    out(0) = new OutCellD(0,outR0)
    out(1) = new OutCellD(0,outR1)
    new RNND(in,cb,out)
  }
  def debugPrint(res:Array[Double],out:Int,blocks:Int,cells:Int) : Unit = {
    for (i <- 0 until out) {
      print(res(i)+" ")
    }
    for (i <- 0 until blocks) {
      print("block: "+i+" ")
      for (j <- 0 until cells) {
        print(res(out+i*blocks+j)+" ")
      }
    }
    print("\n")
  }
  def debugRun : Unit = {
    val rnd = new MersenneTwisterFast
    val rnn = createRNN(rnd)
    //var l = List[Array[Double]]()
    val exp = new OutFun
    for (i <- 0 until 20) {
      val a = new Array[Double](2)
      a(0) = rnd.nextDouble
      a(1) = rnd.nextDouble
      //l = l.:+(a)
      val res = rnn.evolinoActivate(a,exp)
      println("Input:"+res(0)+" "+res(1))
      debugPrint(res,2,2,2)
    }
    println("XML rep of RNN:")
    println(rnn.toXML)
  }
  def debugRun2 : Unit = {
    val rnn = createBasicRNN
    val a0 = new Array[Double](2)
    a0(0) = 1
    a0(1) = -1
    val res0 = rnn.evolinoActivate(a0,new OutFun)
    val a1 = new Array[Double](2)
    a1(0) = 0.5
    a1(1) = -0.5
    val res1 = rnn.evolinoActivate(a1,new OutFun)
    debugPrint(res0,2,2,2)
    debugPrint(res1,2,2,2)
    println("XML rep of RNN:")
    println(rnn.toXML)
  }
  def testCombine3(num:Int) : (Boolean,Int) = {
    var pass = true
    var i = 0
    val rnd = new MersenneTwisterFast
    while (pass && i < num) {
      val nc = new NeuralConnsD(0,20)
      nc.addRandomConnections(5,rnd)
      val nc2 = new NeuralConnsD(0,20)
      nc2.addRandomConnections(6,rnd)
      val nc3 = nc.combine3(nc2,new neurogenesis.util.CauchyDistribution(0.01),0.0,0.01,rnd,0)
      for ((d,w) <- nc.getMap) {
        if (!nc3.getMap.contains(d)) {
          pass = false
        }
      }
      for ((d,w) <- nc2.getMap) {
        if (!nc3.getMap.contains(d)) {
          pass = false
        }
      }
      i += 1
    }
    (pass,i)
  }
  def testCombine1(num:Int) : (Boolean,Int) = {
    var pass = true
    var i = 0
    val rnd = new MersenneTwisterFast
    while (pass && i < num) {
      val nc = new NeuralConnsD(0,20)
      nc.addRandomConnections(5,rnd)
      val nc2 = new NeuralConnsD(0,20)
      nc2.addRandomConnections(6,rnd)
      val nc3 = nc.combine(nc2,new neurogenesis.util.CauchyDistribution(0.01),0.0,0.01,rnd,0)
      for ((d,w) <- nc.getMap) {
        if (!nc3.getMap.contains(d)) {
          pass = false
        }
      }
      for ((d,w) <- nc2.getMap) {
        if (!nc3.getMap.contains(d)) {
          pass = false
        }
      }
      i += 1
    }
    (pass,i)
  }
  def main(args: Array[String]): Unit = {
    //debugRun2
    /*
    val time = System.currentTimeMillis()
    println(time)
    println("Test: "+testCombine3(5000))
    println(System.currentTimeMillis()-time)
    println("Test: "+testCombine1(5000))
    println(System.currentTimeMillis()-time)
    println("Test: "+testCombine3(5000))
    println(System.currentTimeMillis()-time)
    println("Test: "+testCombine1(5000))
    println(System.currentTimeMillis()-time)
    */
    val iface = new EvolverInterface
    iface.startup(new Array[String](0))
  }
}