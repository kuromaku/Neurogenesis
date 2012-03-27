package neurogenesis.doubleprecision
import scala.util.Random
import java.io.File
import java.io.FileReader
import scala.xml._
import scala.collection.mutable.ArrayOps

class CellPopulationD(inputs:Int,blocks:Int,outputs:Int,popSize:Int)  {
  //var numBlocks = 2
  val inputPop = new Array[Array[InCellD]](inputs)
  val blockPop = new Array[Array[CellBlockD]](blocks)
  val outputPop = new Array[Array[OutCellD]](outputs)
  
  def getSize : Int = popSize
  def getIn : Int = inputs
  def getOut : Int = outputs
  def getBlocks : Int = blocks
  //var mutProb = 0.1d
  //var flipProb = 0.01d
  var connProb = 0.5
  //def setMutProb(mp:Double) : Unit = { mutProb = mp }
  //def setFlipProb(fp:Double) : Unit = { flipProb = fp }
  //val random = new Random
  
  def init(scale:Double,outBias:Double,rnd:Random) : Unit = {
    val cDist = new CauchyDistribution(scale)
    val mid = inputs + blocks*4
    val total = mid+outputs
    val numFor = 4
    val numRec = 2
    for (i <- 0.until(inputs)) {
      inputPop(i) = new Array[InCellD](popSize)
      for (j <- 0.until(popSize)) {
        val fc = new NeuralConnsD(inputs,total)
        fc.addRandomConnections(numFor,rnd)
        val rc = new NeuralConnsD(0,total)
        rc.addRandomConnections(numRec,rnd)
        inputPop(i)(j) = new InCellD(fc,rc)
      }
    }
    for (i <- 0.until(blocks)) {
      blockPop(i) = new Array(popSize)
      val memBias = 1.0
      val mCells = if (blocks == 1) 2 else 1
      for (j <- 0.until(popSize)) {
        val fc = new Array[NeuralConnsD](mCells)
        val rc = new Array[NeuralConnsD](mCells)
        for (k <- 0 until mCells) {
          fc(k) = new NeuralConnsD(mid,total)
          fc(k).addRandomConnections(numFor,rnd)
          rc(k) = new NeuralConnsD(0,total)
          rc(k).addRandomConnections(numRec,rnd)
        }
        blockPop(i)(j) = new CellBlockD(memBias,fc,rc)
      }
    }
    for (i <- 0.until(outputs)) {
      outputPop(i) = new Array[OutCellD](popSize)
      for (j <- 0.until(popSize)) {
        val rc = new NeuralConnsD(0,total)
        rc.addRandomConnections(numRec,rnd)
        outputPop(i)(j) = new OutCellD(outBias,rc)
      }
    }
  }
  def getRNN(rnd:Random) : RNND = {
    val ic = new Array[InCellD](inputs)
    for (i <- 0.until(inputs)) {
      ic(i) = inputPop(i)(rnd.nextInt(popSize))
    }
    val bc = new Array[CellBlockD](blocks)
    for (i <- 0.until(blocks)) {
      bc(i) = blockPop(i)(rnd.nextInt(popSize))
    }
    val oc = new Array[OutCellD](outputs)
    for (i <- 0.until(outputs)) {
      oc(i) = outputPop(i)(rnd.nextInt(popSize))
    }
    return new RNND(ic,bc,oc)
  }
  def getRNN2(idx:Int,permsIn:Array[Array[Int]],permsB:Array[Array[Int]],permsOut:Array[Array[Int]]) : RNND = {
    val ic = new Array[InCellD](inputs)
    for (i <- 0.until(inputs)) {
      ic(i) = inputPop(i)(permsIn(i)(idx))
    }
    val bc = new Array[CellBlockD](blocks)
    for (i <- 0.until(blocks)) {
      bc(i) = blockPop(i)(permsB(i)(idx))
    }
    val oc = new Array[OutCellD](outputs)
    for (i <- 0.until(outputs)) {
      oc(i) = outputPop(i)(permsOut(i)(idx))
    }
    return new RNND(ic,bc,oc)
  }
  def getNetworks(permsIn:Array[Array[Int]],permsB:Array[Array[Int]],permsOut:Array[Array[Int]]) : Array[RNND] = {
    val networks = new Array[RNND](permsIn(0).length)
    for (i <- 0.until(networks.length)) {
      networks(i) = getRNN2(i,permsIn,permsB,permsOut)
    }
    networks
  }
  def replaceCells(in:Array[Array[InCellD]],b:Array[Array[CellBlockD]],out:Array[Array[OutCellD]]) : Unit = {
    for (i <- 0 until in.length) {
      inputPop(i) = new Array[InCellD](popSize)
      for (j <- 0 until popSize) {
        inputPop(i)(j) = in(i)(j)
      }
    }
    for (i <- 0 until b.length) {
      blockPop(i) = new Array[CellBlockD](popSize)
      for (j <- 0 until popSize) {
        blockPop(i)(j) = b(i)(j)
      }
    }
    for (i <- 0 until out.length) {
      outputPop(i) = new Array[OutCellD](popSize)
      for (j <- 0 until popSize) {
        outputPop(i)(j) = out(i)(j)
      }
    }
  }
  def burstMutate(burstProb:Double,dist:Distribution,rnd:Random) : Unit = {
    for (i <- 0 until popSize-3) {
      for (j <- 0 until inputs) {
        val idx = rnd.nextInt(3)+1
        inputPop(j)(i) = inputPop(j)(popSize-idx).burstMutate(burstProb,dist,rnd)
      }
      for (j <- 0 until blocks) {
        val idx = rnd.nextInt(3)+1
        blockPop(j)(i) = blockPop(j)(popSize-idx).burstMutate(burstProb,dist,rnd)
      }
      for (j <- 0 until outputs) {
        val idx = rnd.nextInt(3)+1
        outputPop(j)(i) = outputPop(j)(popSize-idx).burstMutate(burstProb,dist,rnd)
      }
    }
  }
  def complexify(addBlock:Boolean,rnd:Random) : CellPopulationD = {
    if (addBlock) {
      val cPop = new CellPopulationD(inputs,blocks+1,outputs,popSize)
      val mC = blockPop(0)(0).getNumOfCells
      val nIn = new Array[Array[InCellD]](inputs)
      val nB = new Array[Array[CellBlockD]](blocks+1)
      val nOut = new Array[Array[OutCellD]](outputs)
      val oldMid = inputs + blocks*(mC+3)
      
      for (i <- 0 until inputs) {
        nIn(i) = new Array[InCellD](popSize)
        for (j <- 0 until popSize) {
          nIn(i)(j) = inputPop(i)(j).complexify(inputs,blocks,mC,outputs,true,rnd)
          if (rnd.nextDouble < connProb) {
            nIn(i)(j).getForward.addConnection((oldMid+rnd.nextDouble*(mC+3)).toInt,rnd.nextDouble*0.4-0.2)
          }
          if (rnd.nextDouble < connProb/3) {
            nIn(i)(j).getRecurrent.addConnection((oldMid+rnd.nextDouble*(mC+3)).toInt,rnd.nextDouble*0.4-0.2)
          }
        }
      }
      for (i <- 0 until blocks) {
        nB(i) = new Array[CellBlockD](popSize)
        for (j <- 0 until popSize) {
          nB(i)(j) = blockPop(i)(j).complexify(inputs,blocks,mC,outputs,true,rnd)
          for (k <- 0 until mC) {
            if (rnd.nextDouble < connProb/3) {
              nB(i)(j).getRecurrent(k).addConnection((oldMid+rnd.nextDouble*(mC+3)).toInt,rnd.nextDouble*0.4-0.2)
            }
          }
        }
      
      }
      nB(blocks) = new Array[CellBlockD](popSize)
      for (i <- 0 until popSize) {
        val nf = new Array[NeuralConnsD](mC)
        val nr = new Array[NeuralConnsD](mC)
        val mid = inputs + (blocks+1)*(mC+3)
        for (k <- 0 until mC) {
          nf(k) = new NeuralConnsD(mid,mid+outputs)
          nf(k).addRandomConnections(3,rnd)
          nr(k) = new NeuralConnsD(0,mid+outputs)
          nr(k).addRandomConnections(2,rnd)
        }
        nB(blocks)(i) = new CellBlockD(blockPop(0)(0).getBias,nf,nr)
      }
      for (i <- 0 until outputs) {
        nOut(i) = new Array[OutCellD](popSize)
        for (j <- 0 until popSize) {
          nOut(i)(j) = outputPop(i)(j).complexify(inputs,blocks,mC,outputs,true,rnd)
          if (rnd.nextDouble < connProb/3) {
            nOut(i)(j).getRecurrent.addConnection((oldMid+rnd.nextDouble*(mC+3)).toInt,rnd.nextDouble*0.4-0.2)
          }
        }
      }
      cPop.replaceCells(nIn,nB,nOut)
      //println(addBlock+"\n"+cPop)
      cPop
    }
    else {
      val cPop = new CellPopulationD(inputs,blocks,outputs,popSize)
      val mC = blockPop(0)(0).getNumOfCells
      val nIn = new Array[Array[InCellD]](inputs)
      val nB = new Array[Array[CellBlockD]](blocks)
      val nOut = new Array[Array[OutCellD]](outputs)
      val oldMid = inputs+mC
      for (i <- 0 until inputs) {
        nIn(i) = new Array[InCellD](popSize)
        for (j <- 0 until popSize) {
          nIn(i)(j) = inputPop(i)(j).complexify(inputs,blocks,mC,outputs,false,rnd)
          while (rnd.nextDouble < connProb) {
            nIn(i)(j).getForward.addConnection(oldMid+rnd.nextInt(blocks)*(mC+3),rnd.nextDouble*0.4-0.2)
          }
          if (rnd.nextDouble < connProb/3) {
            nIn(i)(j).getRecurrent.addConnection(oldMid+rnd.nextInt(blocks)*(mC+3),rnd.nextDouble*0.4-0.2)
          }
        }
      }
      for (i <- 0 until blocks) {
        nB(i) = new Array[CellBlockD](popSize)
        for (j <- 0 until popSize) {
          nB(i)(j) = blockPop(i)(j).complexify(inputs,blocks,mC,outputs,false,rnd)
          for (k <- 0 until (mC+1)) {
            while (rnd.nextDouble < connProb/3) {
              nB(i)(j).getRecurrent(k).addConnection(oldMid+rnd.nextInt(blocks)*(mC+3),rnd.nextDouble*0.4-0.2)
            }
          }
        }
      }
      for (i <- 0 until outputs) {
        nOut(i) = new Array[OutCellD](popSize)
        for (j <- 0 until popSize) {
          nOut(i)(j) = outputPop(i)(j).complexify(inputs,blocks,mC,outputs,false,rnd)
          while (rnd.nextDouble < connProb/3) {
            nOut(i)(j).getRecurrent.addConnection(oldMid+rnd.nextInt(blocks)*(mC+3),rnd.nextDouble*0.4-0.2)
          }
        }
      }
      cPop.replaceCells(nIn,nB,nOut)
      //println(addBlock+"\n"+cPop)
      cPop
    }
  }
  def update(rnd:Random) : Unit = {

  }
  def getCDF(e:Array[_ <: EvolvableD]) : Array[Double] = {
    var tProbMass = 0.0d
    for (i <- 0 until e.length) {
      tProbMass += e(i).getFitness
    }
    val a = new Array[Double](e.length)
    
    a(0) = e(0).getFitness/tProbMass
    var sum = a(0)
    for (i <- 1 until e.length) {
      sum += e(i).getFitness
      a(i) = sum/tProbMass
    }
    a
  }
  def getIDX(d:Double,cdf:Array[Double]) : Int = {
    for (i <- 0 until cdf.length) {
      if (d < cdf(i)) {
        return i
      }
    }
    return cdf.length-1
  }
  /*
  def repopulateSpecializedCellPopulation(e:Array[Combinable],dist:Distribution,rnd:Random) : Unit = {
    val fArray = getCDF(e)
    val l = e.length
    val clones = new Array[Combinable](l)
    for (i <- 0 until l) {
      clones(i) = e(i)
    }
    for (i <- 0 until l) {
      var idx0 = getIDX(rnd.nextDouble,fArray)
      var idx1 = getIDX(rnd.nextDouble,fArray)
      var counter = 0
      while (idx0 == idx1 && counter < 7) {
        idx1 = getIDX(rnd.nextDouble,fArray)
      }
      e(i) = clones(idx0).combine(clones(idx1),dist,mutProb,flipProb)
    }
  }
  */
  /*Evolves the next generation of this CellPopulation
   * 
   */
  def repopulate(dist:Distribution,mutProb:Double,flipProb:Double,rnd:Random) : Unit = {
    val h = (popSize/2).toInt
    var ipop = new Array[Array[InCellD]](inputs)
    /*
    println("Repopulating following inputCells: ")
    for (i <- 0 until inputs) {
      for (j <- 0 until inputPop(i).length) {
        println(inputPop(i)(j))
      }
    }
    */
    for (i <- 0 until inputs) {
      inputPop(i) = inputPop(i).sortWith(_.getFitness < _.getFitness)
      val nextGen = new Array[InCellD](popSize)
      /*
      for (j <- 0 until popSize) {
        nextGen(j) = inputPop(i)(j).makeClone
      }
      */
      val fArray = getCDF(inputPop(i)) //
      for (k <- 0 until popSize) {
        var idx1 = getIDX(rnd.nextDouble,fArray)
        var idx2 = getIDX(rnd.nextDouble,fArray)
        var counter = 0
        while (idx1 == idx2 && counter < 7) {
          idx2 = getIDX(rnd.nextDouble,fArray)
          counter += 1
        }
        nextGen(k) = inputPop(i)(idx1).combine(inputPop(i)(idx2),dist,mutProb,flipProb)
      }
      inputPop(i) = nextGen
    }
    //var bpop = new Array[Array[CellBlockD]](inputs)
    for (i <- 0 until blocks) {
      blockPop(i) = blockPop(i).sortWith(_.getFitness < _.getFitness)
      val nextGen = new Array[CellBlockD](popSize)
      val fArray = getCDF(blockPop(i))
      for (j <- 0 until popSize) {
        val k = getIDX(rnd.nextDouble,fArray)
        var l = getIDX(rnd.nextDouble,fArray)
        var counter = 0
        while (l == k && counter < 7) {
          l = getIDX(rnd.nextDouble,fArray)
          counter += 1
        }
        nextGen(j) = blockPop(i)(k).combine(blockPop(i)(l),dist,mutProb,flipProb)
      }
      blockPop(i) = nextGen
    }
    //var opop = new Array[Array[OutCellD]](inputs)
    for (i <- 0 until outputs) {
      outputPop(i) = outputPop(i).sortWith(_.getFitness < _.getFitness)
      val nextGen = new Array[OutCellD](popSize)
      val fArray = getCDF(outputPop(i))
      for (j <- 0 until popSize) {
        val k = getIDX(rnd.nextDouble,fArray)
        var l = getIDX(rnd.nextDouble,fArray)
        var counter = 0
        while (l == k && counter < 7) {
          l = getIDX(rnd.nextDouble,fArray)
          counter += 1
        }
        if (counter == 7) {
          nextGen(j) = outputPop(i)(l).burstMutate(0.3,dist,rnd)
        }
        else {
          nextGen(j) = outputPop(i)(k).combine(outputPop(i)(l),dist,mutProb,flipProb)
        }
      }
      outputPop(i) = nextGen
    }
    
  }
  /*
  def setCells(inC:Array[Array[InCellD]],cb:Array[Array[CellBlockD]],outC:Array[Array[OutCellD]]) : Unit = {
    inputPop = inC
    blockPop = cb
    outputPop = outC
  }
  */
  override def toString : String = {
    var srep = "<CellPopulation><InputPop>"
    for (i <- 0 until inputs) {
      for (j <- 0 until popSize) {
        srep += inputPop(i)(j)
      }
    }
    srep += "</InputPop><BlockPop>"
    for (i <- 0 until blocks) {
      for (j <- 0 until popSize) {
        srep += blockPop(i)(j)
      }
    }
    srep += "</BlockPop><OutputPop>"
    for (i <- 0 until outputs) {
      for (j <- 0 until popSize) {
        srep += i+""+j+outputPop(i)(j)
      }
    }
    srep += "</OutputPop></CellPopulation>"
    srep
  }
  def toXML : Elem = {
    //val ops = ArrayOps
    val inpopElems = new Array[Elem](inputs*popSize)
    for (i <- 0 until inputs) { for (j <- 0 until popSize) { inpopElems(i*popSize+j) = inputPop(i)(j).toXML }}
    val blockElems = new Array[Elem](blocks*popSize)
    for (i <- 0 until blocks) { for (j <- 0 until popSize) { blockElems(i*popSize+j) = blockPop(i)(j).toXML }}
    val outpopElems = new Array[Elem](outputs*popSize)
    for (i <- 0 until outputs) { for (j <- 0 until popSize) { outpopElems(i*popSize+j) = outputPop(i)(j).toXML }}
    val tscope = TopScope
    val ip = new Elem(null,"InputPopulation",null,tscope,inpopElems: _*)
    val cp = new Elem(null,"BlockPopulation",null,tscope,blockElems: _*)
    val op = new Elem(null,"OutputPopulation",null,tscope,outpopElems: _*)
    val xml = <CellPopulation>{ip}{cp}{op}</CellPopulation>
    xml
  }
  /*
  def readOutCell(xml:Elem) : OutCellD = {
    val bias = xml \\ "Bias"
    
  }
  */
  //def readPopulation(popString:File) : CellPopulation =
}