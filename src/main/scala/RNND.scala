package neurogenesis.doubleprecision

import scalala.library.random.MersenneTwisterFast
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.TopScope

import scalala.tensor.dense.DenseMatrix
import scalala.tensor.mutable.Matrix
import edu.uci.ics.jung.graph.SparseGraph
import scalala.library.LinearAlgebra
import edu.uci.ics.jung.graph.util.Pair
import edu.uci.ics.jung.graph.util.EdgeType
import neurogenesis.util.XMLOperator
import neurogenesis.util.Distribution
import neurogenesis.util.CComplexityMeasure
import neurogenesis.util.Constant
import libsvm._
import java.io.File
import java.io.FileWriter
import scala.xml._

class RNND(inputLayer:Array[InCellD],cellBlocks:Array[CellBlockD],outputLayer:Array[OutCellD]) extends EvolvableD {
  val in = inputLayer.length
  val numBlocks = cellBlocks.length
  val blockSize = cellBlocks(0).getSize - 3 //
  val out = outputLayer.length
  //var firstInput = true
  val synapses = cellBlocks(0).getSize //number of inputs the blocks have (gates + memory cells)
  val midPoint = in + numBlocks*synapses //after these only output cells remain
  
  def activate(inputs:Array[Double],actFun:Function1[Double,Double]) : Array[Double] = {
	  val res = new Array[Double](outputLayer.size)
	  //recurrent stimulations based on activations from the previous step
	  recStim
	  //forward connections next
	  val actInput = new Array[Double](inputs.length)
	  for (i <- 0.until(inputs.length)) {
	    inputLayer(i).stimulate(inputs(i))
	    actInput(i) = inputLayer(i).activate(actFun)
	    stimulate(actInput(i),inputLayer(i).getForward)
	  }
	  for (i <- 0.until(numBlocks)) {
	    val actBlock = cellBlocks(i).activate
	    for (j <- 0.until(actBlock.length)) {
	      stimulate(actBlock(j),cellBlocks(i).getForward(j))
	    }
	  }
	  for (i <- 0.until(out)) {
	    res(i) = outputLayer(i).activate(actFun)
	  }
	  
	  res
  }
  /**Adds recurrent stimulations to the network
   * 
   */
  def recStim : Unit = {
    for (i <- 0.until(in)) {
	  stimulate(inputLayer(i).getActivation,inputLayer(i).getRecurrent)
    }
	for (i <- 0.until(numBlocks)) {
	  val acts = cellBlocks(i).activate
	  for (j <- 0.until(acts.length)) {
		stimulate(acts(j),cellBlocks(i).getRecurrent(j))
	  }
	}
	for (i <- 0.until(out)) {
	  stimulate(outputLayer(i).getActivation,outputLayer(i).getRecurrent)
	}
  }
  def evolinoActivate(inputs:Array[Double],actFun:Function1[Double,Double]) : Array[Double] = {
	  val res = new Array[Double](outputLayer.size+numBlocks*blockSize)
	  recStim
	  val actInput = new Array[Double](inputs.length)
	  for (i <- 0.until(inputs.length)) {
	    inputLayer(i).stimulate(inputs(i))
	    actInput(i) = inputLayer(i).activate(actFun)
	    stimulate(actInput(i),inputLayer(i).getForward)
	  }
	  for (i <- 0.until(numBlocks)) {
	    val actBlock = cellBlocks(i).activate
	    for (j <- 0.until(actBlock.length)) {
	      stimulate(actBlock(j),cellBlocks(i).getForward(j))
	    }
	  }
	  for (i <- 0.until(out)) {
	    res(i) = outputLayer(i).activate(actFun)
	  }
	  for (i <- 0 until numBlocks) {
        for (j <- 0 until blockSize) {
          res(out+i*blockSize+j) = cellBlocks(i).memState(j)
        }
      }
	  res
  }
	/*
	def getNet(inCells:Array[InCell],blocks:Array[CellBlock],outCells:Array[OutCell]) : RNN = {
	  val rnn = new RNN(inCells.length,blocks.length,outCells.length)
	  for (i <- 0.until(inCells.length)) {
	    rnn.inputLayer(i) = inCells(i)
	  }
	  for (i <- 0.until(blocks.length)) {
	    rnn.cellBlocks(i) = blocks(i)
	  }
	  for (i <- 0.until(outCells.length)) {
	    rnn.outputLayer(i) = outCells(i)
	  }
	  rnn
	}
	*/
  def burstMutate(prob:Double,dist:Distribution,rnd:MersenneTwisterFast,cellPop:CellPopulationD) : RNND = {
    val il = new Array[InCellD](in)
    for (i <- 0 until in) {
      il(i) = inputLayer(i).burstMutate(prob,dist,rnd)
      il(i).setID(cellPop.updateCounter)
    }
    val bl = new Array[CellBlockD](numBlocks)
    for (i <- 0 until numBlocks) {
      bl(i) = cellBlocks(i).burstMutate(prob,dist,rnd)
      bl(i).setID(cellPop.updateCounter)
    }
    val ol = new Array[OutCellD](out)
    for (i <- 0 until out) {
      ol(i) = outputLayer(i).burstMutate(prob,dist,rnd)
      ol(i).setID(cellPop.updateCounter)
    }
    new RNND(il,bl,ol)
  }
  def getIn(idx:Int) : InCellD = inputLayer(idx)
  def getMid(idx:Int) : CellBlockD = cellBlocks(idx)
  def getOut(idx:Int) : OutCellD = outputLayer(idx)
  
  def combine(net2:RNND,dist:Distribution,mutP:Double,flipP:Double) : RNND = {
    val il = new Array[InCellD](in)
    val bl = new Array[CellBlockD](numBlocks)
    val ol = new Array[OutCellD](out)
    for (i <- 0 until in) {
      il(i) = inputLayer(i).combine(net2.getIn(i),dist,mutP,flipP)
    }
    for (i <- 0 until numBlocks) {
      bl(i) = cellBlocks(i).combine(net2.getMid(i),dist,mutP,flipP)
    }
    for (i <- 0 until out) {
      ol(i) = outputLayer(i).combine(net2.getOut(i),dist,mutP,flipP)
    }
    new RNND(il,bl,ol)
  }
  def combine(net2:RNND,dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double) : RNND = {
    val il = new Array[InCellD](in)
    val bl = new Array[CellBlockD](numBlocks)
    val ol = new Array[OutCellD](out)
    for (i <- 0 until in) {
      il(i) = inputLayer(i).combine(net2.getIn(i),dist,mutP,flipP,rnd,discardRate)
    }
    for (i <- 0 until numBlocks) {
      bl(i) = cellBlocks(i).combine(net2.getMid(i),dist,mutP,flipP,rnd,discardRate)
    }
    for (i <- 0 until out) {
      ol(i) = outputLayer(i).combine(net2.getOut(i),dist,mutP,flipP,rnd,discardRate)
    }
    new RNND(il,bl,ol)
  }
  /*Combines networks with mutation plus sets the cell IDs
   * 
   */
  def combine(net2:RNND,dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast,discardRate:Double,cellpop:CellPopulationD) : RNND = {
    val il = new Array[InCellD](in)
    val bl = new Array[CellBlockD](numBlocks)
    val ol = new Array[OutCellD](out)
    for (i <- 0 until in) {
      il(i) = inputLayer(i).combine(net2.getIn(i),dist,mutP,flipP,rnd,discardRate,cellpop)
    }
    for (i <- 0 until numBlocks) {
      bl(i) = cellBlocks(i).combine(net2.getMid(i),dist,mutP,flipP,rnd,discardRate,cellpop)
    }
    for (i <- 0 until out) {
      ol(i) = outputLayer(i).combine(net2.getOut(i),dist,mutP,flipP,rnd,discardRate,cellpop)
    }
    new RNND(il,bl,ol)
  }
  def distance(net2:RNND) : Double = {
    var di = 0.0
    for (i <- 0 until in) {
      di += inputLayer(i).distance(net2.getIn(i))
    }
    di /= in
    var db = 0.0
    for (i <- 0 until numBlocks) {
      db += cellBlocks(i).distance(net2.getMid(i))
    }
    db /= numBlocks
    var d2 = 0.0
    for (i <- 0 until out) {
      d2 += outputLayer(i).distance(net2.getOut(i))
    }
    d2 /= out
    di+db+d2
  }
  def feedData(inputData:Traversable[Array[Double]],actFun:Function1[Double,Double]) : Array[Array[Double]] = {
    val output = new Array[Array[Double]](inputData.size)
      var idx = 0
      for (in <- inputData) {
        output(idx) = activate(in,actFun)
        idx += 1
      }
    output
    /*
    if (limit > 0) {
      val output = new Array[Array[Double]](limit)
      var idx = 0
      var rows = inputData.slice(0,limit)
      for (r <- rows) {
        output(idx) = activate(r,actFun)
      }
      output
    }
    else {
      val output = new Array[Array[Double]](inputData.size)
      var idx = 0
      for (in <- inputData) {
        output(idx) = activate(in,actFun)
        idx += 1
      }
      output
    }
    */
  }
  def evolinoFeed(inputData:Traversable[Array[Double]],actFun:Function1[Double,Double],washout:Int=100) : DenseMatrix[Double] = {
    val stateSize = out + numBlocks*blockSize
    val dSize = inputData.size-washout
    val mDat = new Array[Double](dSize*stateSize)
    val m = new DenseMatrix(dSize,stateSize,mDat)
    var idx = 0
    
    for (db <- inputData) {
      val od = evolinoActivate(db,actFun)
      if (idx >= washout) {
        for (j <- 0 until stateSize) {
          m.update(idx-washout,j,od(j))
        }
      }
      idx += 1
    }
    m
    
  }
  /*Feeds data and returns results in a format that can be used by libsvm
   * 
   */
  def svmFeed(inputData:Traversable[Array[Double]],actFun:Function1[Double,Double],washout:Int=100) : Array[Array[svm_node]] = {
    val sNodes = new Array[Array[svm_node]](inputData.size-washout)
    var idx = 0
    val stateL = blockSize + out
    for (r <- inputData) {
      val res = evolinoActivate(r,actFun)
      if (idx >= washout) {
        sNodes(idx-washout) = new Array[svm_node](stateL)
        for (i <- 0 until stateL) {
          val node = new svm_node
          node.index = i
          node.value = res(i)
          sNodes(idx-washout)(i) = node
        }
      }
      idx += 1
    }
    sNodes
    //,targetCol:Array[Double]
  }
  def svmRegression(inputData:Traversable[Array[Double]],targetCols:Array[Array[Double]],actFun:Function1[Double,Double],svmParam:svm_parameter,data2:Traversable[Array[Double]],washout:Int=100) : Array[Array[Double]] = {
    val nodes = svmFeed(inputData,actFun,washout)
    //val probs = new Array[svm_problem](targetCols.length)
    val res = Array.ofDim[Double](data2.size,targetCols.length)
    val nodes2 = svmFeed(data2,actFun,0)
    for (i <- 0 until targetCols.length) {
      val prob = new svm_problem
      prob.l = targetCols(i).length
      prob.x = nodes
      prob.y = targetCols(i)

      val svm_model = svm.svm_train(prob,svmParam) //
      for (j <- 0 until nodes2.length) {
        res(j)(i) = svm.svm_predict(svm_model,nodes2(j))
      }
    }
    res
  }
  def linearRegression(inputData:Traversable[Array[Double]],targetMatrix:DenseMatrix[Double],actFun:Function1[Double,Double],washout:Int=100) : DenseMatrix[Double] = {
    
    val X = evolinoFeed(inputData,actFun,washout)
    val Xt = X.t
    val X2 = Xt * X
    try {
      val XXinv = LinearAlgebra.pinv(X2.toDense)
      val X3 = Xt * targetMatrix
      val X4 = XXinv * X3
      X4
    } catch {
      case _ => {
        null
        //val il = inputData.head.length
        //new DenseMatrix(il,targetMatrix.numCols,new Array[Double](il*targetMatrix.numCols))
      }
    }
  }
  def linearPredict(inputData:Traversable[Array[Double]],inputData2:Traversable[Array[Double]],targetMatrix:DenseMatrix[Double],actFun:Function1[Double,Double],washout:Int=100) : Array[Array[Double]] = {
    val M = linearRegression(inputData,targetMatrix,actFun,washout)
    if (M == null) {
      //let's return a zero matrix if a problem occurs in calculating the regression matrix
      Array.ofDim[Double](targetMatrix.numRows,targetMatrix.numCols)
    }
    else {
      val output0 = evolinoFeed(inputData2,actFun,washout)
      val Y = output0 * M
      val res = Array.ofDim[Double](Y.numRows,Y.numCols)
      for (i <- 0 until res.length) {
        for (j <- 0 until res(i).length) {
          res(i)(j) = Y.apply(i,j)
        }
      }
      res
      }
  }
  def evolinoValidate(in2:Traversable[Array[Double]],out2:Traversable[Array[Double]],actFun:Function1[Double,Double],rMatrix:DenseMatrix[Double],washout:Int=100) : Double = {
    var error = 0.0
    val output1 = evolinoFeed(in2,actFun,0)
    val pred = output1 * rMatrix
    var idx = 0
    for (row <- out2) {
      var err2 = 0.0
      for (j <- 0 until row.length) {
        err2 += scala.math.pow(pred.apply(idx,j)-row(j),2)
      }
      error += err2/row.length
      idx += 1
    }
    error/out2.size
  }
  def makeClone : RNND = {
    val il = new Array[InCellD](in)
    val bl = new Array[CellBlockD](numBlocks)
    val ol = new Array[OutCellD](out)
    for (i <- 0 until in) {
      il(i) = inputLayer(i).makeClone
      il(i).setID(inputLayer(i).getID)
    }
    for (i <- 0 until numBlocks) {
      bl(i) = cellBlocks(i).makeClone
      bl(i).setID(cellBlocks(i).getID)
    }
    for (i <- 0 until out) {
      ol(i) = outputLayer(i).makeClone
      ol(i).setID(outputLayer(i).getID)
    }
    val cloned = new RNND(il,bl,ol)
    cloned.copyFitness(fitness)
    cloned
  }
  /**Sets the fitness value for this network and its each individual cell
   * 
   */
  override def setFitness(f:Double,measure:ComplexityMeasure,cBias:Double) : Unit = {
    if (f > 0) {
      for (i <- 0 until in) {
        inputLayer(i).setFitness(f,measure,cBias)
      }
      for (i <- 0 until numBlocks) {
        cellBlocks(i).setFitness(f,measure,cBias)
      }
      for (i <- 0 until out) {
        outputLayer(i).setFitness(f,measure,cBias)
      }
      val f0 = measure.calculateComplexity(gatherConnections,cBias)
      if (f0 > 1) {
        fitness = f/f0
      }
      else {
        fitness = f
      }
    }
    
  }
  def gatherConnections : List[NeuralConnsD] = {
    var clist = List[NeuralConnsD]()
    for (i <- 0 until in) {
      clist = clist ++ inputLayer(i).gatherConnections
    }
    for (i <- 0 until numBlocks) {
      clist = clist ++ cellBlocks(i).gatherConnections
    }
    for (i <- 0 until out) {
      clist = clist ++ outputLayer(i).gatherConnections
    }
    clist
  }
  override def copyFitness(f:Double) : Unit = {
    for (i <- 0 until in) {
      inputLayer(i).copyFitness(f)
    }
    for (i <- 0 until numBlocks) {
      cellBlocks(i).copyFitness(f)
    }
    for (i <- 0 until out) {
      outputLayer(i).copyFitness(f)
    }
    fitness = f
  }
  def stimulate(actVal:Double,conn:NeuralConnsD) : Unit = {
    val cmap = conn.getMap
    for ((dest,(w,expr)) <- cmap) {
      if (expr) {
        if (dest < in) {
          inputLayer(dest).stimulate(w*actVal)
        }
        else if (dest < midPoint) {
          val aux = dest - in
          val numG = aux % synapses
          val numB:Int = aux / synapses
          cellBlocks(numB).stimulate(w*actVal,numG)
	      
        }
        else {
          outputLayer(dest-midPoint).stimulate(w*actVal)
        }
      }
    }
  }
  //TODO: Test which way to add stims is faster
  def stimulate2(actVal:Double,iter:Iterator[(Int,(Double,Boolean))]) : Unit = {
    while (iter.hasNext) {
      val (dest,(w,b)) = iter.next
      if (dest < in) {
        if (b) inputLayer(dest).stimulate(w*actVal)
      }
      else if (dest < midPoint) {
        if (b) {
          val aux = dest - in
          val numG = aux % synapses
          val numB:Int = aux / synapses
          cellBlocks(numB).stimulate(w*actVal,numG)
        }
      }
      else {
        if (b) outputLayer(dest-midPoint).stimulate(w*actVal)
      }
    }
  }

  def reset : Unit = {
    //firstInput = true
    for (incell <- inputLayer) {
      incell.reset
    }
    for (b <- cellBlocks) {
      b.reset
    }
    for (outcell <- outputLayer) {
      outcell.reset
    }
  }
  override def toString : String = {
    var srep = "<RNND>\n"
    for (i <- 0.until(in)) {
      srep += inputLayer(i)+"\n"
    }
    for (i <- 0.until(numBlocks)) {
      srep += cellBlocks(i)+"\n"
    }
    for (i <- 0.until(out)) {
      srep += outputLayer(i)+"\n"
    }
    srep += "</RNND>\n"
    srep
	}
  def toXML : Elem = {
    val inL = new Array[Elem](in)
    for (i <- 0 until in) inL(i) = inputLayer(i).toXML
    val bL = new Array[Elem](numBlocks)
    for (i <- 0 until numBlocks) bL(i) = cellBlocks(i).toXML
    val oL = new Array[Elem](out)
    for (i <- 0 until out) oL(i) = outputLayer(i).toXML
    val tscope = TopScope
    val ip = new Elem(null,"InputLayer",null,tscope,inL: _*)
    val bp = new Elem(null,"BlockLayer",null,tscope,bL: _*)
    val op = new Elem(null,"OutputLayer",null,tscope,oL: _*)
    val res = <RNND><Fitness>{fitness}</Fitness>{ip}{bp}{op}</RNND>
    res
  }
  def write(f:File) : Boolean = {
    if (f.exists) {
      false
    }
    else {
      val rnnxml = toXML
      val fw = new FileWriter(f)
      //println(tag.head)
      scala.xml.XML.write(fw,rnnxml.head,"UTF-8",true,null)
      rnnxml.tail.foreach(e => scala.xml.XML.write(fw,e,"UTF-8",false,null))
      fw.close     
      true
    }
  }
  
  // In order to display the network using JUNG we need to transorm it to its SparseGraph representation
  def toGraph :  SparseGraph[Int,String] = {
    val graph = new SparseGraph[Int,String]()
    val totalNodes = midPoint + out// + numBlocks*3 //gates and all
    var idx = 0
    for (i <- 0 until totalNodes) {
      graph.addVertex(i)
    }
    for (i <- 0 until in) {
      val cnn = inputLayer(i).getForward.conns
      for ((dest,(w,b)) <- cnn) {
        if (b) {
          var ws = w.toString
          if (ws.length > 6) {
            ws = ws.substring(0,6)
          }
          graph.addEdge(idx+"f"+ws,new Pair[Int](i,dest),EdgeType.DIRECTED)
          idx += 1
        }
      }
      val rnn = inputLayer(i).getRecurrent.conns
      for ((dest,(w,b)) <- rnn) {
        if (b) {
          var ws = w.toString
          if (ws.length > 6) {
            ws = ws.substring(0,6)
          }
          graph.addEdge(idx+"r"+ws,new Pair[Int](i,dest),EdgeType.DIRECTED)
          idx += 1
        }
      }
    }
    for (i <- 0 until numBlocks) {
      for (j <- 0 until (synapses-3)) {
        val cnn = cellBlocks(i).getForward(j).conns
        for ((dest,(w,b)) <- cnn) {
          if (b) {
            var ws = w.toString
            if (ws.length > 6) {
              ws = ws.substring(0,6)
            }
            graph.addEdge(idx+"f"+ws,new Pair[Int](in+i*synapses+j,dest),EdgeType.DIRECTED)
            idx += 1
          }
        }
        val cnn2 = cellBlocks(i).getRecurrent(j).conns
        for ((dest,(w,b)) <- cnn2) {
          if (b) {
            var ws = w.toString
            if (ws.length > 6) {
              ws = ws.substring(0,6)
            }
            graph.addEdge(idx+"r"+ws,new Pair[Int](in+i*synapses+j,dest),EdgeType.DIRECTED)
            idx += 1
          }
        }
      }
      
      val nC = cellBlocks(0).getNumOfCells
      for (j <- 0 until 3) {
        if (cellBlocks(i).gateBits(j)) {
          for (k <- 0 until cellBlocks(i).getNumOfCells) {
            val m = in+i*synapses+j+synapses-3
            graph.addEdge(idx+"g"+j+"c"+k,new Pair[Int](in+i*(synapses)+nC+j,in+i*(nC+3)+k),EdgeType.DIRECTED)
            idx += 1
          }
        }
      }
      
    }
    for (i <- 0 until out) {
      val cnn = outputLayer(i).getRecurrent.conns
      for ((dest,(w,b)) <- cnn) {
        if (b) {
          var ws = w.toString
          if (ws.length > 4) {
            ws = ws.substring(0,4)
          }
          graph.addEdge(idx+"r"+ws,new Pair[Int](midPoint+i,dest),EdgeType.DIRECTED)
          idx += 1
        }
      }
    }
    /*
    //Lets add extra nodes connected to inputs to indicate input nodes
    for (i <- (totalNodes-in-out) until (totalNodes-out)) {
      graph.addEdge("input"+(i-totalNodes+out),new Pair[Int](i-in,totalNodes-i-out),EdgeType.DIRECTED)
      idx += 1
    }
    for (i <- midPoint until (midPoint+out)) {
      graph.addEdge("output"+(midPoint+out-i),new Pair[Int](i,totalNodes-out+i-midPoint),EdgeType.DIRECTED)
      idx += 1
    }
    */
    graph
  }
  /*Returns the state of this RNN which includes the activations of
   * output cells and memory cells
   */
  def getState : Array[Double] = {
    val r = new Array[Double](out+numBlocks*blockSize)
    for (i <- 0 until numBlocks) {
      for (j <- 0 until blockSize) {
        r(i*blockSize+j) = cellBlocks(i).memState(j)
      }
    }
    for (i <- midPoint until (midPoint+out)) {
      r(i) = outputLayer(i-midPoint).activation
    }
    r
  }
  def getStateLength : Int = { out+numBlocks*blockSize }
  def getSizes : Array[Int] = {
    val a = new Array[Int](4)
    a(0) = in; a(1) = numBlocks; a(2) = synapses - 3; a(3) = out
    a
  }
  def getFitnessString : String = {
    val fs = fitness.toString
    if (fs.length > 7) {
      fs.substring(0,7)
    }
    else fs
  }
  
}
object RNND {
  /*
  def fromXML(e:Elem) : Unit = {
    val ilElem = e \\ "InputLayer"
    val blElem = e \\ "BlockLayer"
    val olElem = e \\ "OutputLayer"
    val inputs = XMLOperator.customFilter(ilElem,"InCellD")
    val blocks = XMLOperator.customFilter(blElem,"CellBlockD")
    val outputs = XMLOperator.customFilter(olElem,"OutCellD")
    val in = new Array[InCellD](inputs.size)
    var idx = 0
    for (i <- inputs) {
      in(idx) = NeuralOps.fromXML(i)
      println(in(idx).toXML)
      idx += 1
    }
  }
  */
  def fromXML(e:NodeSeq) : RNND = {
    val ilElem = e \\ "InputLayer"
    val blElem = e \\ "BlockLayer"
    val olElem = e \\ "OutputLayer"
    val inputs = ilElem \\ "InCellD"
    val blocks = blElem \\ "CellBlockD"
    val outputs = olElem \\ "OutCellD"
    val in = new Array[InCellD](inputs.size)
    var idx = 0
    for (i <- inputs) {
      in(idx) = InCellD.fromXML(i)
      idx += 1
    }
    val bl = new Array[CellBlockD](blocks.size)
    idx = 0
    for (b <- blocks) {
      bl(idx) = CellBlockD.fromXML(b)
      idx += 1
    }
    val ol = new Array[OutCellD](outputs.size)
    idx = 0
    for (o <- outputs) {
      ol(idx) = OutCellD.fromXML(o)
      idx += 1
    }
    new RNND(in,bl,ol)
  }
}