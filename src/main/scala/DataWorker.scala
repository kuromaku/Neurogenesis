package neurogenesis.util
import neurogenesis.msg.LoadData
import neurogenesis.msg.ProgressMessage
import neurogenesis.msg.AnotherDataList
import neurogenesis.msg.AnotherArray
import scala.swing.Frame
import scala.io.Source
import scala.actors.Actor
import java.io._
import scala.collection.mutable.ArrayOps
import java.util.Scanner
import scalala.library.Storage
import scalala.tensor.dense.DenseMatrix
import scalala.generic.collection.CanViewAsTensor1._
import scalala.tensor.dense.DenseVectorCol
import scalala.library.Plotting
import libsvm.svm_node

class DataWorker(reporter:ProgressReporter,worker:InterfaceWorker,autoNormalize:Boolean) extends Actor {
	var data = List[List[Array[Double]]]()
	var counter = 0
	var normalizers = List[Array[(Double,Double)]]()
	var extVals = List[List[(Double,Double)]]()
	var normalized = false
	var dimensionsAgree = true
	var normalize = autoNormalize
	var svmNodes = List[Array[Array[svm_node]]]() //null //used with libsvm
	
	var svmCols = List[Array[Array[Double]]]()
	var svmReady = false
	
	def getCount : Int = counter
	def getData(idx:Int) : List[Array[Double]] = data.apply(idx)
	def normalizeData(b:Boolean) : Unit = { normalize = b }
	def getNodes(idx:Int) = svmNodes.apply(idx)
	def getCols(idx:Int) = svmCols.apply(idx)
	
	def checkData : Unit = {
	  if (counter == 2) {
	    if (data.head(0).length == data.tail.head(0).length) {
	    dimensionsAgree = true
	    }
	    else {
	      dimensionsAgree = false
	    }
	  }
	  if (counter == 4) {
	    if ((data.head(0).length == data.tail.head(0).length) && (data.apply(1)(0).length == data.apply(3)(0).length)) {
	      dimensionsAgree = true
	    }
	    else {
	      dimensionsAgree = false
	    }
	  }
	}
	def getDLists : List[List[Array[Double]]] = data
	
	def getDims : Array[Int] = {
	  val dims = new Array[Int](2)
	  dims(0) = data.head(0).length
	  dims(1) = data.tail.head(0).length
	  dims
	}
	def act : Unit = {
	  loop {
	    react {
	      case LoadData(files) => {//
	        for (file <- files) {
	          if (file.exists && file.canRead()) {
	            readMatrix(file)
	            if (counter > 1)
	              reporter ! ProgressMessage("Dataworker finished reading a new data array from file "+file.getPath())
	            else
	              reporter ! ProgressMessage("Dataworker finished reading the first data array from file "+file.getPath())
	            worker ! AnotherDataList(data.apply(counter-1))
	          }
	        }
	      }
	      case AnotherArray(arr) => { //Received after calculating the validation results
	        val cols = arr(0).length
	        Plotting.title("Validation results vs. target data")
	        for (i <- 0 until cols) {
	          val (x1,y1) = getAsTensors(3,i)
	          Plotting.subplot(cols,1,i+1)
	          Plotting.plot(x1,y1,'-',"b")
	          Plotting.hold(true)
	          val (x2,y2) = getAsTensors(arr,i)
	          Plotting.plot(x2,y2,'-',"r")
	          Plotting.hold(false)
	        }
	      }
	    }
	  }
	}
	/*Reads a matrix using the scalala library method and
	 * if normalization is in use calculates some necessary values for it
	 */
	def readMatrix(src:File) : Unit = {
	  val fileIn = new BufferedInputStream(new FileInputStream(src))
	  val mat = Storage.loadtxt(fileIn)
	  fileIn.close
	  if (normalize) {
	    val a = new Array[Array[Double]](mat.numRows)
	    for (i <- 0 until a.length) {
	      a(i) = new Array[Double](mat.numCols)
	    }
	    val norm = new Array[(Double,Double)](mat.numCols)
	    var colExt = List[(Double,Double)]()
	    for (i <- 0 until mat.numCols) {
	      val v = mat.apply(new Range(0,mat.numRows,1),i)
	      val ext = (v.apply(v.argmin),v.apply(v.argmax))
	      colExt = colExt.:+(ext)
	      for (j <- 0 until v.length) {
	        a(j)(i) = v.apply(j)
	      }
	    }
	    extVals = extVals.:+(colExt)
	    data = data :+(a.toList)
	    counter += 1
	  }
	  else {
	    matrix2List(mat)
	    counter += 1
	  }
	}
	def readMatrix2(src:File) : Unit = {
	  val fileIn = new BufferedInputStream(new FileInputStream(src))
	  val mat = Storage.loadtxt(fileIn)
	  fileIn.close
	  if (normalize) {
	    val a = new Array[Array[Double]](mat.numRows)
	    for (i <- 0 until a.length) {
	      a(i) = new Array[Double](mat.numCols)
	    }
	    val norm = new Array[(Double,Double)](mat.numCols)
	    for (i <- 0 until mat.numCols) {
	      val v = mat.apply(new Range(0,mat.numRows,1),i)
	      val dMin = v.apply(v.argmin)
	      val dMax = v.apply(v.argmax)
	      //println("Min: "+dMin+" Max: "+dMax)
	      val width = dMax - dMin
	      val mean = (dMax+dMin)
	      //val f = (x:Double) => -(x-mean)/width
	      for (j <- 0 until v.length) {
	        a(j)(i) = (2*v.apply(j)-mean)/width
	      }
	      norm(i) = (mean,width)
	    }
	    normalizers = normalizers.:+(norm)
	    data = data :+(a.toList)
	    counter += 1
	  }
	  else {
	    matrix2List(mat)
	    counter += 1
	  }
	}
	/*Normalizes all loaded data arrays so that the values lie between the range -1,1. 
	 *Should be used only when all the data has been loaded into memory.
	*/
	def normalizeAll : Unit = {
	  if (!normalized) {
	  val size = data.size
	  val cols1 = data.apply(0).apply(0).length
	  val cols2 = data.apply(1).apply(0).length
	  val amax1 = new Array[Double](cols1)
	  val amin1 = new Array[Double](cols1)
	  var idx = 0
	  while (idx < (size-1)) {
	    val d = extVals.apply(idx)
	    for (j <- 0 until cols1) {
	      if (amin1(j) > d.apply(j)._1) {
	        amin1(j) = d.apply(j)._1
	      }
	      if (amax1(j) < d.apply(j)._2) {
	        amax1(j) = d.apply(j)._2
	      }
	    }
	    idx += 2
	  }
	  val amax2 = new Array[Double](cols2)
	  val amin2 = new Array[Double](cols2)
	  idx = 1
	  while (idx < size) {
	    val d = extVals.apply(idx) 
	      for (j <- 0 until cols2) {
	        if (amin2(j) > d.apply(j)._1) {
	          amin2(j) = d.apply(j)._1
	        }
	        if (amax2(j) < d.apply(j)._2) {
	          amax2(j) = d.apply(j)._2
	        }
	      }
	    idx += 2
	  }
	  idx = 0
	  val wa = new Array[Double](cols1)
	  val ma = new Array[Double](cols1)
	  for (i <- 0 until cols1) { wa(i) = amax1(i)-amin1(i); ma(i) = amax1(i)+amin1(i) }
	  
	  while (idx < (size-1)) {
	    val d = data.apply(idx)
	    for (row <- d) {
	      for (j <- 0 until cols1) {
	        row(j) = (2*row(j)-ma(j))/wa(j)
	      }
	    }
	    idx += 2
	  }
	  idx = 1
	  val wa2 = new Array[Double](cols2)
	  val ma2 = new Array[Double](cols2)
	  for (i <- 0 until cols2) { wa2(i) = amax2(i)-amin2(i); ma2(i) = amax2(i)+amin2(i) }
	  while (idx < size) {
	    val d = data.apply(idx)
	    for (row <- d) {
	      for (j <- 0 until cols2) {
	        row(j) = (2*row(j)-ma2(j))/wa2(j)
	      }
	    }
	    idx += 2
	  }
	  normalized = true
	  }
	}
	/*
	def matrix2Array(mtr:DenseMatrix[Double]) : Unit = {
	  val arr = new Array[Array[Double]](mtr.numRows)
	  val nc = mtr.numCols
	  for (i <- 0 until mtr.numRows) {
	    arr(i) = new Array[Double](nc)
	    for (j <- 0 until nc) {
	      arr(i)(j) = mtr.apply(i,j)
	    }
	  }
	  data = data :+(arr)
	  counter += 1
	}
	*/
	def matrix2List(mtr:DenseMatrix[Double]) : Unit = {
	  var dl = List[Array[Double]]()
	  val nc = mtr.numCols
	  for (i <- 0 until mtr.numRows) {
	    val a = new Array[Double](nc)
	    for (j <- 0 until nc) {
	      a(j) = mtr.apply(i,j)
	    }
	    dl = dl.:+(a)
	  }
	  data = data.:+(dl)
	}
	def printData(idx:Int) : Unit = {
	  println("Data size is: "+data.size)
	  val d = data.apply(idx)
	  for (i <- 0 until d.size) {
	    for (j <- 0 until d(i).size) {
	      print(d(i)(j)+" ")
	    }
	    print("\n")
	  }
	}
	override
	def toString : String = {
	  val sb = new StringBuilder
	  for (i <- 0 until data.size) {
	    for (j <- 0 until data(i).size) {
	      sb.append(data(i)(j)+" ")
	    }
	    sb.append("\n")
	  }
	  sb.toString()
	}
	def getAsString(idx:Int,lines:Int) : String = {
	  
	  val d = data.apply(idx)
	  val sb = new StringBuilder("Displaying "+lines+" out of "+d.size+" lines.\n")
	  var max = 0
	  if (lines < d.size) {
	    max = lines
	  }
	  else {
	    max = d.size
	  }
	  for (i <- 0 until max) {
	    for (j <- 0 until d(i).size) {
	      sb.append(d(i)(j)+" ")
	    }
	    sb.append("\n")
	  }
	  sb.toString()
	}
	def getAsTensors(idx:Int,c:Int) : (DenseVectorCol[Int],DenseVectorCol[Double]) = {
	  val a = data.apply(idx)
	  val rng = new Array[Int](a.length)
	  for (i <- 0 until rng.length) {
	    rng(i) = i
	  }
	  val ds = new Array[Double](a.length)
	  for (i <- 0 until a.length) {
	    ds(i) = a(i)(c)
	  }
	  (ArrayI.apply(rng),ArrayD.apply(ds))
	}
    def getAsTensors(idx:Int,c:Int,maxSize:Int) : (DenseVectorCol[Int],DenseVectorCol[Double]) = {
	  val a = data.apply(idx)
	  val asize = a.size
	  val msize = if (asize > maxSize) maxSize else asize
	  val stepSize = if (asize > maxSize) asize/maxSize else 1
	  val rng = new Array[Int](msize)
	  for (i <- 0 until rng.length) {
	    rng(i) = i*stepSize
	  }
	  val ds = new Array[Double](msize)
	  for (i <- 0 until msize) {
	    ds(i) = a(i*stepSize)(c)
	  }
	  (ArrayI.apply(rng),ArrayD.apply(ds))
	}
    def getAsTensors(da:Array[Array[Double]],c:Int) : (DenseVectorCol[Int],DenseVectorCol[Double]) = {
	  
	  val rng = new Array[Int](da.length)
	  for (i <- 0 until rng.length) {
	    rng(i) = i
	  }
	  val ds = new Array[Double](da.length)
	  for (i <- 0 until da.length) {
	    ds(i) = da(i)(c)
	  }
	  (ArrayI.apply(rng),ArrayD.apply(ds))
	}
	def plotColumns : Unit = {
	  val a = data.apply(counter-1)
	  for (i <- 0 until a(0).length) {
	    val (x,y) = getAsTensors(counter-1,i)
	    val k = i % 6
	    k match {
	      case 0 => Plotting.plot(x,y,'-',"r")
	      case 1 => Plotting.plot(x,y,'-',"b")
	      case 2 => Plotting.plot(x,y,'-',"B")
	      case 3 => Plotting.plot(x,y,'-',"y")
	      case 4 => Plotting.plot(x,y,'-',"c")
	      case 5 => Plotting.plot(x,y,'-',"k")
	      case _ => Plotting.plot(x,y,'-',"m")
	    }
	    Plotting.hold(true)
	  }
	  Plotting.hold(false)
	}
	def makeSubplots : Unit = {
	  val a = data.apply(counter-1)
	  val l = a(0).length
	  
	  
	  for (i <- 0 until l) {
	    Plotting.subplot(l,1,i+1)
	    val (x,y) = getAsTensors(counter-1,i,1000)
	    val k = i % 6
	    k match {
	      case 0 => Plotting.plot(x,y,'-',"r")
	      case 1 => Plotting.plot(x,y,'-',"b")
	      case 2 => Plotting.plot(x,y,'-',"B")
	      case 3 => Plotting.plot(x,y,'-',"y")
	      case 4 => Plotting.plot(x,y,'-',"c")
	      case 5 => Plotting.plot(x,y,'-',"k")
	      case _ => Plotting.plot(x,y,'-',"m")
	    }
	    Plotting.hold(true)
	    Plotting.title("column: "+i)
	    Plotting.hold(false)
	  }
	}
   def makeSubplots(idx:Int,frame:Frame) : Unit = {
      frame.close
	  val a = data.apply(idx)
	  val l = a(0).length
	  for (i <- 0 until l) {
	    Plotting.subplot(l,1,i+1)
	    val (x,y) = getAsTensors(idx,i,1000)
	    val k = i % 6
	    k match {
	      case 0 => Plotting.plot(x,y,'-',"r")
	      case 1 => Plotting.plot(x,y,'-',"b")
	      case 2 => Plotting.plot(x,y,'-',"B")
	      case 3 => Plotting.plot(x,y,'-',"y")
	      case 4 => Plotting.plot(x,y,'-',"c")
	      case 5 => Plotting.plot(x,y,'-',"k")
	      case _ => Plotting.plot(x,y,'-',"m")
	    }
	    Plotting.hold(true)
	    Plotting.title("column: "+i)
	    Plotting.hold(false)
	  }
	}
	def removeAllData : Unit = { 
	  data = List[List[Array[Double]]]()
	  normalizers = List[Array[(Double,Double)]]()
	  svmNodes = List[Array[Array[svm_node]]]()
	  svmCols = List[Array[Array[Double]]]()
	  svmReady = false
	  counter = 0
	}
	def data2svmformat(idx:Int) : Unit = {
	  val d = data.apply(idx)
	  val t = data.apply(idx+1)
	  var i = 0
	  //val size = t.length
	  val cols = d(0).length
	  val svmNodes2 = new Array[Array[svm_node]](d.size)
	  for (row <- d) {
	    svmNodes2(i) = new Array[svm_node](cols)
	    for (j <- 0 until cols) {
	      val node = new svm_node
	      node.index = j + 1
	      node.value = row(j)
	      svmNodes2(i)(j) = node
	    }
	    i += 1
	  }
	  i = 0
	  val l = t.size
	  val svmCol = Array.ofDim[Double](t(0).length,l)
	  
	  for (row <- t) {
	    for (j <- 0 until row.length) {
	      svmCol(j)(i) = row(j)
	    }
	    i += 1
	  }
	  svmNodes = svmNodes.+:(svmNodes2)
	  svmCols = svmCols.+:(svmCol)
	}
	def initSVM : Unit = {
	  var i = 0
	  while (i < counter-1) {
	    data2svmformat(i)
	    i += 2
	  }
	  svmNodes = svmNodes.reverse
	  svmCols = svmCols.reverse
	  svmReady = true
	}
}