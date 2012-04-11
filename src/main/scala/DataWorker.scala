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
class DataWorker(reporter:ProgressReporter,worker:InterfaceWorker,autoNormalize:Boolean) extends Actor {
	var data = List[List[Array[Double]]]()
	var counter = 0
	var normalizers = List[Array[(Double,Double)]]()
	var dimensionsAgree = true
	var normalize = autoNormalize
	def getCount : Int = counter
	def getData(idx:Int) : List[Array[Double]] = data.apply(idx)
	def normalizeData(b:Boolean) : Unit = { normalize = b }
	
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
	  counter = 0
	}
}