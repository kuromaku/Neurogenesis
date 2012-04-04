package neurogenesis.doubleprecision

import scala.xml.Elem
import scala.xml.Node
import scalala.tensor.dense.DenseMatrix
import scalala.generic.collection.CanViewAsTensor1._
import scalala.tensor.mutable.Matrix
import scalala.library.LinearAlgebra
import scalala.library.Plotting
import scala.swing.TextArea
import neurogenesis.util.XMLOperator

object NeuralOps {
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
  def fromXML(elem:Node) : InCellD = {
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
  def array2Matrix(a:Array[Array[Double]]) : DenseMatrix[Double] = {
    val mDat = new Array[Double](a.length*a(0).length)
    for (i <- 0 until a.length) {
      for (j <- 0 until a(i).length) {
        mDat(i*a(0).length+j) = a(i)(j)
      }
    }
    new DenseMatrix[Double](a.length,a(0).length,mDat)
  }
  def list2Matrix(l:List[Array[Double]]) : DenseMatrix[Double] = {
    val mDat = new Array[Double](l.size*l.head.length)
    var idx = 0
    for (l0 <- l) {
      for (j <- 0 until l0.length) {
        mDat(idx*l0.length+j) = l0(j)
      }
      idx += 1
    }
    new DenseMatrix[Double](l.size,l.head.length,mDat)
  }
  def matrix2List(m:DenseMatrix[Double]) : List[Array[Double]] = {
    var l = List[Array[Double]]()
    for (i <- 0 until m.numRows) {
      val a = new Array[Double](m.numCols)
      for (j <- 0 until m.numCols) {
        a(j) = m.apply(i,j)
      }
      l = l.:+(a)
    }
    
    l
  }
  def totalError(l1:List[Array[Double]],l2:List[Array[Double]]) : Double = {
    var error = 0.0
    var l0 = l2
    for (l <- l1) {
      error += squaredError(l,l0.head)
      l0 = l0.tail
    }
    error
  }
  def squaredError(a1:Array[Double],a2:Array[Double]) : Double = {
    var error = 0d
    for (i <- 0 until a1.length) {
      error += Math.sqrt(Math.pow(a2(i)-a1(i),2))
    }
    error = error
    error
  }
  def runLinearRegression(m1:DenseMatrix[Double],d2:List[Array[Double]],m2:DenseMatrix[Double],d4:List[Array[Double]],rArea:TextArea) : Unit = {

    //Regression formula B=(X^t * X)^-1 * X^t * Y  X*B=Y
    val mt = m1.t
    val mt2 = mt * m1
    try {
      val mInv = LinearAlgebra.pinv(mt2.toDense)
      rArea.append("Calculated the pseudo-inverse.\n")
      val m3 = mt * list2Matrix(d2)
      val B = mInv * m3
      
      val res = m2 * B
      val rows = res.numRows
      val cols = res.numCols
      val range = 0 until rows
      val m5 = list2Matrix(d4)
      
      val points = new Array[Int](rows)
      for (i <- 0 until rows) { points(i) = i}
      val p = ArrayI.apply(points)
      Plotting.subplot(cols,1,1)
      Plotting.plot(p,res.apply(range,0),'-',"b")
      Plotting.hold(true)
      Plotting.plot(p,m5.apply(range,0),'-',"r")
      Plotting.title("Column 1")
      Plotting.hold(false)
      for (i <- 1 until cols) {
        Plotting.subplot(cols,1,i+1)
        Plotting.plot(p,res.apply(range,i),'-',"b")
        Plotting.hold(true)
        Plotting.plot(p,m5.apply(range,i),'-',"r")
        Plotting.title("Column "+(i+1))
        Plotting.hold(false)
      }
      rArea.append("B:\n"+B.toString+"\n")
      rArea.append("Res:\n"+res.toString+"\n")
      rArea.append("Target:\n"+m5.toString+"\n")
    } catch {
      case _ => rArea.append("Could not complete linear regression because of a singular inversion matrix.")
    }
  }
}