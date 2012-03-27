package neurogenesis.doubleprecision
import scala.xml.Elem
import scala.xml.Node
import scalala.tensor.dense.DenseMatrix

import scalala.tensor.mutable.Matrix
import scalala.library.LinearAlgebra
import scalala.library.Plotting
import scala.swing.TextArea

package object NeuralOps {
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
  def runLinearRegression(d1:List[Array[Double]],d2:List[Array[Double]],d3:List[Array[Double]],d4:List[Array[Double]],rArea:TextArea) : Unit = {
    val m1 = list2Matrix(d1)
    val m2 = list2Matrix(d2)
    
    val mt = m1.t
    val mt2 = mt * m1
    try {
      val mInv = LinearAlgebra.pinv(mt2.toDense)
      rArea.append("Calculated the pseudo-inverse.\n")
      val m3 = mt * m2
      val m4 = mInv * m3
    
      val m5 = list2Matrix(d3)
      val res = m5 * m4
    
      val rows = res.numRows
      val cols = res.numCols
      Plotting.plot(m5.apply(new Range(0,rows,1),0),res.apply(new Range(0,rows,1),0))
      Plotting.hold(true)
      for (i <- 1 until cols) {
        Plotting.subplot(cols+1,1,i+1)
        Plotting.plot(m5.apply(new Range(0,rows,1),i),res.apply(new Range(0,rows,1),i))
        Plotting.hold(true)
      }
      Plotting.hold(false)
      
    //Plotting.hold(true)
    } catch {
      case _ => rArea.append("Could not complete linear regression do to a singular inversion matrix.")
    }
  }
}