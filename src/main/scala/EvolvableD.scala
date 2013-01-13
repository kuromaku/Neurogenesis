package neurogenesis.doubleprecision
import scala.util.Random
import neurogenesis.util.CComplexityMeasure

trait EvolvableD {
  //type T
  var fitness:Double = 0
  var id:Long = 0
  def getFitness : Double = fitness
  def getID = id
  def setID(l:Long) : Unit = { id = l }
  def setFitness(f:Double,measure:ComplexityMeasure,cBias:Double) : Unit = { fitness = f }
  def copyFitness(f:Double) : Unit = { fitness = f }
  def makeClone : EvolvableD
  
  def sort[T <: EvolvableD](a:Array[T]) : Array[T] = {
    a.sortWith((_.getFitness < _.getFitness))
  }
  def insertionSort[T <: EvolvableD](a:Array[T]) : Array[T] = {
    var i = 0
    for (j <- 1 until a.length) {
      val f = a(j).getFitness
      val key = a(j)
      i = j - 1
      while (i >= 0 && a(i).getFitness > f) {
        a(i+1) = a(i)
        i = i - 1
      }
      a(i+1) = key
    }
    a
  }
}