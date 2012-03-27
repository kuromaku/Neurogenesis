package neurogenesis.doubleprecision
import scala.util.Random
trait EvolvableD {
  //type T
  var fitness:Double = 0
  
  def getFitness : Double = fitness
  def setFitness(f:Double) : Unit = { fitness = f }
  def makeClone : EvolvableD
  //def combine[T <: Evolvable](e2:T,dist:Distribution,mutP:Double,flipP:Double) : T
  //def combine(e2:Evolvable,dist:Distribution,mutP:Double,flipP:Double) : Evolvable
  //def complexify [T <: Evolvable] (in:Int,blocks:Int,memCells:Int,out:Int,addBlock:Boolean) : T
  //def burstMutate(prob:Double,dist:Distribution,rnd:Random) : Evolvable
  def sort[T <: EvolvableD](a:Array[T]) : Array[T] = {
    val sorted = a.sortWith((_.getFitness < _.getFitness))
    sorted
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