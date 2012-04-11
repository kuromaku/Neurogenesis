package neurogenesis.doubleprecision
import neurogenesis.util._
import scalala.library.random.MersenneTwisterFast

trait NetRepopulator[T,U] {
  def repopulate(pop:T,pop2:U,bestNets:Array[RNND],dist:Distribution,schedule:CoolingSchedule,rnd:MersenneTwisterFast,discardRate:Double=0.75) : Unit
  def permute(l:Int) : Array[Int] = {
    val idx = new Range(0,l,1).toArray
    for (i <- 0.until(l)) {
      val j = (Math.random*l).toInt
      val aux = idx(i)
      idx(i) = idx(j)
      idx(j) = aux
    }
    idx
  }
  def permutations(w:Int,h:Int) : Array[Array[Int]] = {
    val a = new Array[Array[Int]](h)
    for (i <- 0.until(h)) {
      a(i) = permute(w)
    }
    a
  }
  def bestReady(bn:Array[RNND]) : Boolean = {
    bn.forall(_ != null)
  }
}