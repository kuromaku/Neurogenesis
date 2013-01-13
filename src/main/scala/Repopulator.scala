package neurogenesis.doubleprecision
import neurogenesis.util._
import scalala.library.random.MersenneTwisterFast

trait Repopulator[T] {
  def repopulate(pop:T,dist:Distribution,schedule:CoolingSchedule,rnd:MersenneTwisterFast,discardRate:Double=0.75) : T
}