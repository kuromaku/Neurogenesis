package neurogenesis.doubleprecision
import neurogenesis.util._
import scala.util.Random

trait Repopulator[T] {
  def repopulate(pop:T,dist:Distribution,schedule:CoolingSchedule,rnd:Random) : Unit
}