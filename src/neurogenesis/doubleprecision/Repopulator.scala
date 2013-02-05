package neurogenesis.doubleprecision
import neurogenesis.util._
import scalala.library.random.MersenneTwisterFast

trait Repopulator[T] {
  var ctype: String = "Basic"
  
  def createConnections(min:Int,max:Int,mval:Double,cnum:Int,crnd:MersenneTwisterFast) : AbstractNeuralconnections = {
    var nc: AbstractNeuralconnections = {
    ctype match {
      case "Rigid" => new RigidNeuralConnections(min,max,mval)
      case "Basic" => new NeuralConnsD(min,max,mval)
      case _ => null
    }
    }
    nc.addRandomConnections(cnum,crnd)
    nc
    
  }  
  def setConnectionType(t:String) : Unit = { ctype = t }
  
  def repopulate(pop:T,dist:Distribution,schedule:CoolingSchedule,rnd:MersenneTwisterFast,discardRate:Double=0.75) : T
}