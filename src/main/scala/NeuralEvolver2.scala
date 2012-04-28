package neurogenesis.doubleprecision
import neurogenesis.util.SimpleSchedule
import neurogenesis.util.CoolingSchedule
import neurogenesis.util.Distribution
import neurogenesis.util.CauchyDistribution

import scalala.library.random.MersenneTwisterFast
class NeuralEvolver2(cellPop:CellPopulationD,netPop:NetPopulationD,rnd:MersenneTwisterFast,discardRate:Double=0.75) {
  var cellRepopulator:Repopulator[CellPopulationD] = new BasicRepopulator
  var netRepopulator:NetRepopulator[NetPopulationD,CellPopulationD] = new SimpleNetRepopulator
  var schedule: CoolingSchedule = new SimpleSchedule(0.05,0.02,50000)
  val bestNets = new Array[RNND](5)
  val distribution = new CauchyDistribution(0.03)
  def getNet(idx:Int) : RNND = {
    netPop.getRNN(idx)
  }
  def update : Unit = {
    cellRepopulator.repopulate(cellPop,distribution,schedule,rnd,discardRate)
    netRepopulator.repopulate(netPop,cellPop,bestNets,distribution,schedule,rnd,discardRate)
  }
  def setFitness(idx:Int,f:Double) : Unit = {
    netPop.getRNN(idx).setFitness(f)
  }
}