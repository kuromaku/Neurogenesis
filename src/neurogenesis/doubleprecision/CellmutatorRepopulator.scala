package neurogenesis.doubleprecision

import neurogenesis.util.Distribution
import scalala.library.random.MersenneTwisterFast
import neurogenesis.util.CoolingSchedule

/*Instead of combining cells to form a new population this just mutates the best existing cells
 * in the old population to produce the next generation
 */
class CellmutatorRepopulator(deathRate:Double) extends Repopulator[CellPopulationD] {

  def repopulate(pop: CellPopulationD, dist: Distribution, schedule: CoolingSchedule, rnd: MersenneTwisterFast, discardRate: Double) : CellPopulationD = {
    val mutProb = schedule.getProb1
    val flipProb = schedule.getProb2
    val popSize = pop.getSize
    val h = (deathRate*popSize).toInt
    val inputs = pop.getIn
    val inputPop = pop.getInPop
    val diff = popSize - h
    for (i <- 0 until inputs) {
      inputPop(i) = inputPop(i).sortWith(_.getFitness < _.getFitness)
      val nextGen = new Array[InCellD](popSize)
      for (k <- 0 until popSize) {
        nextGen(k) = inputPop(i)(h+rnd.nextInt(diff)).burstMutate(mutProb,dist,rnd)
      }
      inputPop(i) = nextGen
    }
    val blocks = pop.getBlocks
    val blockPop = pop.getBlockPop
    for (i <- 0 until blocks) {
      blockPop(i) = blockPop(i).sortWith(_.getFitness < _.getFitness)
      val nextGen = new Array[CellBlockD](popSize)
      for (j <- 0 until popSize) {
        nextGen(j) = blockPop(i)(h+rnd.nextInt(diff)).burstMutate(mutProb,dist,rnd)
      }
      blockPop(i) = nextGen
    }
    val outputs = pop.getOut
    val outputPop = pop.getOutPop
    for (i <- 0 until outputs) {
      outputPop(i) = outputPop(i).sortWith(_.getFitness < _.getFitness)
      val nextGen = new Array[OutCellD](popSize)
      for (j <- 0 until popSize) {
        nextGen(j) = outputPop(i)(h+rnd.nextInt(diff)).burstMutate(mutProb,dist,rnd)
      }
      outputPop(i) = nextGen
    }
    val neoPop = new CellPopulationD(inputs,blocks,outputs,popSize)
    neoPop.replaceCells(inputPop,blockPop,outputPop)
    neoPop
  }
  override def toString : String = "MutatorRepopulator"

}