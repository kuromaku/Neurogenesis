package neurogenesis.doubleprecision
import neurogenesis.util.Distribution
import neurogenesis.util.CoolingSchedule
import scalala.library.random.MersenneTwisterFast
/*
 * Same as ComplexRepopulator but this one introduces one new random cell into each 
 * subpopulation in each generation
 * to add diversity
 */
class RandCellRepopulator(deathRate:Double) extends Repopulator[CellPopulationD] {
  def repopulate(pop: CellPopulationD, dist: Distribution, schedule: CoolingSchedule, rnd: MersenneTwisterFast,discardRate:Double=0.75): CellPopulationD = { 
    val mutProb = schedule.getProb1
    val flipProb = schedule.getProb2
    val popSize = pop.getSize
    val h = (deathRate*popSize).toInt
    val inputs = pop.getIn
    val inputPop = pop.getInPop
    val minNode1 = inputPop(0)(0).getForward.getMin
    val maxNode1 = inputPop(0)(0).getForward.getMax
    val numNew = 5
    
    for (i <- 0 until inputs) {
      inputPop(i) = inputPop(i).sortWith(_.getFitness < _.getFitness)
      val nextGen = new Array[InCellD](popSize)
      val fArray = pop.getCDF(inputPop(i)) //
      for (k <- 1 until h) {
        var idx1 = pop.getIDX(rnd.nextDouble,fArray)
        var idx2 = pop.getIDX(rnd.nextDouble,fArray)
        var counter = 0
        while (idx1 == idx2 && counter < 7) {
          idx2 = pop.getIDX(rnd.nextDouble,fArray)
          counter += 1
        }
        nextGen(k) = inputPop(i)(idx1).combine(inputPop(i)(idx2),dist,mutProb,flipProb,rnd,discardRate)
      }
      val rf = new NeuralConnsD(minNode1,maxNode1)
      rf.addRandomConnections(numNew,rnd)
      val rf2 = new NeuralConnsD(0,maxNode1)
      rf2.addRandomConnections((scala.math.random*2).toInt,rnd)
      nextGen(0) = new InCellD(rf,rf2)
      for (k <- h until popSize) {
        nextGen(k) = inputPop(i)(k)
      }
      inputPop(i) = nextGen
    }
    val blocks = pop.getBlocks
    val blockPop = pop.getBlockPop
    val minNode2 = blockPop(0)(0).getForward(0).getMin
    val maxNode2 = maxNode1
    val mcells = blockPop(0)(0).getNumOfCells
    val blockBias = blockPop(0)(0).getBias
    for (i <- 0 until blocks) {
      blockPop(i) = blockPop(i).sortWith(_.getFitness < _.getFitness)
      val nextGen = new Array[CellBlockD](popSize)
      val fArray = pop.getCDF(blockPop(i))
      for (j <- 1 until h) {
        val k = pop.getIDX(rnd.nextDouble,fArray)
        var l = pop.getIDX(rnd.nextDouble,fArray)
        var counter = 0
        while (l == k && counter < 7) {
          l = pop.getIDX(rnd.nextDouble,fArray)
          counter += 1
        }
        nextGen(j) = blockPop(i)(k).combine(blockPop(i)(l),dist,mutProb,flipProb,rnd,discardRate)
      }
      val rf = new Array[NeuralConnsD](mcells)
      val rf2 = new Array[NeuralConnsD](mcells)
      for (j <- 0 until mcells) {
        rf(j) = new NeuralConnsD(minNode2,maxNode2)
        rf(j).addRandomConnections(numNew-1,rnd)
        rf2(j) = new NeuralConnsD(0,maxNode2)
        rf2(j).addRandomConnections((scala.math.random*2).toInt,rnd)
      }
      nextGen(0) = new CellBlockD(blockBias,rf,rf2)
      for (j <- h until popSize) {
        nextGen(j) = blockPop(i)(j)
      }
      blockPop(i) = nextGen
    }
    val outputs = pop.getOut
    val outputPop = pop.getOutPop
    val outBias = outputPop(0)(0).getBias
    for (i <- 0 until outputs) {
      outputPop(i) = outputPop(i).sortWith(_.getFitness < _.getFitness)
      val nextGen = new Array[OutCellD](popSize)
      val fArray = pop.getCDF(outputPop(i))
      for (j <- 1 until h) {
        val k = pop.getIDX(rnd.nextDouble,fArray)
        var l = pop.getIDX(rnd.nextDouble,fArray)
        var counter = 0
        while (l == k && counter < 7) {
          l = pop.getIDX(rnd.nextDouble,fArray)
          counter += 1
        }
        if (counter == 7) {
          nextGen(j) = outputPop(i)(l).burstMutate(0.3,dist,rnd)
        }
        else {
          nextGen(j) = outputPop(i)(k).combine(outputPop(i)(l),dist,mutProb,flipProb,rnd,discardRate)
        }
      }
      val rf = new NeuralConnsD(0,maxNode2)
      rf.addRandomConnections((scala.math.random*2).toInt,rnd)
      nextGen(0) = new OutCellD(outBias,rf)
      for (j <- h until popSize) {
        nextGen(j) = outputPop(i)(j)
      }
      outputPop(i) = nextGen
    }
    
    val neoPop = new CellPopulationD(inputs,blocks,outputs,popSize)
    neoPop.replaceCells(inputPop,blockPop,outputPop)
    neoPop
  }
  override def toString : String = "RandCellRepopulator"
}