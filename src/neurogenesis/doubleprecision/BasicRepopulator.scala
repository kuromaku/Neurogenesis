package neurogenesis.doubleprecision
import neurogenesis.util._
import scala.util.Random

class BasicRepopulator extends Repopulator[CellPopulationD] {

  def repopulate(pop:CellPopulationD,dist:Distribution,schedule:CoolingSchedule,rnd:Random): Unit = { 
    val mutProb = schedule.getProb1
    val flipProb = schedule.getProb2
    val popSize = pop.getSize
    val h = popSize - 2
    //var ipop = new Array[Array[InCellD]](inputs) 
    val inputs = pop.getIn
    val inputPop = pop.getInPop
    
    
    for (i <- 0 until inputs) {
      inputPop(i) = inputPop(i).sortWith(_.getFitness < _.getFitness)
      val nextGen = new Array[InCellD](popSize)
      /*
      for (j <- 0 until popSize) {
        nextGen(j) = inputPop(i)(j).makeClone
      }
      */
      val fArray = pop.getCDF(inputPop(i)) //
      for (k <- 0 until h) {
        var idx1 = pop.getIDX(rnd.nextDouble,fArray)
        var idx2 = pop.getIDX(rnd.nextDouble,fArray)
        var counter = 0
        while (idx1 == idx2 && counter < 7) {
          idx2 = pop.getIDX(rnd.nextDouble,fArray)
          counter += 1
        }
        nextGen(k) = inputPop(i)(idx1).combine(inputPop(i)(idx2),dist,mutProb,flipProb)
      }
      for (k <- h until popSize) {
        nextGen(k) = inputPop(i)(k)
      }
      inputPop(i) = nextGen
    }
    val blocks = pop.getBlocks
    val blockPop = pop.getBlockPop
    for (i <- 0 until blocks) {
      blockPop(i) = blockPop(i).sortWith(_.getFitness < _.getFitness)
      val nextGen = new Array[CellBlockD](popSize)
      val fArray = pop.getCDF(blockPop(i))
      for (j <- 0 until h) {
        val k = pop.getIDX(rnd.nextDouble,fArray)
        var l = pop.getIDX(rnd.nextDouble,fArray)
        var counter = 0
        while (l == k && counter < 7) {
          l = pop.getIDX(rnd.nextDouble,fArray)
          counter += 1
        }
        nextGen(j) = blockPop(i)(k).combine(blockPop(i)(l),dist,mutProb,flipProb)
      }
      for (j <- h until popSize) {
        nextGen(j) = blockPop(i)(j)
      }
      blockPop(i) = nextGen
    }
    val outputs = pop.getOut
    val outputPop = pop.getOutPop
    for (i <- 0 until outputs) {
      outputPop(i) = outputPop(i).sortWith(_.getFitness < _.getFitness)
      val nextGen = new Array[OutCellD](popSize)
      val fArray = pop.getCDF(outputPop(i))
      for (j <- 0 until h) {
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
          nextGen(j) = outputPop(i)(k).combine(outputPop(i)(l),dist,mutProb,flipProb)
        }
      }
      for (j <- h until popSize) {
        nextGen(j) = outputPop(i)(j)
      }
      outputPop(i) = nextGen
    }
    
    val neoPop = new CellPopulationD(inputs,blocks,outputs,popSize)
    neoPop.replaceCells(inputPop,blockPop,outputPop)
  }
  override def toString : String = "BasicRepopulator"
}