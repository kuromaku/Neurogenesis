package neurogenesis.doubleprecision
import neurogenesis.util._
import scalala.library.random.MersenneTwisterFast

class SimpleNetRepopulator extends NetRepopulator[NetPopulationD,CellPopulationD] {
  type T = NetPopulationD
  type U = CellPopulationD
  def repopulate(pop:T,pop2:U,bestNets:Array[RNND],dist:Distribution,schedule:CoolingSchedule,rnd:MersenneTwisterFast,discardRate:Double=0.75) : Unit = {
    val mutP = schedule.getProb1
    val flipP = schedule.getProb2
    val l = pop2.getSize
    val combines = new Array[RNND](l)
    if (!bestReady(bestNets)) {
      
      val inPerms = permutations(l,pop2.getIn)
      val blockPerms = permutations(l,pop2.getBlocks)
      val outPerms = permutations(l,pop2.getOut)
      val n = pop2.getNetworks(inPerms,blockPerms,outPerms)
      for (i <- 0 until l) {
        pop.netPop(i) = n(i)
      }
      for (i <- l until pop.getSize) {
        //Remember to change this if you ever change the number of networks in NetPopulation compared to the size of CP subpopulations
        val idx1 = rnd.nextInt(l)+l
        var idx2 = rnd.nextInt(l)+l
        var count = 0
        while (idx2 == idx1 && count < 7) {
          idx2 = rnd.nextInt(l)+l
        }
        if (count == 7) {
          combines(i-l) = pop.netPop(i).burstMutate(mutP,dist,rnd)
        }
        else {
          combines(i-l) = pop.netPop(idx1).combine(pop.netPop(idx2),dist,mutP,flipP,rnd,discardRate)
        }
      }
      pop.setSorted(false)
    }
    else {
      val l = pop2.getSize
      val inPerms = permutations(l,pop2.getIn)
      val blockPerms = permutations(l,pop2.getBlocks)
      val outPerms = permutations(l,pop2.getOut)
      val n = pop2.getNetworks(inPerms,blockPerms,outPerms)
      for (i <- 0 until l) {
        pop.netPop(i) = n(i)
      }
      for (i <- l until pop.getSize) {
        if (rnd.nextDouble < 0.5) {
          val idx1 = rnd.nextInt(l)+l
          var idx2 = rnd.nextInt(l)+l
          var count = 0
          while (idx2 == idx1 && count < 7) {
            idx2 = rnd.nextInt(l)+l
          }
          if (count == 7) {
            combines(i-l) = pop.netPop(i).burstMutate(mutP,dist,rnd)
          }
          else {
            combines(i-l) = pop.netPop(idx1).combine(pop.netPop(idx2),dist,mutP,flipP,rnd,discardRate)
          }
        }
        else {
          val idx1 = rnd.nextInt(l)+l
          val idx2 = rnd.nextInt(bestNets.length)
          combines(i-l) = pop.netPop(idx1).combine(bestNets(idx2),dist,mutP,flipP,rnd,discardRate)
        }
      }
      for (i <- 0 until combines.length) {
        pop.netPop(l+i) = combines(i)
      }
      pop.setSorted(false)
    }

    /*
    val l = pop.netPop.length
    val mutP = schedule.getProb1
    val flipP = schedule.getProb2
    
    for (i <- 0 until (l-1)) {
      val idx1 = l - rnd.nextInt(l/2) - 1
      var idx2 = l - rnd.nextInt(l/2) - 1
      var count = 0
      while (idx2 == idx1 && count < 7) {
        idx2 = l - rnd.nextInt(l/2) - 1
        count += 1
      }
      if (count == 7) {
        //maybe burstMutate?
        pop.netPop(i) = pop.netPop(idx1).burstMutate(mutP,dist,rnd)
        //netPop(i).reset
      }
      else {
        pop.netPop(i) = pop.netPop(idx1).combine(pop.netPop(idx2),dist,mutP,flipP)
      }
    }
    pop
    */
  }

}