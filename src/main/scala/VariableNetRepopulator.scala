package neurogenesis.doubleprecision
import neurogenesis.util._
import scala.util.Random

class VariableNetRepopulator(deathRate:Double) extends NetRepopulator[NetPopulationD,CellPopulationD] {
  type T = NetPopulationD
  type U = CellPopulationD
  def repopulate(pop:T,pop2:U,bestNets:Array[RNND],dist:Distribution,schedule:CoolingSchedule,rnd:Random) : Unit = {
    val deaths = (deathRate*pop.netPop.length).toInt
    val tl = pop.netPop.length
    //val diff = tl - deaths

    val mutP = schedule.getProb1
    val flipP = schedule.getProb2
    if (!bestReady(bestNets)) {
      val l = pop2.getSize
      val inPerms = permutations(l,pop2.getIn)
      val blockPerms = permutations(l,pop2.getBlocks)
      val outPerms = permutations(l,pop2.getOut)
      val n = pop2.getNetworks(inPerms,blockPerms,outPerms)
      for (i <- 0 until l) {
        pop.netPop(i) = n(i)
      }
      for (i <- l until deaths) {
        val idx1 = rnd.nextInt(tl-l)
        var idx2 = rnd.nextInt(tl-l)
        var count = 0
        while (idx2 == idx1 && count < 7) {
          idx2 = rnd.nextInt(tl-l)
        }
        if (count == 7) {
          pop.netPop(i) = pop.netPop(idx1).burstMutate(mutP,dist,rnd)
        }
        else {
          pop.netPop(i) = pop.netPop(idx1).combine(pop.netPop(idx2),dist,mutP,flipP)
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
      for (i <- l until deaths) {
        if (rnd.nextDouble < 0.5) {
          val idx1 = rnd.nextInt(tl-l)
          var idx2 = rnd.nextInt(tl-l)
          var count = 0
          while (idx2 == idx1 && count < 7) {
            idx2 = rnd.nextInt(tl-l)
          }
          if (count == 7) {
            pop.netPop(i) = pop.netPop(idx1).burstMutate(mutP,dist,rnd)
          }
          else {
            pop.netPop(i) = pop.netPop(idx1).combine(pop.netPop(idx2),dist,mutP,flipP)
          }
        }
        else {
          val idx1 = rnd.nextInt(tl-l)
          val idx2 = rnd.nextInt(bestNets.length)
          pop.netPop(i) = pop.netPop(idx1).combine(bestNets(idx2),dist,mutP,flipP)
        }
      }
      pop.setSorted(false)
    }
  }
  
}