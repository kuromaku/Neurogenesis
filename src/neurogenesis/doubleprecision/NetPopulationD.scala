package neurogenesis.doubleprecision
import neurogenesis.util.Distribution
import scala.util.Random
// extends NetPopulationT[Double]
class NetPopulationD(cPop:CellPopulationD) {
  var netPop = new Array[RNND](cPop.getSize*2)
  var sorted = false
  def setSorted(b:Boolean) : Unit = { sorted = b }
  
  //val arrOps = implicitly[ArrayOps]
  def init : Unit = {
    val inPerms = permutations(cPop.getSize,cPop.getIn)
    val blockPerms = permutations(cPop.getSize,cPop.getBlocks)
    val outPerms = permutations(cPop.getSize,cPop.getOut)
    val n = cPop.getNetworks(inPerms,blockPerms,outPerms)
    for (i <- 0 until cPop.getSize) {
      netPop(i) = n(i)
    }
    val inPerms2 = permutations(cPop.getSize,cPop.getIn)
    val blockPerms2 = permutations(cPop.getSize,cPop.getBlocks)
    val outPerms2 = permutations(cPop.getSize,cPop.getOut)
    val n2 = cPop.getNetworks(inPerms,blockPerms,outPerms)
    for (i <- cPop.getSize until netPop.length) {
      netPop(i) = n2(i-cPop.getSize)
    }
  }
  def getBestFitness : Double = {
    if (sorted) {
      return netPop(netPop.length-1).getFitness
    }
    var best = 0d
    for (i <- 0 until netPop.size) {
      if (netPop(i).getFitness > best) {
        best = netPop(i).getFitness
      }
    }
    best
  }
  def getRNN(idx:Int) : RNND = netPop(idx)
  def getBestNet(f:Double) : RNND = {
    if (sorted) {
      return netPop(netPop.length-1)
    }
    var idx = 0
    while (idx < netPop.length) {
      if (netPop(idx).getFitness == f) {
        return netPop(idx)
      }
      idx += 1
    }
    return netPop(0)
  } 
  def getSize = netPop.length
  
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
  //The following functionality is becoming obsolete as it is being moved to NetRepopulator
  def repopulate(dist:Distribution,mutP:Double,flipP:Double,rnd:Random) : Unit = {
    sortPop
    val l = netPop.length
    for (i <- 0 until l/2) {
      val idx1 = l - rnd.nextInt(l/2) - 1
      var idx2 = l - rnd.nextInt(l/2) - 1
      var count = 0
      while (idx2 == idx1 && count < 7) {
        idx2 = l - rnd.nextInt(l/2) - 1
        count += 1
      }
      if (count == 7) {
        //maybe burstMutate?
        netPop(i) = netPop(idx1).burstMutate(mutP,dist,rnd)
        //netPop(i).reset
      }
      else {
        netPop(i) = netPop(idx1).combine(netPop(idx2),dist,mutP,flipP)
      }
    }
    sorted = false
  }
  def repopulate(nPop:CellPopulationD) : Unit = {
    val inPerms = permutations(nPop.getSize,nPop.getIn)
    val blockPerms = permutations(nPop.getSize,nPop.getBlocks)
    val outPerms = permutations(nPop.getSize,nPop.getOut)
    val n = nPop.getNetworks(inPerms,blockPerms,outPerms)
    for (i <- 0 until netPop.size) {
      netPop(i) = n(i)
    }
    sorted = false
  }
  def repopulate(nCellPop:CellPopulationD,bNets:Array[RNND],dist:Distribution,mutP:Double,flipP:Double,rnd:Random) : Unit = {
    val num = nCellPop.getSize/2
    val inPerms = permutations(num,nCellPop.getIn)
    val blockPerms = permutations(num,nCellPop.getBlocks)
    val outPerms = permutations(num,nCellPop.getOut)
    val n = nCellPop.getNetworks(inPerms,blockPerms,outPerms)
    for (i <- 0 until num) {
      netPop(i) = n(i)
    }
    for (i <- num until nCellPop.getSize) {
      netPop(i) = netPop(i).combine(bNets(rnd.nextInt(bNets.length)),dist,mutP,flipP,rnd)
    }
    sorted = false
  }
  def sortPop : Unit = {
    if (!sorted) {
      netPop = netPop(0).sort(netPop)
      sorted = true
    }
  }
  override def toString : String = {
    var srep = "<NetPopulationT>\n"
    for (i <- 0.until(netPop.length)) {
      srep += netPop(i)+"\n"
    }
    srep += "</NetPopulationT>"
    srep
  }
}