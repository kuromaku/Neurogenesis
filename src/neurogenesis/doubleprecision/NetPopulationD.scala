package neurogenesis.doubleprecision
import neurogenesis.util.Distribution
import neurogenesis.util.CauchyDistribution
import scalala.library.random.MersenneTwisterFast

import scala.xml.Elem
import scala.xml.TopScope
import scala.xml.NodeSeq

class NetPopulationD(cPop:CellPopulationD) {
  var netPop = new Array[RNND](cPop.getSize*2) //some cells will be used in more than one network
  var sorted = false
  def setSorted(b:Boolean) : Unit = { sorted = b }
  
  //val arrOps = implicitly[ArrayOps]
  def init : Unit = {
    val psize = cPop.getSize
    val inPerms = permutations(psize,cPop.getIn)
    val blockPerms = permutations(psize,cPop.getBlocks)
    val outPerms = permutations(psize,cPop.getOut)
    val n = cPop.getNetworks(inPerms,blockPerms,outPerms)
    for (i <- 0 until psize) {
      netPop(i) = n(i)
    }
    val inPerms2 = permutations(psize,cPop.getIn)
    val blockPerms2 = permutations(psize,cPop.getBlocks)
    val outPerms2 = permutations(psize,cPop.getOut)
    val n2 = cPop.getNetworks(inPerms,blockPerms,outPerms)
    for (i <- psize until netPop.length) {
      netPop(i) = n2(i-psize)
    }
  }
  def init(n:Array[RNND]) : Unit = {
    netPop = n
  }
  
  def burstMutate(rnd:MersenneTwisterFast) : NetPopulationD = {
    val cauchy = new CauchyDistribution(0.005)
    val np = new Array[RNND](netPop.length)
    for (i <- 0 until netPop.length) {
      np(i) = netPop(i).burstMutate(0.1,cauchy,rnd)
    }
    val cp = new CellPopulationD(1,1,1,1)
    val np2 = new NetPopulationD(cp)
    np2.init(np)
    np2
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
      val bc = netPop(netPop.length-1)
      if (bc != null) return bc 
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
  def getSizes = netPop(0).getSizes
  def calculateDiversity : Double = {
    var d = 0.0
    for (i <- 0 until netPop.length - 1) {
      for (j <- i+1 until netPop.length) {
        d += netPop(i).distance(netPop(j))
      }
    }
    d/(getSize*(getSize+1)/2)
  }
  def permute(l:Int) : Array[Int] = {
    val idx = new Range(0,l,1).toArray
    for (i <- 0.until(l)) {
      val j = (math.random*l).toInt
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
  def repopulate(dist:Distribution,mutP:Double,flipP:Double,rnd:MersenneTwisterFast) : Unit = {
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
  def getCDF(e:Array[_ <: EvolvableD]) : Array[Double] = {
    var tProbMass = 0.0d
    for (i <- 0 until e.length) {
      tProbMass += e(i).getFitness
    }
    val a = new Array[Double](e.length)
    
    a(0) = e(0).getFitness/tProbMass
    var sum = a(0)
    for (i <- 1 until e.length) {
      sum += e(i).getFitness
      a(i) = sum/tProbMass
    }
    a
  }
  def getIDX(d:Double,cdf:Array[Double]) : Int = {
    for (i <- 0 until cdf.length) {
      if (d < cdf(i)) {
        return i
      }
    }
    return cdf.length-1
  }
  /*
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
  */
  def sortPop : Unit = {
    if (!sorted) {
      netPop = netPop(0).sort(netPop)
      sorted = true
    }
  }
  override def toString : String = {
    var srep = "<NetPopulationD>\n"
    for (i <- 0.until(netPop.length)) {
      srep += netPop(i)+"\n"
    }
    srep += "</NetPopulationD>"
    srep
  }
  def toXML : Elem = {
    val elems = new Array[Elem](netPop.length)
    //val top = TopScope
    for (i <- 0 until elems.length) {
      elems(i) = netPop(i).toXML //new Elem(null,"Net"+i,null,top,netPop(i).toXML)
    }
    <NetPopulationD>{for (i <- 0 until elems.length) yield elems(i) }</NetPopulationD>
  }
}
object NetPopulationD {
  def fromXML(ns:NodeSeq) : NetPopulationD = {
    val np = ns \\ "RNND"
    val nSize = np.size
    val n = new Array[RNND](nSize)
    
    var i = 0
    for (net <- np) {
      n(i) = RNND.fromXML(net)
      i += 1
    }
    val netP = new NetPopulationD(new CellPopulationD(1,1,1,1))//Might make sense to change the default constructor
    netP.init(n)
    netP
  }
}