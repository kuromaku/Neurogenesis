package neurogenesis.doubleprecision

import neurogenesis.msg._
import neurogenesis.util._
import scala.actors.Actor
//import scala.collection.mutable.LinkedList
import scala.collection.immutable.List
import scala.util.Random
import java.io.File
import java.io.FileOutputStream
import java.io.FileInputStream
import java.io.FileWriter
import scala.xml._
import scalala.library.Storage
import java.util.zip.Deflater
import java.util.zip.DeflaterOutputStream
import java.util.zip.InflaterInputStream
import scalala.tensor.dense.DenseMatrix
import scalala.library.random.MersenneTwisterFast
import libsvm._

class NeuralEvolver(cellPop:CellPopulationD,netPop:NetPopulationD,supervisor:EvolutionSupervisor,id:Int,reporter:ProgressReporter,rnd:MersenneTwisterFast,discardRate:Double=0.75) extends Actor {
  var proceed = true
  /*
  var inputData = List[Array[Double]]()
  var outputData = List[Array[Double]]()
  var valInput = List[Array[Double]]()
  var valTarget = List[Array[Double]]()
  */
  var dataSets = List[List[Array[Double]]]() // 0 training inputs, 1 training targets, 2 validation inputs, 3 validation targets...
  var useCompression = true //Let's compress by default when saving the populations
  var cellPopulation = cellPop
  var netPopulation = netPop
  var actFun: Function1[Double,Double] = new SigmoidExp
  var distribution:Distribution = new CauchyDistribution(0.05)
  var schedule: CoolingSchedule = new SimpleSchedule(0.05,0.02,50000)
  //val rnd = new Random
  var savePath = "./saves/"
  var printInfo = true
  var myId = id
  val maxBest = 5
  var bestNets = new Array[RNND](maxBest)
  var lastBestFitness = 0d
  //var maxSteps = 5000
  var noImprovementCounter = 0
  var burstMutationFreq = 25
  var spawningFreq = 50
  var spawned = false
  val freqIncrementFactor = 1.15
  var cellRepopulator:Repopulator[CellPopulationD] = new BasicRepopulator
  var netRepopulator:NetRepopulator[NetPopulationD,CellPopulationD] = new SimpleNetRepopulator
  var mutateNow = false
  var updatingNow = false
  //var obsolete = false
  var writeNow = false
  var evoMode = 0 //which mode to use?
  var fileName = new File("")
  var matrix:DenseMatrix[Double] = null
  
  var svmPar:svm_parameter = null
  var svmCols: Array[Array[Double]] = null
  var svmNodes:Array[Array[svm_node]] = null
  var epsilonRegression = false
  
  def getCellPop : CellPopulationD = cellPopulation
  def getNetPop : NetPopulationD = netPopulation
  def setID(nid:Int) : Unit = { myId = nid }
  def setNetPop(np2:NetPopulationD) : Unit = { netPopulation = np2 }
  def setCellPop(cp2:CellPopulationD) : Unit = { cellPopulation = cp2 }
  def setRepopulator(rp:Repopulator[CellPopulationD]) : Unit = { cellRepopulator = rp }
  def setNetRepopulator(nrp:NetRepopulator[NetPopulationD,CellPopulationD]) : Unit = { netRepopulator = nrp }
  def setDistribution(dist2:Distribution) : Unit = { distribution = dist2 }
  def setSchedule(cs:CoolingSchedule) : Unit = {
    schedule = cs
  }
  def setSavePath(path:String) : Unit = { savePath = path }
  def isRunning : Boolean = updatingNow
  def initMatrix : Unit = {
    matrix = NeuralOps.list2Matrix(dataSets.apply(1))
  }
  def act : Unit = {
    if (evoMode == 2) {
      initMatrix
    }
    loop {
      react {
        case UpdateNow(step) => {
          updatingNow = true
          //println("Evo"+myId+" Step: "+schedule.getCurrent)
          for (i <- 0 until netPopulation.getSize) {
            val rnn = netPopulation.getRNN(i)
            if (evoMode < 2 || evoMode == 4) {
              val res = rnn.feedData(dataSets.apply(0),actFun)
              var mse = totalError(dataSets.apply(1),res)
              var fn = 10.0/mse
              if (fn.isNaN()) {
                fn = 0
              }
              rnn.setFitness(fn)
            }
            else if (evoMode == 2) { //Evolino
              val rM = rnn.linearRegression(dataSets.apply(0),matrix,actFun) //NeuralOps.list2Matrix(dataSets.apply(1))
              if (rM != null) {
                val err = rnn.evolinoValidate(dataSets.apply(2),dataSets.apply(3),actFun,rM)
                var fn = 1000000.0
                if (err > 0.0) {
                  fn = 100.0/err
                }
                else if (err.isNaN) {
                  fn = 0
                }
                rnn.setFitness(fn)
              }
              else {
                rnn.setFitness(0)
              }
            }
            else if (evoMode == 3) { //svm
              val res = rnn.svmRegression(dataSets.apply(0),svmCols,actFun,svmPar,dataSets.apply(2))
              val err = totalError(dataSets.apply(3),res)
              
              if (!err.isNaN && err > 0) {
                rnn.setFitness(100.0/err)
              }
              else {
                rnn.setFitness(0.0)
              }
            }
          }
          netPopulation.sortPop
          if (evoMode == 4) { //let's only use the best network for svm regression
            val rnn = netPopulation.getRNN(netPopulation.getSize-1)
            rnn.reset
            val res = rnn.svmRegression(dataSets.apply(0),svmCols,actFun,svmPar,dataSets.apply(2))
            val err = totalError(dataSets.apply(3),res)
            if (!err.isNaN && err > 0) {
               rnn.setFitness(100.0/err)
            }
            else {
              rnn.setFitness(0.0)
            }
            netPopulation.sortPop
          }
          val bestFitness = netPopulation.getBestFitness
          if (lastBestFitness < bestFitness) {
            lastBestFitness = bestFitness
            addToBestNets(netPopulation.getRNN(netPopulation.netPop.length-1))
            if (printInfo) {
              reporter ! ProgressMessage("Evo "+myId+" found a new better solution with fitness: "+bestFitness.toString.substring(0,7))
              reporter ! ProgressMessage("There was no progress for "+noImprovementCounter+" generations")
            }
            noImprovementCounter = 0
          }
          else {
            noImprovementCounter += 1
            if (noImprovementCounter % burstMutationFreq == 0) {
              mutateNow = true
            }
          }
          //netPop.repopulate(distribution,mutProb,flipProb,rnd)
          if (!mutateNow) {
            cellRepopulator.repopulate(cellPopulation,distribution,schedule,rnd,discardRate)
            /*
            if (evoMode == 0) {
              cellPopulation.repopulate(distribution,schedule,rnd)
            }
            else {
              cellPopulation.repopulate(distribution,schedule,rnd,0.7)
            }
            */
          }
          else {
            cellPopulation.burstMutate(schedule.getProb1*1.5,distribution,rnd)
            mutateNow = false
            reporter ! ProgressMessage("Evolver id: "+myId+" undergoing burstmutation")
          }
          netRepopulator.repopulate(netPopulation,cellPopulation,bestNets,distribution,schedule,rnd,discardRate)
          /*
          if (evoMode == 0 || (evoMode == 1 && !bestNetsReady)) {
            netPop.repopulate(cellPopulation)
          }
          else if (evoMode == 1 && bestNetsReady) {
            netPop.repopulate(cellPopulation,bestNets,distribution,schedule.getProb1,schedule.getProb2,rnd)
          }
          else {
            netPop.repopulate(cellPopulation)
          }
          */
          if (!spawned && spawnNow) {
            val cPop2 = cellPopulation.complexify(false,rnd)
            val cPop3 = cellPopulation.complexify(true,rnd)
            val nPop2 = new NetPopulationD(cPop2)
            nPop2.init
            val nPop3 = new NetPopulationD(cPop3)
            nPop3.init
            val evo2 = new NeuralEvolver(cPop2,nPop2,supervisor,2*id,reporter,new MersenneTwisterFast(System.currentTimeMillis()),discardRate)
            //evo2.addData(inputData,outputData)
            evo2.setEvoMode(evoMode)
            
            val evo3 = new NeuralEvolver(cPop3,nPop3,supervisor,2*id+1,reporter,new MersenneTwisterFast(System.currentTimeMillis()),discardRate)
            //evo3.addData(inputData,outputData)
            evo2.addDLists(dataSets)
            evo3.addDLists(dataSets)
            evo2.setSavePath(savePath)
            evo2.setSpawningFreq(spawningFreq)
            evo2.setRepopulator(cellRepopulator)
            evo2.setNetRepopulator(netRepopulator)
            evo2.setSchedule(new SimpleSchedule(schedule.getProb1,schedule.getProb2,schedule.getMax))
            evo3.setEvoMode(evoMode)
            evo3.setSavePath(savePath)
            evo3.setSpawningFreq(spawningFreq)
            evo3.setRepopulator(cellRepopulator)
            evo3.setNetRepopulator(netRepopulator)
            evo3.setSchedule(new SimpleSchedule(schedule.getProb1,schedule.getProb2,schedule.getMax))
            if (evoMode >= 3) {
              evo2.initSVMLearner(svmNodes,svmCols,epsilonRegression)
              evo3.initSVMLearner(svmNodes,svmCols,epsilonRegression)
            }
            supervisor ! BirthMessageD(evo2)
            supervisor ! BirthMessageD(evo3)
            if (printInfo) {
              reporter ! ProgressMessage("Evolver id: "+myId+" spawning new NeuralEvolvers.")
            }
            spawned = true
          }
          
          if (writeNow) {
            write(fileName)
            writeNow = false
          }
          
          supervisor ! StatusMessage(bestFitness,myId)
          
          if (schedule.getCurrent == schedule.getMax) {
            //supervisor ! UpdateNow(schedule.getCurrent)
            supervisor ! "Exit"
            exit
          }
          schedule.update(netPopulation.getBestFitness)
          updatingNow = false
        }
        case Save2File(f) => {
          fileName = f
          writeNow = true
          reporter ! ProgressMessage("Will try writing an XML representation of NeuralEvolver "+myId+" to a file "+f.getPath+" after next step")
        }
        case MakeExit(saveOnExit) => {
          if (printInfo) {
            reporter ! ProgressMessage("Terminating NeuralEvolver "+myId+".")
          }
          if (saveOnExit) {
            val saveDir = new File(savePath)
            if (!saveDir.exists()) {
              saveDir.mkdir()
            }
            val s0 = lastBestFitness.toString
            val fstring = if (s0.length > 6) s0.substring(0,6) else { s0 }
            val fileName = new File(savePath+"/evolver_g"+schedule.getCurrent+"id"+myId+"f"+fstring+".xml")
            if (!fileName.exists) {
              saveEvolver(fileName)
            }
            supervisor ! "Exiting"
            exit()
          }
          else {
            supervisor ! "Exiting"
            exit()
          }
        }
      }
    }

  }
  /*
  def addData(inDat:List[Array[Double]],tgtDat:List[Array[Double]]) : Unit = {
    inputData = inDat
    outputData = tgtDat
  }
  def addData2(inDat2:List[Array[Double]],tgtDat:List[Array[Double]]) : Unit = {
    valInput = inDat2
    valTarget = tgtDat
  }
  */
  def addDLists(dat:List[List[Array[Double]]]) : Unit = { dataSets = dataSets ++ dat }
  

  def addToBestNets(net:RNND) : Boolean = {
    var emptySlot = false
    var emptyIdx = 0
    for (i <- 0 until bestNets.length) {
      if (bestNets(i) != null) {
        if (bestNets(i).getFitness == net.getFitness) {
          return false
        }
      }
      else {
        emptySlot = true
        emptyIdx = i
      }
    }
    if (emptySlot) {
      bestNets(emptyIdx) = net.makeClone
      true
    }
    else {
      bestNets = bestNets.sortWith(_.getFitness < _.getFitness)
      if (bestNets(0).getFitness < net.getFitness) {
        bestNets(0) = net.makeClone
        true
      }
      else {
        false
      }
    }
  }
  def bestNetsReady : Boolean = {
    !bestNets.exists(_ == null)
  }
  def countBest : Int = {
    var c = 0
    for (i <- 0 until bestNets.length) {
      if (bestNets(i) != null) c += 1
    }
    c
  }
  def getBest : RNND = {
    var idx = bestNets.length - 1
    var found = false
    var bf = 0.0
    var idx2 = 0
    while (idx >= 0) {
      if (bestNets(idx) != null) {
        val cnd = bestNets(idx).getFitness
        if (cnd > bf) {
          bf = cnd
          idx2 = idx
        }
      }
      idx -= 1
    }
    bestNets(idx2)
  }
  def getAllTheBest : List[RNND] = {
    var lobn = List[RNND]()
    for (i <- 0 until maxBest) {
      if (bestNets(i) != null) {
        lobn = lobn :+ (bestNets(i))
      }
    }
    lobn
  }
  def meanSquaredError(a1:Array[Double],a2:Array[Double]) : Double = {
    var error = 0d
    for (i <- 0 until a1.length) {
      error += scala.math.sqrt(scala.math.pow(a2(i)-a1(i),2))
    }
    error = error
    error
  }
  def totalError(a1:Array[Array[Double]],a2:Array[Array[Double]]) : Double = {
    var error = 0d
    for (i <- 0 until a1.size) {
      error += meanSquaredError(a1(i),a2(i))
    }
    error = error/a1.length
    error
  }
  def totalError(l1:List[Array[Double]],l2:List[Array[Double]]) : Double = {
    var error = 0.0
    var l0 = l2
    for (l <- l1) {
      error += meanSquaredError(l,l0.head)
      l0 = l0.tail
    }
    error
  }
  def totalError(l1:List[Array[Double]],a2:Array[Array[Double]]) : Double = {
    var error = 0.0
    var idx = 0
    for (l <- l1) {
      error += meanSquaredError(l,a2(idx))
      idx += 1
    }
    error
  }
  def totalError2(a1:List[Array[Double]],a2:Array[Array[Double]]) : Double = {
    var error = 0.0
    var idx = 0
    for (l <- a1) {
      if (idx != 0) {
        error += meanSquaredError(l,a2(idx-1))
      }
      idx += 1
    }
    error
  }
  def initParam(epsilon:Boolean) : Unit = {
    svmPar = new svm_parameter
    if (epsilon) {
      svmPar.svm_type = svm_parameter.EPSILON_SVR
    }
    else {
      svmPar.svm_type = svm_parameter.NU_SVR
    }
    svmPar.kernel_type = svm_parameter.RBF
    svmPar.degree = 3
    svmPar.gamma = 1.0/cellPopulation.getStateLength
    svmPar.coef0 = 0
    svmPar.nu = 0.5
    svmPar.cache_size = 1
    svmPar.C = 1
    svmPar.eps = 1e-2
    svmPar.p = 0.1
    svmPar.shrinking = 1
    svmPar.probability = 0
    svmPar.nr_weight = 0
    //svmPar.weight_label = new Array[Int](0)
    //svmPar.weight = new Array[Double](0)
  }
  def initSVMLearner(nodes:Array[Array[svm_node]],tCols:Array[Array[Double]],epsilonR:Boolean) : Unit = {
    initParam(epsilonR)
    svmNodes = nodes
    svmCols = tCols
  }
  def setActFun(actFun2:Function1[Double,Double]) : Unit = { actFun = actFun2 }
  def setBurstFreq(freq:Int) : Unit = { burstMutationFreq = freq }
  def setPrintInfo(b:Boolean) : Unit = { printInfo = b }
  def setSpawningFreq(freq:Int) : Unit = { spawningFreq = freq }
  //def setMutationProb(d:Double) : Unit = { mutProb0 = d }
  //def setFlipProb(d:Double) : Unit = { flipProb0 = d }
  def setEvoMode(m:Int) : Unit = { evoMode = m }
  def spawnNow : Boolean = {
    val b = (schedule.getCurrent % spawningFreq.toLong) == 0L
    if (b) {
      spawningFreq = (spawningFreq*freqIncrementFactor).toInt
    }
    b
  }
  /*
  def updateParameters : Unit = {
    val factor = (maxSteps-currentStep).toDouble/maxSteps
    mutProb = factor*mutProb0
    distribution.adjust(0.98)
  }
  */
  def write(f:File) : Boolean = {
    if (f.exists) {
      false
    }
    else {
      val tag = <NeuralEvolver>{parametersToXML}{cellPopulation.toXML}</NeuralEvolver>
      val fw = new FileWriter(f)
      //println(tag.head)
      scala.xml.XML.write(fw,tag.head,"UTF-8",true,null)
      tag.tail.foreach(e => scala.xml.XML.write(fw,e,"UTF-8",false,null))
      fw.close
      /*
      if (evoMode == 2) {
        val rM =
      }
      */
      true
    }
  }
  def saveBestNet(f:File) : Boolean = {
    val fw = new FileWriter(f)
    val found = false
    var idx = 4
    while (idx >= 0 && bestNets(idx) == null) {
      idx -= 1
    }
    
    if (idx > 0) {
      
      val netAsXML = bestNets(idx).toXML
      scala.xml.XML.write(fw,netAsXML.head,"UTF-8",true,null)
      val fxml = <Fitness>{bestNets(idx).getFitness}</Fitness>
      scala.xml.XML.write(fw,fxml,"UTF-8",false,null)
      netAsXML.tail.foreach(e => scala.xml.XML.write(fw,e,"UTF-8",false,null))
      fw.close
      
      if (evoMode == 2) {
        bestNets(idx).reset
        val rM = bestNets(idx).linearRegression(dataSets.apply(0),NeuralOps.list2Matrix(dataSets.apply(1)),actFun)
        val fw2 = new FileOutputStream(f.getParent()+"weightMatrix.txt")
        Storage.storetxt(fw2,rM)//
        fw2.close
      }
      
      true
    }
    else {
      false
    }
  }
  def saveEvolver(f:File) : Unit = {
    if (useCompression) {
      val f2 = new File(f.getPath+".zip")
      val xmls = toXML.toString.getBytes
      val compressor = new DeflaterOutputStream(new FileOutputStream(f2))//Deflater
      val l = xmls.length
      var i = 0
      while (i < l) {
        compressor.write(xmls(i))
        i += 1
      }
      compressor.close
      //compressor.setInput(xmls)
      //compressor.finish()
      
    }
    else {
      val fw = new FileWriter(f)
      val xml = toXML
      scala.xml.XML.write(fw,xml.head,"UTF-8",false,null)
      xml.tail.foreach(e => scala.xml.XML.write(fw,e,"UTF-8",false,null))
      fw.close
    }

  }
  /*
  def restoreEvolver(f:File) : Unit = {
    val inflater = new InflaterInputStream(new FileInputStream(f))
    var lb = List[Byte]()
    val bl = 100
    var buffer = new Array[Byte](bl)
    while (inflater.available == 1) {
      val r = inflater.read(buffer,0,bl)
      for (i <- 0 until r) {
        lb = lb.+:(buffer(i))
      }
    }
    val s = new java.lang.String(lb.reverse.toArray)
    println(s.substring(0,40))
  }
  */
  def parametersToString : String = {
    val s = "<Parameters><MutProb>"+schedule.getProb1+"</MutProb><FlipProb>"+schedule.getProb2+"</FlipProb></Parameters>"
    s
  }
  def parametersToXML : Elem = {
    val mp = <MutProb>{schedule.getProb1}</MutProb>
    val fp = <FlipProb>{schedule.getProb2}</FlipProb>
    val x = <Parameters>{mp}{fp}</Parameters>
    x
  }

  def getSimpleRepresentation : String = {
    val rep = new StringBuilder("NeuralEvolver:\n")
    rep.append("Learning Mode: "+evoMode+"\n")
    rep.append("Distribution: "+distribution.toString+"\n")
    rep.append("ActFun: "+actFun.toString+"\n")
    rep.append("Schedule: "+schedule.toString+"\n")
    rep.append(cellRepopulator.toString)
    rep.append()
    rep.toString
  }
  def toXML : Elem = {
    val c = countBest
    if (c > 0) {
      val reps = new Array[Elem](c)
      var bid = 0
      for (i <- 0 until c) {
        while (bestNets(bid) == null) {
          bid += 1
        }
        reps(i) = bestNets(bid).toXML
      }
      //val bnXML = <BestNets>{for (i <- 0 until bestNets.length) yield reps(i) }</BestNets>
      <NeuralEvolver><Fitness>{lastBestFitness}</Fitness><BestNets>{for (i <- 0 until c) yield reps(i) }</BestNets>{cellPopulation.toXML}{netPopulation.toXML}</NeuralEvolver>
    }
    else {
      <NeuralEvolver><Fitness>{lastBestFitness}</Fitness>{cellPopulation.toXML}{netPopulation.toXML}</NeuralEvolver>
    }
      /*
    val pop1xml = cellPopulation.toXML
    val pop2xml = netPopulation.toXML
    <NeuralEvolver>{pop1xml}{pop2xml}</NeuralEvolver>
    */
  }
  def restoreBestNets(e:NodeSeq) : Unit = {
    val n = e \\ "RNND"
    bestNets = new Array[RNND](n.length)
    var idx = 0
    for (net <- n) {
      bestNets(idx) = RNND.fromXML(net)
      idx += 1
    }
  }

}