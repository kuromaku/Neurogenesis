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

class NeuralEvolver(cellPop:CellPopulationD,netPop:NetPopulationD,supervisor:EvolutionSupervisor,reporter:ProgressReporter,rnd:MersenneTwisterFast,discardRate:Double=0.75) extends Actor {
  var proceed = true
  var dataSets = List[List[Array[Double]]]() // 0 training inputs, 1 training targets, 2 validation inputs, 3 validation targets...
  var useCompression = true //Let's compress by default when saving the populations
  var cellPopulation = cellPop
  var netPopulation = netPop
  var actFun: Function1[Double,Double] = new SigmoidExp
  var distribution:Distribution = new CauchyDistribution(0.05)
  var schedule: CoolingSchedule = new SimpleSchedule(0.05,0.02,50000)
  //var cmeasure:CComplexityMeasure = new Constant(1d)
  val fsep = System.getProperty("file.separator")
  var savePath = "."+fsep+"saves"+fsep
  var printInfo = true
  var myId = 0//id
  val maxBest = 5
  var bestNets = new Array[RNND](maxBest)
  var lastBestFitness = 0d
  var goodEnoughFitness = 100.0 //used to determine when to stop evolution
  //var maxSteps = 5000
  var noImprovementCounter = 0
  var burstMutationFreq = 25
  var spawningFreq = 50
  var spawned = false
  var saveFreq = 0
  val freqIncrementFactor = 1.15
  var cellRepopulator:Repopulator[CellPopulationD] = new BasicRepopulator
  var netRepopulator:NetRepopulator[NetPopulationD,CellPopulationD] = new SimpleNetRepopulator
  var cMeasure:ComplexityMeasure = new SimpleMeasure
  var complexityBias: Double = cellPop.getNetworkSize
  var mutateNow = false
  var updatingNow = false
  var writeNow = false
  var learningMode = 0 //Basic ESP: 0, Evolino:2, SVMBoost:3, SVMLite: 4
  var combineMode = true
  var fileName = new File("")
  var matrix:DenseMatrix[Double] = null //Used only when the mode is set to Evolino
  var partialDataFeed = true
  val minFeedLength = 1000
  var svmPar:svm_parameter = null
  var svmCols: Array[Array[Double]] = null
  var epsilonRegression = false //Determines which SVM regression mode to use
  var washoutTime = 100 //the time period the networks are allowed to get into the input while the predictions are not counted
  var maxBlocks = 20
  var maxCells = 20
  
  def getCellPop : CellPopulationD = cellPopulation
  def getNetPop : NetPopulationD = netPopulation
  def getMemorySize : (Int,Int) = { (cellPopulation.getBlocks,cellPopulation.getStateLength) }
  def setID(nid:Int) : Unit = { myId = nid }
  def setNetPop(np2:NetPopulationD) : Unit = { netPopulation = np2 }
  def setCellPop(cp2:CellPopulationD) : Unit = { cellPopulation = cp2 }
  def setRepopulator(rp:Repopulator[CellPopulationD]) : Unit = { cellRepopulator = rp }
  def setNetRepopulator(nrp:NetRepopulator[NetPopulationD,CellPopulationD]) : Unit = { netRepopulator = nrp }
  def setComplexityMeasure(measure:ComplexityMeasure) : Unit = { cMeasure = measure }
  def setDistribution(dist2:Distribution) : Unit = { distribution = dist2 }
  def setMeasure(measure2:ComplexityMeasure) : Unit = { cMeasure = measure2 }
  def setSchedule(cs:CoolingSchedule) : Unit = {
    schedule = cs
  }
  def setSavePath(path:String) : Unit = { savePath = path }
  def setSaveFreq(frq:Int) : Unit = { saveFreq = frq }
  def setUseFullDataFeed(b:Boolean) : Unit = { partialDataFeed = !b }
  def setMaximumSize(maxCells2:Int,maxBlocks2:Int) : Unit = {
    maxCells = maxCells2
    maxBlocks = maxBlocks2
  }
  def isRunning : Boolean = updatingNow
  def initMatrix : Unit = {
    matrix = NeuralOps.list2Matrix(dataSets.apply(1).drop(washoutTime)) //.drop(washoutTime)
  }
  def act : Unit = {
    if (learningMode == 2) {
      //Evolino requires an initialized data matrix
      initMatrix
    }
    if (dataSets.apply(0).size < minFeedLength) {
      partialDataFeed = false
    }
    loop {
      react {
        case UpdateNow(step) => {
          updatingNow = true
          //println("Evo"+myId+" Step: "+schedule.getCurrent)
          //TODO: Verify logic
          val (d0,d1,d2,d3) = sliceData //slices datasets if only using partial data
          if (partialDataFeed && learningMode == 2) {
            matrix = NeuralOps.list2Matrix(d1)
            /*
            if (washoutTime == 0) {
              matrix = NeuralOps.list2Matrix(d1)
            }
            else {
              matrix = NeuralOps.list2Matrix(d1.drop(washoutTime))
            }
            */
          }
          for (i <- 0 until netPopulation.getSize) {
            val rnn = netPopulation.getRNN(i)

            if (learningMode < 2 || learningMode == 4) {
              val res = rnn.feedData(d0,actFun).drop(washoutTime)
              
              var mse = totalError(d1.drop(washoutTime),res)
              val closeToConstant = calculateVariability(res)
              var fn = 1.0/mse
              if (fn.isNaN()) {
                fn = 0
              }
              else if (closeToConstant < 0.05 && closeToConstant < fn) {
                fn = closeToConstant //this is supposed to penalize networks which predict for example constant zero values
              }
              rnn.setFitness(fn,cMeasure,complexityBias)
            }
            else if (learningMode == 2) { //Evolino
              val rM = rnn.linearRegression(d0,matrix,actFun,washoutTime) //NeuralOps.list2Matrix(dataSets.apply(1))
              if (rM != null) {
                val err = rnn.evolinoValidate(d2,d3,actFun,rM)
                //val closeToConstant = calculateVariability(res)
                var fn = 1000000.0
                if (err.isNaN) {
                  fn = 0
                }
                else if (err > 0.0) {
                  fn = 1.0/err
                }
                rnn.setFitness(fn,cMeasure,complexityBias)
              }
              else {
                rnn.setFitness(0,cMeasure,complexityBias)
              }
            }
            else if (learningMode == 3) { //svm
              val res = rnn.svmRegression(d0,svmCols,actFun,svmPar,d2,washoutTime)
              val err = totalError(d3,res)
              val closeToConstant = calculateVariability(res)
              if (closeToConstant < 0.05 || closeToConstant.isNaN) {
                rnn.setFitness(0,cMeasure,complexityBias)
              }
              else {
                if (!err.isNaN && err > 0) {
                  rnn.setFitness(1.0/err,cMeasure,complexityBias)
                }
                else {
                  rnn.setFitness(0.0,cMeasure,complexityBias)
                }
              }

            }
          }
          netPopulation.sortPop
          if (learningMode == 4) { //let's only use the best network for svm regression
            val rnn = netPopulation.getRNN(netPopulation.getSize-1)
            rnn.reset
            val res = rnn.svmRegression(d0,svmCols,actFun,svmPar,d2,washoutTime)
            val err = totalError(d3,res)
            
            if (!err.isNaN && err > 0) {
              val svmF = 1.0/err
              var fs = svmF.toString
              if (fs.length > 7) {
                fs = fs.substring(0,7)
              }
              if (svmF > rnn.getFitness) { reporter ! ProgressMessage("SVM fitness: "+fs+" while without regression it is: "+rnn.getFitnessString) }
               //rnn.setFitness(100.0/err)
            }
            else {
              //rnn.setFitness(0.0)
            }
            //netPopulation.sortPop
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
            if (bestFitness >= goodEnoughFitness) {
              supervisor ! AnotherRNN(netPopulation.getBestNet(bestFitness))
              exit
            }
          }
          else {
            noImprovementCounter += 1
            if (noImprovementCounter % burstMutationFreq == 0) {
              mutateNow = true
            }
            else if (noImprovementCounter % (burstMutationFreq-7) == 0) {
              //As the net population develops some unique cells let's occasionally inject them back to the cellPopulation
              val rnn = bestNets(rnd.nextInt(bestNets.length))
              if (rnn != null) {
                cellPopulation.injectCells(rnn)
              }
            }
            
          }
          //netPop.repopulate(distribution,mutProb,flipProb,rnd)
          if (!mutateNow) {
            cellRepopulator.repopulate(cellPopulation,distribution,schedule,rnd,discardRate)
            /*
            if (learningMode == 0) {
              cellPopulation.repopulate(distribution,schedule,rnd)
            }
            else {
              cellPopulation.repopulate(distribution,schedule,rnd,0.7)
            }
            */
          }
          else {
            cellPopulation.burstMutate(schedule.getProb1*1.1,distribution,rnd)
            mutateNow = false
            if (printInfo) {
              reporter ! ProgressMessage("Evolver id: "+myId+" undergoing burstmutation")
            }
          }
          netRepopulator.repopulate(netPopulation,cellPopulation,bestNets,distribution,schedule,rnd,discardRate)
          /*
          if (learningMode == 0 || (learningMode == 1 && !bestNetsReady)) {
            netPop.repopulate(cellPopulation)
          }
          else if (learningMode == 1 && bestNetsReady) {
            netPop.repopulate(cellPopulation,bestNets,distribution,schedule.getProb1,schedule.getProb2,rnd)
          }
          else {
            netPop.repopulate(cellPopulation)
          }
          */
          if (!spawned && spawnNow) {
            val sizes = netPopulation.getSizes
            var evo2:NeuralEvolver = null
            var evo3:NeuralEvolver = null
            if (sizes(2) < maxCells) {
              val cPop2 = cellPopulation.complexify(false,rnd)
              val nPop2 = new NetPopulationD(cPop2)
              nPop2.init
              evo2 = new NeuralEvolver(cPop2,nPop2,supervisor,reporter,new MersenneTwisterFast(System.currentTimeMillis()),discardRate)
              configure(evo2)
            }
            else {
              reporter ! ProgressMessage("Evolver id: "+myId+" could not spawn a new Evolver with an added memory cell due to size limitation")
            }
            if (sizes(1) < maxBlocks) {
              val cPop3 = cellPopulation.complexify(true,rnd)
              val nPop3 = new NetPopulationD(cPop3)
              nPop3.init
              evo3 = new NeuralEvolver(cPop3,nPop3,supervisor,reporter,new MersenneTwisterFast(System.currentTimeMillis()),discardRate)
              configure(evo3)
            }
            else {
              reporter ! ProgressMessage("Evolver id: "+myId+" could not spawn a new Evolver with an added memory block due to size limitation")
            }
            if (evo2 != null || evo3 != null) {
              supervisor ! BirthMessageD(evo2,evo3)
              if (printInfo) {
                reporter ! ProgressMessage("Evolver id: "+myId+" spawning new NeuralEvolvers.")
              }
            }
            //supervisor ! BirthMessageD()

            spawned = true
          }
          
          if (writeNow) {
            write(fileName)
            writeNow = false
          }
          else if (schedule.getCurrent % saveFreq == 0) {
            val s0 = lastBestFitness.toString
            val fstring = if (s0.length > 6) s0.substring(0,6) else { s0 }
            val fileName = new File(savePath+"/evolver_g"+schedule.getCurrent+"id"+myId+"f"+fstring+".xml")
            saveEvolver(fileName)
          }
          supervisor ! StatusMessage(bestFitness,myId)
          schedule.update(netPopulation.getBestFitness)
          if (schedule.getCurrent == schedule.getMax) {
            //supervisor ! UpdateNow(schedule.getCurrent)
            updatingNow = false
            supervisor ! "Exiting"
            exit
          }
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
  def addDLists(dat:List[List[Array[Double]]]) : Unit = { dataSets = dataSets ++ dat }
  def configure(e:NeuralEvolver) : Unit = {
    e.addDLists(dataSets)
    e.setActFun(actFun)
    e.setSavePath(savePath)
    e.setSpawningFreq(spawningFreq)
    e.setRepopulator(cellRepopulator)
    e.setNetRepopulator(netRepopulator)
    e.setSchedule(schedule.makeClone)
    e.setEvoMode(learningMode)
    e.setMaximumSize(maxCells,maxBlocks) //system resource demands grow rather fast especially if SVMBoost is used with network size
    if (learningMode >= 3) {
      e.initSVMLearner(svmCols,epsilonRegression)//svmNodes,
    }
    e.setSaveFreq(saveFreq)
  }
  def sliceData : (List[Array[Double]],List[Array[Double]],List[Array[Double]],List[Array[Double]]) = {
    val (d0,d0b) = if (partialDataFeed) { 
          dataSets.apply(0).splitAt(schedule.getFeedLength(0)) } 
            else (dataSets.apply(0),null)
    val (d1,d1b) = if (partialDataFeed) { 
            dataSets.apply(1).splitAt(schedule.getFeedLength(1)) } 
            else (dataSets.apply(1),null)
    val d2:List[Array[Double]] = if (learningMode >=2 ) {
            if (partialDataFeed) {
              val d0bl = d0b.length
              val d2l = schedule.getFeedLength(2)
              if (d2l > d0bl) {
                d0b ++ dataSets.apply(2).slice(0,d2l-d0bl)
              }
              else {
                d0b.slice(0,d2l)
              }
            }
            else {
              dataSets.apply(2) 
            }
          }
          else null
   val d3:List[Array[Double]] = if (learningMode >=2 ) {
            if (partialDataFeed) {
              val d1bl = d1b.length
              val d3l = schedule.getFeedLength(3)
              if (d3l > d1bl) {
                d1b ++ dataSets.apply(3).slice(0,d3l-d1bl)
              }
              else {
                d1b.slice(0,d3l)
              } 
            }
            else {
              dataSets.apply(3)
            }
          }
          else null
    (d0,d1.drop(washoutTime),d2,d3)
  }
  def sliceData2 : (List[Array[Double]],List[Array[Double]],List[Array[Double]],List[Array[Double]]) = {
    if (!partialDataFeed) {
      (dataSets.apply(0),dataSets.apply(1),dataSets.apply(2),dataSets.apply(3))
    }
    else {
      val (a0,a1) = dataSets.apply(0).splitAt(schedule.getFeedLength(0))
      val (b0,b1) = dataSets.apply(1).splitAt(schedule.getFeedLength(1))
      val a1l = a1.length
      val c2l = schedule.getFeedLength(2)
      val c0 = if (c2l > a1l) {
        a1 ++ dataSets.apply(2).slice(0,c2l-a1l)
      }
      else { a1.slice(0,c2l) }
      val d0 = if (learningMode >= 2) {
        val b1l = b1.length
        val d3l = schedule.getFeedLength(3)
        if (d3l > b1l) {
          b1 ++ dataSets.apply(3).slice(0,d3l-b1l)
        }
        else {
          b1.slice(0,d3l)
        }
      }
      else { null }
      (a0,b0.drop(washoutTime),c0,d0)
    }
  }
  def addToBestNets(net:RNND) : Boolean = {
    val idx = bestNets.indexWhere(_ == null)
    if (idx >= 0) {
      bestNets(idx) = net.makeClone
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
    bestNets.count(_ != null)
  }
  def getBest : RNND = {
    val networks = bestNets.filter(_ != null)
    networks.last
  }
  def getAllTheBest : List[RNND] = {
    bestNets.filter(_ != null).toList
  }
  def getBestIfPossible(mf:Double) : RNND = {
    if (bestNets == null) {
      null
    }
    else {
      val networks0 = bestNets.filter(_ != null)
      if (networks0.length > 0) {
        val candidates = networks0.find(_.getFitness == mf)
        if (candidates == None) {
          null
        }
        else {
          candidates.get
        }
      }
      else {
        null
      }
    }
  }
  def meanSquaredError(a1:Array[Double],a2:Array[Double]) : Double = {
    var error = 0d
    for (i <- 0 until a1.length) {
      error += scala.math.sqrt(scala.math.pow(a2(i)-a1(i),2))
    }
    error = error/a1.length
    error
  }
  def totalError(a1:Array[Array[Double]],a2:Array[Array[Double]]) : Double = {
    var error = 0d
    for (i <- 0 until a1.length) {
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
    error/l1.size
  }
  def totalError(l1:List[Array[Double]],a2:Array[Array[Double]]) : Double = {
    var error = 0.0
    var idx = 0
    for (l <- l1) {
      error += meanSquaredError(l,a2(idx))
      idx += 1
    }
    error/l1.size
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
    error/a1.length
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
  def initSVMLearner(tCols:Array[Array[Double]],epsilonR:Boolean) : Unit = {
    initParam(epsilonR)
    //svmNodes = nodes, nodes:Array[Array[svm_node]],
    svmCols = tCols
  }
  def mixPopulations(evolver2:NeuralEvolver,mixProb:Double) : Unit = {
    cellPop.mixPopulations(evolver2.getCellPop, mixProb)
  }
  def calculateVariability(a:Array[Array[Double]]) : Double = {
    val cols = a(0).length
    val vars = new Array[Double](cols)
    var minsum = Double.MaxValue
    val maxRows = if (a.length < 50) a.length else 50 //Maybe it's enough to consider only a few values
    for (c <- 0 until cols) {
      for (r <- 1 until maxRows) {
        vars(c) += scala.math.abs(a(r)(c)-a(r-1)(c))
      }
      if (vars(c) < minsum) {
        minsum = vars(c)
      }
    }
    minsum
  }
  def setActFun(actFun2:Function1[Double,Double]) : Unit = { actFun = actFun2 }
  def setBurstFreq(freq:Int) : Unit = { burstMutationFreq = freq }
  def setPrintInfo(b:Boolean) : Unit = { printInfo = b }
  def setSpawningFreq(freq:Int) : Unit = { spawningFreq = freq }
  def setWashoutTime(time:Int) : Unit = { washoutTime = time }
  //def setMutationProb(d:Double) : Unit = { mutProb0 = d }
  //def setFlipProb(d:Double) : Unit = { flipProb0 = d }
  def setEvoMode(m:Int) : Unit = { learningMode = m }
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
      if (learningMode == 2) {
        val rM =
      }
      */
      true
    }
  }
  def saveBestNet(f:File) : Boolean = {
    val fw = new FileWriter(f)
    val networks = bestNets.filter(_ != null).toList
    
    if (networks.size < 1) {
      false
    }
    else {
      for (n <- networks) {
        val netAsXML = n.toXML
        scala.xml.XML.write(fw,netAsXML.head,"UTF-8",true,null)
        val fxml = <Fitness>{n.getFitness}</Fitness>
        scala.xml.XML.write(fw,fxml,"UTF-8",false,null)
        netAsXML.tail.foreach(e => scala.xml.XML.write(fw,e,"UTF-8",false,null))
        fw.close
      
        if (learningMode == 2) {
          n.reset
          val rM = n.linearRegression(dataSets.apply(0),NeuralOps.list2Matrix(dataSets.apply(1)),actFun)
          val fw2 = new FileOutputStream(f.getParent()+"weightMatrix.txt")
          Storage.storetxt(fw2,rM)//
          fw2.close
        }
      }
      
      true
    }
  }
  /*Writes this instance into a given file possibly using compression
   * 
   */
  def saveEvolver(f:File) : Unit = {
    if (useCompression) {
      val f2 = new File(f.getPath+".zip")
      val xmls = toXML.toString.getBytes
      val compressor = new DeflaterOutputStream(new FileOutputStream(f2))
      val l = xmls.length
      var i = 0
      while (i < l) {
        compressor.write(xmls(i))
        i += 1
      }
      compressor.close
      
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
    rep.append("Learning Mode: "+learningMode+"\n")
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
  def runDiagnostics : (Int,Double) = {
    val identical = cellPopulation.countEquals
    (identical,cellPopulation.calculateDiversity)
  }

}
object NeuralEvolver {
  def makeEvolver(memoryBlocks:Int,memoryCells:Int,inputs:Int,outputs:Int,subpopSize:Int,distScale:Double,supervisor:EvolutionSupervisor,reporter:ProgressReporter) : NeuralEvolver = {
    var cellPopulation = new CellPopulationD(inputs,memoryBlocks,outputs,subpopSize)
    val rnd2 = new MersenneTwisterFast(System.currentTimeMillis)
    cellPopulation.init(distScale,1,rnd2)
    for (i <- 1 until memoryCells) {
      cellPopulation = cellPopulation.complexify(false,rnd2)
    }
    val nPop = new NetPopulationD(cellPopulation)
    nPop.init
    new NeuralEvolver(cellPopulation,nPop,supervisor,reporter,rnd2)
  }
}