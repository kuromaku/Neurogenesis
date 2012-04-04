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

class NeuralEvolver(cellPop:CellPopulationD,netPop:NetPopulationD,supervisor:EvolutionSupervisor,id:Int,reporter:ProgressReporter,rnd:Random) extends Actor {
  var proceed = true
  /*
  var inputData = List[Array[Double]]()
  var outputData = List[Array[Double]]()
  var valInput = List[Array[Double]]()
  var valTarget = List[Array[Double]]()
  */
  var dataSets = List[List[Array[Double]]]() // 0 training inputs 1 training targets 2 validation inputs 3 validation targets...
  
  var cellPopulation = cellPop
  var netPopulation = netPop
  var actFun: Function1[Double,Double] = new SigmoidExp
  val distribution:Distribution = new CauchyDistribution(0.05)
  var schedule: CoolingSchedule = new SimpleSchedule(0.05,0.02,50000)
  //val rnd = new Random
  var savePath = "./saves/"
  var printInfo = true
  var myId = id
  var bestNets = new Array[RNND](5)
  var lastBestFitness = 0d
  //var maxSteps = 5000
  var noImprovementCounter = 0
  var burstMutationFreq = 25
  var spawningFreq = 50
  val freqIncrement = 20
  var rePopulator:Repopulator[CellPopulationD] = new BasicRepopulator
  var netRepopulator:NetRepopulator[NetPopulationD,CellPopulationD] = new SimpleNetRepopulator
  var mutateNow = false
  var updatingNow = false
  //var obsolete = false
  var writeNow = false
  var evoMode = 0 //which mode to use?
  var fileName = new File("")
  def setID(nid:Int) : Unit = { myId = nid }
  def setNetPop(np2:NetPopulationD) : Unit = { netPopulation = np2 }
  def setCellPop(cp2:CellPopulationD) : Unit = { cellPopulation = cp2 }
  def setRepopulator(rp:Repopulator[CellPopulationD]) : Unit = { rePopulator = rp }
  def setNetRepopulator(nrp:NetRepopulator[NetPopulationD,CellPopulationD]) : Unit = { netRepopulator = nrp }
  def isRunning : Boolean = updatingNow
  
  def act : Unit = {
    loop {
      react {
        case UpdateNow(step) => {
          /*
          if (printInfo) {
            reporter ! ProgressMessage("Evolver id "+myId+" reacting to the command to update to step "+step+".")
          }
          */
          updatingNow = true
          //println("Evo"+myId+" Step: "+schedule.getCurrent)
          //currentStep += 1
          for (i <- 0 until netPopulation.getSize) {
            if (evoMode < 2) {
              val res = netPopulation.getRNN(i).feedData(dataSets.apply(0),actFun)
              var mse = totalError(dataSets.apply(1),res)
              var fn = 10.0/mse
              if (fn.isNaN()) {
                fn = 0
              }
              netPopulation.getRNN(i).setFitness(fn)
            }
            else if (evoMode == 2) {
              val rM = netPopulation.getRNN(i).linearRegression(dataSets.apply(0),NeuralOps.list2Matrix(dataSets.apply(1)),actFun)
              if (rM != null) {
                val err = netPopulation.getRNN(i).evolinoValidate(dataSets.apply(2),dataSets.apply(3),actFun,rM)
                var fn = 1000000.0
                if (err > 0.0) {
                  fn = 100.0/err
                }
                else if (err.isNaN) {
                  fn = 0
                }
                netPopulation.getRNN(i).setFitness(fn)
              }
              else {
                netPopulation.getRNN(i).setFitness(0)
              }
            }
          }
          netPopulation.sortPop
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
            rePopulator.repopulate(cellPopulation,distribution,schedule,rnd)
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
          netRepopulator.repopulate(netPopulation,cellPopulation,bestNets,distribution,schedule,rnd)
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
          if (spawnNow) {
            val cPop2 = cellPopulation.complexify(false,rnd)
            val cPop3 = cellPopulation.complexify(true,rnd)
            val nPop2 = new NetPopulationD(cPop2)
            nPop2.init
            val nPop3 = new NetPopulationD(cPop3)
            nPop3.init
            val evo2 = new NeuralEvolver(cPop2,nPop2,supervisor,2*id,reporter,rnd)
            //evo2.addData(inputData,outputData)
            evo2.setEvoMode(evoMode)
            val evo3 = new NeuralEvolver(cPop3,nPop3,supervisor,2*id+1,reporter,rnd)
            //evo3.addData(inputData,outputData)
            evo2.addDLists(dataSets)
            evo3.addDLists(dataSets)
            
            evo3.setEvoMode(evoMode)
            supervisor ! BirthMessageD(evo2)
            supervisor ! BirthMessageD(evo3)
            if (printInfo) {
              reporter ! ProgressMessage("Evolver id: "+myId+" spawning new NeuralEvolvers.")
            }
          }
          /*
          if (printInfo) {
           reporter ! ProgressMessage("Evolver id: "+myId+" sending the report to the supervisor.")
          }
          */
          supervisor ! StatusMessage(bestFitness,myId) 
          if (writeNow) {
            write(fileName)
            writeNow = false
          }
          if (schedule.getCurrent < schedule.getMax) {
            supervisor ! UpdateNow(schedule.getCurrent)
          }
          else {
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
        case "Exit" => {
          if (printInfo) {
            reporter ! ProgressMessage("Terminating NeuralEvolver "+myId+".")
          }
          val saveDir = new File(savePath)
          if (!saveDir.exists()) {
            saveDir.mkdir()
          }
          /*
          if (schedule.getCurrent > 1000) {
            saveBestNet(new File(savePath+"rnn"+schedule.getCurrent+"id"+myId+"blocks"+cellPopulation.getBlocks+"memCells"+cellPopulation.blockPop(0)(0).getNumOfCells+".xml"))
          }
          */
          val fileName = new File(savePath+"evolver_g"+schedule.getCurrent+"id"+myId+".txt")
          if (!fileName.exists) {
            saveEvolver(fileName)
          }
          supervisor ! "Exiting"
          exit()
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
    if (bestNets(0) == null) {
      bestNets(0) = net
      true
    }
    else {
      var emptySlot = false
      var idx = 1
      while (!emptySlot && idx < bestNets.length) {
        if (bestNets(idx) == null) {
          emptySlot = true
          bestNets(idx) = net
        }
        else {
          idx += 1
        }
      }
      if (!emptySlot) {
        bestNets = bestNets.sortWith(_.getFitness < _.getFitness)
        if (bestNets(0).getFitness < net.getFitness) {
          bestNets(0) = net
          true
        }
        else {
          false
        }
      }
      else {
        true
      }
    }
  }
  def bestNetsReady : Boolean = {
    !bestNets.exists(_ == null)
  }
  def meanSquaredError(a1:Array[Double],a2:Array[Double]) : Double = {
    var error = 0d
    for (i <- 0 until a1.length) {
      error += Math.sqrt(Math.pow(a2(i)-a1(i),2))
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
  def setActFun(actFun2:Function1[Double,Double]) : Unit = { actFun = actFun2 }
  def setBurstFreq(freq:Int) : Unit = { burstMutationFreq = freq }
  def setPrintInfo(b:Boolean) : Unit = { printInfo = b }
  //def setMutationProb(d:Double) : Unit = { mutProb0 = d }
  //def setFlipProb(d:Double) : Unit = { flipProb0 = d }
  def setEvoMode(m:Int) : Unit = { evoMode = m }
  def spawnNow : Boolean = {
    //(rnd.nextDouble > 0.80)
    
    val b = (schedule.getCurrent % spawningFreq.toLong) == 0L
    if (b) {
      spawningFreq += freqIncrement
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
    val fw = new FileWriter(f)
    val xml = toXML
    scala.xml.XML.write(fw,xml.head,"UTF-8",false,null)
    xml.tail.foreach(e => scala.xml.XML.write(fw,e,"UTF-8",false,null))
    fw.close
  }
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
  def setSchedule(cs:CoolingSchedule) : Unit = {
    schedule = cs
  }
  def getSimpleRepresentation : String = {
    val rep = new StringBuilder("NeuralEvolver:\n")
    rep.append("Learning Mode: "+evoMode+"\n")
    rep.append("Distribution: "+distribution.toString+"\n")
    rep.append("ActFun: "+actFun.toString+"\n")
    rep.append("Schedule: "+schedule.toString+"\n")
    rep.append(rePopulator.toString)
    rep.append()
    rep.toString
  }
  def toXML : Elem = {
    <NeuralEvolver><Fitness>{lastBestFitness}</Fitness>{cellPopulation.toXML}{netPopulation.toXML}</NeuralEvolver>
    /*
    val pop1xml = cellPopulation.toXML
    val pop2xml = netPopulation.toXML
    <NeuralEvolver>{pop1xml}{pop2xml}</NeuralEvolver>
    */
  }
}