package neurogenesis.doubleprecision

import neurogenesis.msg._
import neurogenesis.util.ProgressReporter

import scala.actors.Actor
import scala.collection.immutable.Queue
import scala.swing.TextArea
import scala.swing.Label
import scala.xml.XML
import scala.xml.Elem
import java.io.File
//import scala.collection.mutable.LinkedList
import scala.collection.mutable.ArrayOps
import neurogenesis.util.XMLOperator

class EvolutionSupervisor(tArea:TextArea,fLabel:Label,numThreads:Int,saveWhenExiting:Boolean=false) extends Actor {
  var maxThreads = numThreads
  var runningThreads = 0
  var evolvers = new Array[(NeuralEvolver,Int)](numThreads)
  var numEvolvers = numThreads
  var maxGenerations = 10000L
  var counter = 1L
  var printInfo = true
  val reporter = new ProgressReporter(tArea)
  reporter.start
  var maxFit = 0.0d
  var saveCounter = 1
  var saveFrequency = 5000L
  var savePath = "./saves/"
  var exitCounter = 0
  var saveOnExit = saveWhenExiting
  var running = false
  var maxID = 0
  var mixingFreq = 41
  var mixingProb = 0.05
  var mixingStep = 1
  def act : Unit = {
    loop {
      react {
        case "Start" => {
          running = true
          for (i <- 0 until evolvers.length) {
            evolvers(i)._1.start
          }
          var i = 0
          while (runningThreads < maxThreads && i < evolvers.length) {
            evolvers(i)._1 ! UpdateNow(counter)
            runningThreads += 1
            if (printInfo) {
              reporter ! ProgressMessage("Number of running threads: "+runningThreads)
            }
            i += 1
          }
        }
        case UpdateNow(step) => {
          //reporter ! ProgressMessage("Supervisor is updating: "+step.toString+"\n")
          var freeThreads = (runningThreads < maxThreads)
          var i = 0
          while (freeThreads && i < evolvers.length) {
            if (!evolvers(i)._1.isRunning) {
              evolvers(i)._1 ! UpdateNow(counter)
              runningThreads += 1
              if (runningThreads == maxThreads) {
                freeThreads = false
              }
              if (printInfo) {
                reporter ! ProgressMessage("Number of running threads: "+runningThreads)
              }
            }
            /*
            if (saveFrequency > 1 && counter % saveFrequency == 0L) {
              evolvers(i)._1 ! Save2File(new File(savePath+saveCounter+"g"+evolvers(i)._2+".xml"))
            }
            */
            i += 1
          }
          if (freeThreads) {
            this ! UpdateNow(counter)
          }
        }
        case StatusMessage(bestFitness,from) => {
          //fitnessTable = fitnessTable.+:((bestFitness,from))
          
          var idx = 0
          var found = false
          while (idx < evolvers.length && !found) {
            if (evolvers(idx)._1.myId == from) {
              evolvers(idx) = (evolvers(idx)._1,evolvers(idx)._2+1)
              idx = evolvers.length
              found = true
            }
            idx += 1
          }
          if (bestFitness > maxFit) {
            maxFit = bestFitness
            fLabel.text_=("Fitness: "+maxFit.toString.substring(0,7))
          }
          if (printInfo) {
            val fRep = bestFitness.toString
            if (fRep.length > 7) {
              reporter ! ProgressMessage("Latest reported fitness value: "+fRep.substring(0,7)+" (id: "+from+")")
            }
            else {
              reporter ! ProgressMessage("Latest reported fitness value: "+fRep+" (id: "+from+")")
            }
            
          }
          processData
          if (mixingFreq > 1 && mixingStep % mixingFreq == 0) {
            val numMixed = mixPopulations(mixingProb)
            reporter ! ProgressMessage("Mixed "+numMixed+" populations")
            mixingStep = 1
          }
          else {
            if (mixingFreq > 1) mixingStep += 1
          }
          this ! UpdateNow(counter)
        }
        
        case BirthMessageD(evo2,evo3) => {
          addEvolvers(Seq[NeuralEvolver](evo2,evo3))
          //addEvolver(evo3)
          if (printInfo) {
            reporter ! ProgressMessage("Supervisor started another NeuralEvolver")
          }
          this ! UpdateNow(counter)
        }
        case AnotherRNN(goodEnough) => {
          reporter ! ProgressMessage("Found a network that seems good enough!!!")
          reporter ! ProgressMessage(goodEnough.toXML.toString)
          if (saveOnExit) {
            save(goodEnough.toXML,goodEnough.getFitness.toString)
            reporter ! ProgressMessage("Tried saving the result.")
          }
          this ! "Exit"
        }
        case "Exit" => {
          for (e <- evolvers) {
            e._1 ! MakeExit(saveOnExit)
          }
          reporter ! "Exit"
        }
        case "Exiting" => {
          exitCounter += 1
          running = false
          if (exitCounter == evolvers.size) {
            var bn = gatherBest.toArray
            bn = bn.sortWith(_.getFitness < _.getFitness)
            tArea.append("Maximum number of steps already performed.\n")
            tArea.append("The resulting best net in XML:\n")
            XMLOperator.runPrettyPrint(bn(bn.length-1).toXML,tArea)
            tArea.append("\n")
            if (printInfo) {
              reporter ! ProgressMessage("Supervisor says Goodbye!")
            }
            exit
          }
        }
      }

    }
  }
  def addEvolver(idx:Int,e:NeuralEvolver) : Unit = {
    if (idx < evolvers.length) {
      evolvers(idx) = (e,0)
      e.setID(idx)
      e.start
      maxID += 1
    }
    else addEvolver(e)
  }
  def addEvolver(e:NeuralEvolver) : Unit = {
    val arr = new Array[(NeuralEvolver,Int)](evolvers.length+1)
    for (i <- 0 until evolvers.length) {
      arr(i) = evolvers(i)
    }
    e.setID(maxID)
    arr(evolvers.length) = (e,0)
    evolvers = arr
    e.setPrintInfo(printInfo)
    e.start()
    maxID += 1
  }
  def addEvolvers(el:Seq[NeuralEvolver]) : Unit = {
    val arr = new Array[(NeuralEvolver,Int)](evolvers.length+el.size)
    for (i <- 0 until evolvers.length) {
      arr(i) = evolvers(i)
    }
    val k = evolvers.length
    var s = 0
    for (evo <- el) {
      evo.setID(maxID)
      arr(k+s) = (evo,0)
      evo.setPrintInfo(printInfo)
      evo.start
      maxID += 1
      s += 1
    }
    evolvers = arr
  }
  def removeEvolver : Unit = {
    var i = 0
    var removed = false
    while (i < evolvers.length/2 && !removed) {
      val (e,g) = evolvers(evolvers.length-1-i)
      val best = e.getBest
      if (best != null) {
        val f = best.getFitness
        var j = evolvers.length/2
        while (j < evolvers.length && !removed) {
          val (e2,g2) = evolvers(evolvers.length-j)
          if (e2.getBest.getFitness > f && g2 < g) {
            e ! MakeExit(false)
            val arr2 = new Array[(NeuralEvolver,Int)](evolvers.length-1)
            for (k <- 0 until evolvers.length) {
              if (k < i) {
                arr2(k) = evolvers(k)
              }
              else if (k == i) {
              
              }
              else {
                arr2(k-1) = evolvers(k)
              }
            }
            removed = true
            evolvers = arr2
            tArea.append("Supervisor dropped one obsolete Evolver.\n")
          }
          j += 1
        }
      }
      i += 1
    }
  }
  def getReporter : ProgressReporter = reporter
  def getNumberOfEvolvers = numEvolvers
  def mixPopulations(mixProb:Double) : Int = {
    var numMixed = 0
    for (i <- 0 until (evolvers.length-1)) {
      val memPar = evolvers(i)._1.getMemorySize
      val rest = evolvers.drop(i+1)
      for (j <- 0 until rest.length) {
        if (rest(j)._1.getMemorySize == memPar) {
          evolvers(i)._1.mixPopulations(rest(j)._1, mixProb)
          numMixed += 1
        }
      }
    }
    numMixed
  }
  def setThreads(maxT:Int) : Unit = { 
    maxThreads = maxT
    if (!running) {
      evolvers = new Array[(NeuralEvolver,Int)](maxThreads)
    } 
  }
  def setMaximumGenerations(g:Long) : Unit = { maxGenerations = g }
  def setPrintInfo(b:Boolean) : Unit = { printInfo = b }
  def processData : Unit = {
    runningThreads -= 1
    evolvers = evolvers.sortWith(_._2 < _._2) //ordered by how many steps they have already evolved
    if (evolvers.head._2 == counter) { //add to counter only when all evolvers have reached this step
      counter += 1
      //println("Counter: "+counter)
      if (counter % 33 == 0) {
        if (evolvers.length > maxThreads) {
          removeEvolver
        }
      }
      if (counter == maxGenerations) {
        this ! "Exit"
      }
    }
  }
  def reset : Unit = {
    evolvers = new Array[(NeuralEvolver,Int)](numThreads)
    running = false
  }
  def getSuperStar : RNND = {
    var found = false
    //var idx = 4
    var best = evolvers(0)._1.getBest
    var bi = 1
    var f = 0.0
    if (best != null) {
      f = best.getFitness
    }
    while (bi < evolvers.size) {
      val candidate = evolvers(bi)._1.getBest
      if (candidate != null) {
        val f2 = candidate.getFitness
        if (f2 > f) {
          f = f2
          best = candidate
        }
      }
      bi += 1
    }
    best
  }
  def printInfo(b:Boolean) : Unit = {
    printInfo = b
    for ((e,gen) <- evolvers) {
      e.setPrintInfo(b)
    }
  }
  def gatherBest : List[RNND] = {
    var lobn = List[RNND]()
    for ((e,c) <- evolvers) {
      val bofe = e.getAllTheBest
      if (bofe != List[RNND]()) {
        lobn = lobn ++ bofe
      }
    }
    lobn
  }
  def runDiagnostics : List[(Int,Double)] = {
    var ivals = List[(Int,Double)]()
    reporter ! ProgressMessage("Running diagnostics for "+evolvers.length+" evolvers")
    
    for (i <- 0 until evolvers.size) {
      val (e,g) = evolvers(i)
      reporter ! ProgressMessage("Running diagnostics for evolver number: "+e.myId)
      ivals = ivals.+:(e.runDiagnostics)
    }
    ivals
  }
  def setSaveOnExit(b:Boolean) : Unit = { saveOnExit = b }
  def setSavePath(path:String) = savePath = path
  def setMixingParameters(freq:Int,prob:Double) : Unit = { mixingFreq = freq; mixingProb = prob }
  def save(e:Elem,fs:String) : Unit = {
    XML.save(savePath+"best"+fs+".xml",e,"UTF-8",true)
  }
}