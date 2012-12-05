package neurogenesis.doubleprecision

import neurogenesis.msg._
import neurogenesis.util._
import java.util.zip.InflaterInputStream
import java.util.zip.DeflaterOutputStream
import java.io.FileInputStream
import java.io.FileOutputStream
import scala.swing.SimpleSwingApplication
import scala.swing._
import scala.swing.FileChooser.SelectionMode
import scala.swing.event._
import scala.util.Random
import scala.xml._
import java.io.File
import java.io.FileWriter
import java.io.BufferedReader
import java.io.FileReader
import scala.xml.Text
import scalala.library.Plotting
import scalala.generic.collection.CanViewAsTensor1._
import javax.swing.border._
import edu.uci.ics.jung.graph.SparseGraph
import edu.uci.ics.jung.io.GraphMLWriter
import edu.uci.ics.jung.algorithms.layout.FRLayout
import edu.uci.ics.jung.visualization.VisualizationImageServer
import edu.uci.ics.jung.visualization.VisualizationViewer
import java.awt.Image
//import java.awt.image.RenderedImage
//import java.awt.Point
import javax.imageio.ImageIO
import scalala.library.random.MersenneTwisterFast
import scalala.library.Storage
import libsvm.svm_parameter
import libsvm.svm_node

class EvolverInterface extends SimpleSwingApplication {
  val fsep = System.getProperty("file.separator")
  val configPath = "."+fsep+"EvolverConfig.xml"
  var saveDirectory = "."+fsep+"data"+fsep
  var numberOfLinesShown = 100
  var dataReady = false
  var dims = new Array[Int](2)
  var popSize = 20
  val reportArea = new TextArea
  reportArea.border_=(new TitledBorder(new LineBorder(java.awt.Color.black),"Program messages:")) //new SoftBevelBorder(BevelBorder.RAISED)
  reportArea.editable_=(false)
  val fitnessLabel = new Label("NaN") //
  var numThreads = 4
  var supervisor = new EvolutionSupervisor(reportArea,fitnessLabel,numThreads)
  var evolvers = List[NeuralEvolver]()
  var restoredRNNs = List[RNND]()
  //var evolvers2 = List[NeuralEvolver[Float]]()
  val rnd = new MersenneTwisterFast(System.currentTimeMillis())//new Random
  var printInfo = false
  val start = new Button {
	  text = "Start Evolution"
	  enabled_=(false)
  }
  val rCol = new Color(100,100,100)
  val sCol = new Color(200,200,200)
  var normalizeData = true
  object dataPanel extends TextArea { editable_=(false); border_=(new TitledBorder(new SoftBevelBorder(BevelBorder.LOWERED),"Data in memory:")) }
  object dataCounter extends Label { text_=("No data") }
  val makePredictions = new MenuItem("Predict using best network") { enabled_=(false) }
  val iWorker = new InterfaceWorker(dataPanel,dataCounter,start)
  val dworker = new DataWorker(supervisor.getReporter,iWorker,normalizeData)
  //val modes = initModes
  var evolutionMode = 0 // 0 basic 2 evolino
  var initialDistScale = 0.1d
  var mutProb = 0.07
  var flipProb = 0.01
  var burstMutationFreq = 17
  var autoSave = 500
  var autoSaveBackup = 500
  var maxSteps = 10000L
  var washoutTime = 100 //the time period the networks are allowed to get into the input while the predictions are not counted
  
  var saveOnExit = false
  var actFun: Function1[Double,Double] = new OutFun
  var dimX = 540
  var dimY = 400
  var regressionMode = false
  var initialFwdConnections = 4
  var initialRecConnections = 2
  var initialCells = 2
  var initialBlocks = 2
  //var restoredEvolver:NeuralEvolver = null
  var evolverInMemory = false
  var restoredCount = 0
  //var defaultLearningMode = 0
  //var parFields = new Array[TextField](6)
  object memoryCellField extends TextField(initialCells.toString) {
    object cellAction extends Action("") {
      def apply : Unit = {
        try {
          initialCells = memoryCellField.text.toInt
          if (initialCells < 1) { initialCells = 0 }
          reportArea.append("Initial Cells: "+initialCells+"\n")
        } catch {
          case _ => reportArea.append("Enter that number again.\n")
          
        }
      }
    }
    action_=(cellAction)
  }
  object memoryBlockField extends TextField(initialBlocks.toString) {
    object blockAction extends Action("") {
      def apply : Unit = {
        try {
          initialBlocks = memoryBlockField.text.toInt
          if (initialBlocks < 1) {
            initialBlocks = 1
          }
          reportArea.append("Initial Blocks: "+initialBlocks+"\n")
        } catch {
          case _ => reportArea.append("Enter the number of blocks again.\n")
        }
      }
    }
    action_=(blockAction)
  }
  object mutProbField extends TextField(mutProb.toString) {
    object mutprobAction extends Action("") {
      def apply : Unit = {
        try {
          mutProb = mutProbField.text.toDouble
          reportArea.append("Changing the mutProb to "+mutProb+"\n")
          if (mutProb > 0.9) {
            mutProb = 0.9
            reportArea.append("Sorry, 0.9 is the maximum accepted value.\n")
          }
          else if (mutProb < 0) {
            mutProb = 0.001
          }
        } catch {
          case _ => reportArea.append("Could not parse the value of mutProb.\n")
        }
      }
    }
    action_=(mutprobAction)
  }
  object flipProbField extends TextField(flipProb.toString) {
    object flipAction extends Action("") {
      def apply : Unit = {
        try {
          flipProb = flipProbField.text.toDouble
          reportArea.append("Changing the flipProb to "+flipProb+"\n")
        } catch {
          case _ => reportArea.append("Could not parse the value of flipProb.\n")
        }
      }
    }
    action_=(flipAction)
  }
  object scaleField extends TextField(initialDistScale.toString) {
    object scaleAction extends Action("") {
      def apply : Unit = {
        try {
          initialDistScale = scaleField.text.toDouble
          reportArea.append("Dist scale is now "+initialDistScale+"\n")
        } catch {
          case _ => reportArea.append("Could not parse the value of distScale.\n")
        }
      }
    }
    action_=(scaleAction)
  }
  object burstFreqField extends TextField(burstMutationFreq.toString) {
    object burstAction extends Action("") {
      def apply : Unit = {
        try {
          burstMutationFreq = burstFreqField.text.toInt
          reportArea.append("Burst Freq: "+burstMutationFreq+"\n")
        } catch {
          case _ => reportArea.append("Could not parse the typed value.\n")
        }
      }
    }
    action_=(burstAction)
  }
  object autoSaveField extends TextField(autoSave.toString) {
    object saveAction extends Action("") {
      def apply : Unit = {
        try {
          autoSave = autoSaveField.text.toInt
          reportArea.append("Autosaving every "+autoSave+" steps.\n")
        } catch {
          case _ => reportArea.append("Could not parse that value.\n")
        }
      }
    }
    action_=(saveAction)
  }
  var spawnFreq = 50
  object spawnFreqField extends TextField(spawnFreq.toString) {
    object freqAction extends Action("") {
      def apply : Unit = {
        try {
          spawnFreq = spawnFreqField.text.toInt
          reportArea.append("Changing the spawnFreq to "+spawnFreq+"\n")
        } catch {
          case _ => reportArea.append("Could not parse the value of spawnFreq.\n")
        }
      }
    }
    action_=(freqAction)
  }
  var mixingFreq = 50
  object mixingFreqField extends TextField(mixingFreq.toString) {
    object mixingAction extends Action("") {
      def apply : Unit = {
        try {
          mixingFreq = mixingFreqField.text.toInt
          reportArea.append("Mixing Frequency is now: "+mixingFreq+"\n")
          if (mixingFreq < -1) {
            mixingFreq = -1 //no mixing
          }
        } catch {
          case _ => reportArea.append("Please enter acceptable mixing frequency.\n")
        }
      }
    }
    action_=(mixingAction)
  }
  var mixingProb = 0.1
  object mixingProbField extends TextField(mixingProb.toString) {
    object mixingProbAction extends Action("") {
      def apply : Unit = {
        try {
          mixingProb = mixingProbField.text.toDouble
          if (mixingProb < 0.0) {
            mixingProb = 0.0
          }
          else if (mixingProb > 0.5) {
            mixingProb = 0.5 //no extremes
          }
          reportArea.append("Mixing probability is now: "+mixingProb+"\n")
        } catch {
          case _ => reportArea.append("Try to enter another value for mixing probability.\n")
        }
      }
    }
    action_=(mixingProbAction)
  }
  var discardRate = 0.5
  object discardRateField extends TextField(discardRate.toString) {
    object rateAction extends Action("") {
      def apply : Unit = {
        try {
          discardRate = discardRateField.text.toDouble
          reportArea.append("Discard Rate is now: "+discardRate+"\n")
        } catch {
          case _ => reportArea.append("Could not parse the value of discard rate.\n")
        }
      }
    }
    action_=(rateAction)
  }
  val popSizeSlider = new Slider {
	max_=(100)
	min_=(5)
	value_=(popSize)
  }
  var maxPopSize = 100
  object subpopField extends TextField(maxPopSize.toString) {
    object subpopAction extends Action("") {
      def apply : Unit = {
        try {
          maxPopSize = subpopField.text.toInt
          popSizeSlider.max_=(maxPopSize)
          reportArea.append("Maximum subpopulation size is now: "+maxPopSize+"\n")
        } catch {
          case _ => reportArea.append("Could not parse that.\n")
        }
      }
    }
    action_=(subpopAction)
  }
  //val actFunField = new TextField(actFun.toString)
  object printInfoSelector extends CheckBox { 
    selected_=(printInfo)
    object printAction extends Action("Display Info") {
      def apply : Unit = {
        if (printInfoSelector.selected) {
          reportArea.append("Now displaying info when evolving.\n")
          printInfo = true
        }
        else {
          reportArea.append("Silent mode selected.\n")
          printInfo = false
        }
        supervisor.printInfo(printInfo)
      }
    }
    action_=(printAction)
  }
  object threadsField extends TextField(numThreads.toString()) {
    object threadsAction extends Action("") {
      def apply : Unit = {
        try {
          numThreads = threadsField.text.toInt
          reportArea.append("Number of concurrent Evolvers is now "+numThreads+"\n")
          supervisor.setThreads(numThreads)
        } catch {
          case _ => reportArea.append("Could not parse that value.\n")
        }
      }
    }
    action_=(threadsAction)
  }
  //val defModeField = new TextField(defaultLearningMode.toString)
  object maxStepsField extends TextField(maxSteps.toString) {
    object stepsAction extends Action("") {
      def apply : Unit = {
        try {
          maxSteps = maxStepsField.text.toLong
          reportArea.append("Maximum number of steps is now "+maxSteps+"\n")
        } catch {
          case _ => reportArea.append("No luck parsing that value.\n")
        }
      }
    }
    action_=(stepsAction)
  }
  object saveDirSelector extends Button(saveDirectory) {
    object saveDirAction extends Action("") {
      def apply : Unit = {
        val dirChooser = new FileChooser
        dirChooser.fileSelectionMode_=(SelectionMode.DirectoriesOnly)
        val ok = dirChooser.showOpenDialog(saveDirSelector)
        if (ok.toString.equals("Approve")) {
          saveDirectory = dirChooser.selectedFile.toString
        }
        saveDirSelector.text_=(saveDirectory)
      }
    }
    action_=(saveDirAction)
  }
  object autoNormalize extends CheckBox {
    selected_=(normalizeData)
    object normalizationAction extends Action("AutoNormalize") {
      def apply : Unit = {
        normalizeData = autoNormalize.selected
        if (normalizeData) {
          reportArea.append("Autornomalizing loaded data.\n")
          dworker.normalizeData(true)
        }
        else {
          reportArea.append("Data will not be normalized automatically.\n")
          dworker.normalizeData(false)
        }
      }
    }
    action_=(normalizationAction)
  }
  object SaveOnExit extends CheckBox {
    selected_=(saveOnExit)
    object exitAction extends Action("SaveOnExit") {
      def apply : Unit = {
        saveOnExit = SaveOnExit.selected
        if (saveOnExit) {
          reportArea.append("Will save all evolvers when exiting training.\n")
          supervisor.setSaveOnExit(true)
        }
        else {
          reportArea.append("Won't save evolvers when stopping training.\n")
          supervisor.setSaveOnExit(false)
        }
      }
    }
    action_=(exitAction)
  }
  var useFullDataFeed = true
  object UseFullDataFeed extends CheckBox {
    selected_=(useFullDataFeed)
    object feedAction extends Action("Feed all data") {
      def apply : Unit = {
        useFullDataFeed = UseFullDataFeed.selected
        reportArea.append(if (useFullDataFeed) { "Using all data when training.\n" } else { "Using only part of data at beginning.\n" })
      }
    }
    action_=(feedAction)
  }
  var maxBlocks = 20
  object maxBlocksField extends TextField(maxBlocks.toString) {
    object maxBlocksAction extends Action("") {
      def apply : Unit = {
        try {
          maxBlocks = maxBlocksField.text.toInt
          reportArea.append("Maximum number of memory blocks is now: "+maxBlocks+"\n")
        } catch {
          case _ => reportArea.append("Enter that number again.\n")
        }
      }
    }
    action_=(maxBlocksAction)
  }
  var maxCells = 20
  object maxCellsField extends TextField(maxCells.toString) {
    object maxCellsAction extends Action("") {
      def apply : Unit = {
        try {
          maxCells = maxCellsField.text.toInt
          reportArea.append("Maximum number of memory cells is now: "+maxCells+"\n")
        } catch {
          case _ => reportArea.append("Enter that number again.\n")
        }
      }
    }
    action_=(maxCellsAction)
  }
  object washoutField extends TextField(washoutTime.toString) {
    object washoutAction extends Action("") {
      def apply : Unit = {
        try {
          washoutTime = washoutField.text.toInt
          reportArea.append("Washout time is now: "+washoutTime+"\n")
        } catch {
          case _ => reportArea.append("Enter new value for washout time.\n")
        }
      }
    }
  }
  //var cmeasure:CComplexityMeasure = new Constant(1d)
  //object cmeasureSelector extends ComboBox(Seq[String]("Constant","Variable"))
  object measureChooser extends ComboBox(Seq[String]("Constant","Simple","Total"))
  var measure:ComplexityMeasure = null
  val configurationReady = new Button("All Done")
  val functionChooser = new ComboBox(Seq[String]("Bipolar Sigmoid","Sigmoid","Ramping"))
  var bRNN:RNND = null //EvoLauncher.createRNN
  var netReady = false
  val confWindow = new Frame { title_=("Configure Parameters") }
  val writeBestNet = new MenuItem("Save Best Network")
  writeBestNet.enabled_=(false)
  var schedule:CoolingSchedule = new SimpleSchedule(mutProb,flipProb,maxSteps)
  val scheduleChooser = new ComboBox(Seq[String]("SimpleSchedule","AdaptiveSchedule"))
  val modes = Seq[String]("Basic ESP","ESP+","Evolino","SVMBoost","SVMLite")
  object modeSelector extends ComboBox(modes)
  val repopulatorSelector = new ComboBox(Seq[String]("BasicRP","ComplexRP","RandRP","MutatorRP"))
  var startedBefore = false
  

  
  def readConfig : Unit = {
    val cFile = new File(configPath)
    if (cFile.exists) {
      val e = XML.loadFile(cFile)
      try {
        dimX = (e \\ "DimensionX").text.toInt
      } catch {
        case _ => reportArea.append("Could not read DimensionX from config.\n")
      }
      try {
        dimY = (e \\ "DimensionY").text.toInt
      } catch {
        case _ => reportArea.append("Could not read DimensionY from config.\n")
      }
      try {
        numThreads = (e \\ "NumThreads").text.toInt
        threadsField.text_=(numThreads.toString)
      } catch {
        case _ => reportArea.append("Could not read numThreads from config.\n")
      }
      try {
        initialDistScale = (e \\ "InitialDistScale").text.toDouble
        scaleField.text_=(initialDistScale.toString)
      } catch {
        case _ => reportArea.append("Could not read InitialDistScala from config.\n")
      }
      //
      //popSize = (e \\ "PopSize").text.toInt
      try {
        mutProb = (e \\ "MutProb").text.toDouble
        mutProbField.text_=(mutProb.toString)
      } catch {
        case _ => reportArea.append("Could not read MutProb from config.\n")
      }
      try {
        flipProb = (e \\ "FlipProb").text.toDouble
        flipProbField.text_=(flipProb.toString)
      } catch {
        case _ => reportArea.append("Could not read flipProb from config.\n")
      }
      try {
        burstMutationFreq = (e \\ "BurstMutationFreq").text.toInt
        burstFreqField.text_=(burstMutationFreq.toString)
      } catch {
        case _ => reportArea.append("Could not read the frequency of burst mutations.\n")
      }
      try {
        autoSave = (e \\ "AutoSave").text.toInt
      } catch {
        case _ => reportArea.append("Using default autoSave frequency.\n")
      }
      try {
        val afun:String = (e \\ "ActFun").text
        afun match {
          case "SigmoidExp" => actFun = new SigmoidExp; functionChooser.item_=("SigmoidExp")
          case "BiSigmoid" => actFun = new OutFun; functionChooser.item_=("BiSigmoidExp")
          case "Ramping" => actFun = new RampingFun; functionChooser.item_=("Ramping")
          case _ => actFun = new SigmoidExp
        }
      } catch {
        case _ => reportArea.append("Could not read which Activation Function to use.\n")
      }
      try {
        saveDirectory = (e \\ "SaveDir").text
        saveDirSelector.text_=(saveDirectory)
      } catch {
        case _ => reportArea.append("Could not find saveDirectory.\n")
      }
      try { 
        maxSteps = (e \\ "MaxSteps").text.toLong
        maxStepsField.text_=(maxSteps.toString)
      } catch {
        case _ => reportArea.append("Could not read the value of MaxSteps from config.\n")
      }
      try {
        val srep = (e \\ "CSchedule").text
        srep match {
          case "SimpleSchedule" => schedule = new SimpleSchedule(mutProb,flipProb,maxSteps); scheduleChooser.selection.index_=(0)
          case "AdaptiveSchedule" => schedule = new AdaptiveSchedule(mutProb,flipProb,maxSteps); scheduleChooser.selection.index_=(1)//item_=("AdaptiveSchedule")
          case _ => reportArea.append("Could not determine what Cooling Schedule to use.\n")
        }
      } catch {
        case _ => reportArea.append("Could not determine what Cooling Schedule to use.\n")
      }
      try {
        printInfo = (e \\ "PrintInfo").text.toBoolean
        if (printInfo) {
          printInfoSelector.selected_=(true)
        }
      } catch {
        case _ => reportArea.append("Could not determine whether to print info all the time or not.\n")
      }
      try {
        val lMode = (e \\ "LearningMode").text
        lMode match {
          case "Basic ESP" => modeSelector.selection.index_=(0)
          case "ESP+" => modeSelector.selection.index_=(1)
          case "Evolino" => modeSelector.selection.index_=(2)
          case "SVMBoost" => modeSelector.selection.index_=(3)
          case "SVMLite" => modeSelector.selection.index_=(4)
          case _ => modeSelector.selection.index_=(0)
        }
      } catch {
        case _ => reportArea.append("Using default learning mode.\n")
      }
      try {
        popSize = (e \\ "SubpopulationSize").text.toInt
        popSizeSlider.value_=(popSize)
      } catch {
        case _ => reportArea.append("Using default subpopulation size.\n")
      }
      try {
        normalizeData = (e \\ "AutoNormalize").text.toBoolean
      } catch {
        case _ => reportArea.append("Could not find the value of AutoNormalize.\n")
      }
      try {
        spawnFreq = (e \\ "SpawnFreq").text.toInt
      } catch {
        case _ => reportArea.append("Could not read the value of SpawnFreq.\n")
      }
      try {
        discardRate = (e \\ "DiscardRate").text.toDouble
      } catch {
        case _ => reportArea.append("Could not read the value of Discard Rate.\n")
      }
      try {
        maxPopSize = (e \\ "MaxPopSize").text.toInt
        popSizeSlider.max_=(maxPopSize)
      } catch {
        case _ => reportArea.append("Could not read the value of maximum subpopulation size.\n")
      }
      try {
        saveOnExit = (e \\ "SaveOnExit").text.toBoolean
        SaveOnExit.selected_=(saveOnExit)
      } catch {
        case _ => reportArea.append("Could not determine whether to save evolvers on exit.\n")
      }
      try {
        val selector = (e \\ "Repopulator").text
        selector match {
          case "ComplexRP" => {
            repopulatorSelector.selection.index_=(1)
          }
          case "RandRP" => {
            repopulatorSelector.selection.index_=(2)
          }
          case "MutatorRP" => {
            repopulatorSelector.selection.index_=(3)
          }
          case _ => Unit
        }
      } catch {
        case _ => reportArea.append("Using default repopulator.\n")
      }
      try {
        useFullDataFeed = (e \\ "FeedAllData").text.toBoolean
      } catch {
        case _ => reportArea.append("Could not determine whether to use all data at each step or not.\n")
      }
      try {
        val aux = (e \\ "InitialFwdConnections").text
        if (aux.length > 0) {
          initialFwdConnections = aux.toInt
        }
        val aux2 = (e \\ "InitialRecConnections").text
        if (aux2.length > 0) {
          initialRecConnections = aux2.toInt
        }
      } catch {
        case _ => {}
      }
      try {
        initialCells = (e \\ "InitCells").text.toInt
        initialBlocks = (e \\ "InitBlocks").text.toInt
      } catch {
        case _ => {}
      }
      try {
        mixingFreq = (e \\ "MixingFreq").text.toInt
      } catch {
        case _ => { reportArea.append("Could not parse the value of MixingFreq.\n") }
      }
      try {
        mixingProb = (e \\ "MixingProb").text.toDouble
        
      } catch {
        case _ => { reportArea.append("Could not parse the value of MixingProb.\n")}
      }
      try {
        maxBlocks = (e \\ "MaxBlocks").text.toInt
        maxBlocksField.text_=(maxBlocks.toString)
      } catch {
        case _ => { reportArea.append("Could not parse the maximum number of memory blocks.\n")}
      }
      try {
        maxCells = (e \\ "MaxCells").text.toInt
        maxCellsField.text_=(maxCells.toString)
      } catch {
        case _ => { reportArea.append("Could not parse the maximum number of memory cells.\n")}
      }
      try {
        washoutTime = (e \\ "WashoutTime").text.toInt
        washoutField.text_=(washoutTime.toString)
      } catch {
        case _ => { reportArea.append("Using the default value for washout period.\n") }
      }
      try {
        val m = (e \\ "ComplexityMeasure").text
        m match {
          case "Simple" => measureChooser.item_=("Simple")
          case "Total" => measureChooser.item_=("Total")
          case _ => measureChooser.item_=("Constant")
        }
      } catch {
        case _ => reportArea.append("Could not read which complexity measure should be used.\n")
      }
    }
    else {
      reportArea.append("Could not read the config file.\n")
      reportArea.append("You can save your config after setting the parameters.\n")
    }
  }
  def writeConfig : Boolean = {
    val e1 = new Array[Elem](31)
    e1(0) = <DimensionX>{dimX}</DimensionX>
    e1(1) = <DimensionY>{dimY}</DimensionY>
    e1(10) = <NumThreads>{numThreads}</NumThreads>
    e1(2) = <InitialDistScale>{initialDistScale}</InitialDistScale>
    e1(3) = <AutoSave>{autoSave}</AutoSave>
    e1(4) = <PopSize>{popSize}</PopSize>
    e1(5) = <MutProb>{mutProb}</MutProb>
    e1(6) = <FlipProb>{flipProb}</FlipProb>
    e1(7) = <BurstMutationFreq>{burstMutationFreq}</BurstMutationFreq>
    e1(8) = <ActFun>{functionChooser.selection.item}</ActFun>
    e1(9) = <PrintInfo>{printInfo}</PrintInfo>
    e1(11) = <LearningMode>{modeSelector.selection.item}</LearningMode>
    e1(12) = <MaxSteps>{maxSteps}</MaxSteps>
    e1(13) = <SaveDir>{saveDirectory}</SaveDir>
    e1(14) = <CSchedule>{scheduleChooser.selection.item}</CSchedule>
    e1(15) = <SubpopulationSize>{popSizeSlider.value}</SubpopulationSize>
    e1(16) = <AutoNormalize>{normalizeData}</AutoNormalize>
    e1(17) = <SpawnFreq>{spawnFreq}</SpawnFreq>
    e1(18) = <DiscardRate>{discardRate}</DiscardRate>
    e1(19) = <MaxPopSize>{maxPopSize}</MaxPopSize>
    e1(20) = <SaveOnExit>{saveOnExit}</SaveOnExit>
    e1(21) = <Repopulator>{repopulatorSelector.selection.item}</Repopulator>
    e1(22) = <FeedAllData>{useFullDataFeed}</FeedAllData>
    e1(23) = <InitCells>{initialCells}</InitCells>
    e1(24) = <InitBlocks>{initialBlocks}</InitBlocks>
    e1(25) = <MixingFreq>{mixingFreq}</MixingFreq>
    e1(26) = <MixingProb>{mixingProb}</MixingProb>
    e1(27) = <MaxBlocks>{maxBlocks}</MaxBlocks>
    e1(28) = <MaxCells>{maxCells}</MaxCells>
    e1(29) = <WashoutTime>{washoutTime}</WashoutTime>
    e1(30) = <ComplexityMeasure>{measureChooser.selection.item}</ComplexityMeasure>
    //e1(15) =
    val xrep = <EvolverConfig>{for (i <- 0 until e1.length) yield e1(i)}</EvolverConfig>
    val f = new File(configPath)
    try {
    if (f.exists) {
      f.delete
      reportArea.append("Replacing the old config file with the new configuration.\n")
      XML.save(configPath,xrep,"UTF-8")
    }
    else {
      //val fw = new FileWriter(f)
      XML.save(configPath,xrep,"UTF-8")
      reportArea.append("Saving the configuration...\n")
    }
    return true
    } catch {
      case _ => reportArea.append("Error in writing the configuration.\n"); false
    }
  }
  def configure : Unit = {
    val cnfPanel = new GridPanel(13,4) {
      contents += new Label("Threads:")
      contents += threadsField

      contents += new Label("Dist Scale:")
      contents += scaleField
      contents += new Label("Save Freq")
      contents += autoSaveField
      contents += new Label("Mut prob:")
      contents += mutProbField
      contents += new Label("Flip prob:")
      contents += flipProbField
      contents += new Label("Burst Freq")
      contents += burstFreqField

      contents += new Label("Max Steps:")
      contents += maxStepsField

      contents += new Label("SpawnFreq:")
      contents += spawnFreqField//new Label("")

      contents += new Label("SaveDir:")
      contents += saveDirSelector
      contents += new Label("DiscardRate:")
      contents += discardRateField
      contents += new Label("MaxPopSize:")
      contents += subpopField
      contents += new Label("MemBlocks:")
      contents += memoryBlockField
      contents += new Label("MemCells:")
      contents += memoryCellField
      contents += new Label("Act Fun:")
      contents += functionChooser//actFunField
      contents += new Label("Cooling Schedule:")
      contents += scheduleChooser
      contents += new Label("Repopulator:")
      contents += repopulatorSelector
      contents += printInfoSelector
      contents += autoNormalize
      contents += SaveOnExit
      contents += UseFullDataFeed
      contents += new Label("MixingFreq:")
      contents += mixingFreqField //new Label("")
      contents += new Label("MixingProb:")
      contents += mixingProbField
      contents += new Label("Max Blocks:")
      contents += maxBlocksField
      contents += new Label("Max Cells:")
      contents += maxCellsField
      contents += new Label("WashoutTime:")
      contents += washoutField
      
      contents += new Label("CMeasure:")
      contents += measureChooser
      contents += new Label("")
      contents += new Label("")
      contents += new Label("")
      contents += configurationReady
    }
    cnfPanel.border_=(Swing.EtchedBorder(Swing.Raised,rCol,sCol))
    confWindow.contents_=(cnfPanel)
    
    confWindow.pack
    confWindow.visible_=(true)
  }
  def initSupervisor : Unit = {
    printInfo = printInfoSelector.selected
    supervisor.setPrintInfo(printInfo)
    supervisor.setSaveOnExit(saveOnExit)
    supervisor.setMixingParameters(mixingFreq,mixingProb)
    supervisor.setMaximumSize(maxCells,maxBlocks)
    if (supervisor.getNumberOfEvolvers != numThreads) {
      supervisor.setThreads(numThreads)
    }
    supervisor.setWashoutTime(washoutTime)
    measure = measureChooser.selection.item match {
      case "Total" => new TotalMeasure
      case "Constant" => new SimpleMeasure
      case _ => new ConstantMeasure
    }
    
    supervisor.setComplexityMeasure(measure)
  }
  def initPopulation() : Unit = {
    dims = dworker.getDims
    if (normalizeData) {
      dworker.normalizeAll
    }
    popSize = popSizeSlider.value
    val allPops = new Array[CellPopulationD](numThreads)
    val allNets = new Array[NetPopulationD](numThreads)
    val allEvolvers = new Array[NeuralEvolver](numThreads)
    //val rnd = new Random
    initSupervisor
    //supervisor.setThreads(numThreads)
    val scheduleRep = scheduleChooser.selection.item
    var schedule:CoolingSchedule = new SimpleSchedule(mutProb,flipProb,maxSteps)
    var populator1:Repopulator[CellPopulationD] = new BasicRepopulator
    var populator2:NetRepopulator[NetPopulationD,CellPopulationD] = new SimpleNetRepopulator
    val populatorRep = repopulatorSelector.selection.item
    populatorRep match {
        case "ComplexRP" => populator1 = new ComplexRepopulator(0.75); populator2 = new VariableNetRepopulator(0.8)
        case "RandRP" => populator1 = new RandCellRepopulator(0.75); populator2 = new VariableNetRepopulator(0.8)
        case _ => Unit
    }
    scheduleRep match {
      case "AdaptiveSchedule" => schedule = new AdaptiveSchedule(mutProb,flipProb,maxSteps)
      case _ => Unit
    }
    if (modes.indexOf(modeSelector.selection.item) >= 3) {
      //SVMBoost
      dworker.initSVM
    }
    val dsizes = new Array[Int](if (dworker.getCount < 5) dworker.getCount else 4) //holds the lengths of data arrays
    for (i <- 0 until dsizes.length) { dsizes(i) = dworker.getData(i).size }
    schedule.setSizes(dsizes)
    if (!useFullDataFeed) {
      reportArea.append("Caution! Not using the whole data set at the beginning.\n")
    }
    var measure:ComplexityMeasure = new SimpleMeasure
    if (measureChooser.selection.item == "Total") {
      measure = new TotalMeasure
    }
    else if (measureChooser.selection.item == "Constant") {
      measure = new ConstantMeasure
    }
    supervisor.setComplexityMeasure(measure)
    for (i <- 0 until numThreads) {
      /*
      allPops(i) = new CellPopulationD(dims(0),if (i % 2 == 0) 1 else 2,dims(1),popSize)
      allPops(i).init(initialDistScale,1,rnd)
      allNets(i) = new NetPopulationD(allPops(i))
      allNets(i).init
      allEvolvers(i) = new NeuralEvolver(allPops(i),allNets(i),supervisor,supervisor.getReporter,rnd,discardRate)
      */
      allEvolvers(i) = NeuralEvolver.makeEvolver(initialBlocks,initialCells,dims(0),dims(1),popSize,initialDistScale,supervisor,supervisor.getReporter)
      allEvolvers(i).addDLists(dworker.getDLists)
      if (modes.indexOf(modeSelector.selection.item) >= 3) {
        allEvolvers(i).initSVMLearner(dworker.getCols(0),regressionMode)//dworker.getNodes(1),
      }
      allEvolvers(i).setActFun(actFun)
      allEvolvers(i).setBurstFreq(burstMutationFreq)
      allEvolvers(i).setPrintInfo(printInfo)
      allEvolvers(i).setUseFullDataFeed(useFullDataFeed)
      allEvolvers(i).setEvoMode(modes.indexOf(modeSelector.selection.item))
      allEvolvers(i).setRepopulator(populator1)
      allEvolvers(i).setNetRepopulator(populator2)
      allEvolvers(i).setSpawningFreq(spawnFreq)
      allEvolvers(i).setSchedule(schedule.makeClone)
      allEvolvers(i).setSavePath(saveDirectory)
      allEvolvers(i).setSaveFreq(autoSave)
      
      /*
      if (evolutionMode == 2) {
        allEvolvers(i).addData2(dworker.getAsList(2),dworker.getAsList(3))
      }
      */
      //allEvolvers(i).setMutationProb(mutProb)
      //allEvolvers(i).setFlipProb(flipProb)
      //reportArea.append(allEvolvers(i).getSimpleRepresentation+"\n")
      allEvolvers(i).start
      supervisor.addEvolver(i,allEvolvers(i))
    }
    //supervisor.setThreads(numThreads)
	supervisor.start
	supervisor ! "Start" //UpdateNow(0L)
	writeBestNet.enabled_=(true)
  }
  def initEvolvers : Unit = {
    if (startedBefore) {
      supervisor.reset
    }
    else {
      dworker.normalizeAll
    }
    initSupervisor
    //supervisor.setThreads(numThreads)
    reportArea.append("Initializing the evolutionary procedure using restored populations.\n")
    var id = 0
    val scheduleRep = scheduleChooser.selection.item
    var schedule:CoolingSchedule = new SimpleSchedule(mutProb,flipProb,maxSteps)
    var populator1:Repopulator[CellPopulationD] = new BasicRepopulator
    var populator2:NetRepopulator[NetPopulationD,CellPopulationD] = new SimpleNetRepopulator
    val populatorRep = repopulatorSelector.selection.item
    populatorRep match {
      case "ComplexRP" => populator1 = new ComplexRepopulator(0.75); populator2 = new VariableNetRepopulator(0.8)
      case "RandRP" => populator1 = new RandCellRepopulator(0.75); populator2 = new VariableNetRepopulator(0.8)
      case "MutatorRP" => populator1 = new CellmutatorRepopulator(0.75); populator2 = new VariableNetRepopulator(0.8)
      case _ => Unit
    }
    scheduleRep match {
      case "AdaptiveSchedule" => schedule = new AdaptiveSchedule(mutProb,flipProb,maxSteps)
      case _ => Unit
    }
    if (!useFullDataFeed) {
      reportArea.append("Caution! Not using the whole data set at the beginning.\n")
    }
    for (i <- restoredCount until numThreads) {
      val pop0 = evolvers.apply(0).getCellPop
      val npop0 = evolvers.apply(0).getNetPop
      val pop1 = pop0.burstMutate2(0.1,new CauchyDistribution(0.01),rnd)
      val npop1 = npop0.burstMutate(rnd)
      val evoX = new NeuralEvolver(pop1,npop1,supervisor,supervisor.getReporter,rnd,discardRate)
      evolvers = evolvers.:+(evoX)
      reportArea.append("Added another mutated Evolver.\n")
    }
    if (modes.indexOf(modeSelector.selection.item) >= 3) {
      dworker.initSVM
    }
    for (e <- evolvers) {
      e.addDLists(dworker.getDLists)
      e.setActFun(actFun)
      e.setBurstFreq(burstMutationFreq)
      e.setPrintInfo(printInfo)
      e.setEvoMode(modes.indexOf(modeSelector.selection.item))
      if (modes.indexOf(modeSelector.selection.item) >= 3) {
        e.initSVMLearner(dworker.getCols(0),regressionMode)//dworker.getNodes(1),
      }
      e.setUseFullDataFeed(useFullDataFeed)
      e.setRepopulator(populator1)
      e.setNetRepopulator(populator2)
      e.setSpawningFreq(spawnFreq)
      e.setSchedule(schedule.makeClone)
      e.setSavePath(saveDirectory)
      e.setSaveFreq(autoSave)
      e.start
      supervisor.addEvolver(id,e)
      id += 1
    }
    //supervisor.setThreads(numThreads)
    supervisor.start
    supervisor ! "Start"
    
  }
  def init : Unit = {
    val frame = new MainFrame()
	frame.pack
	frame.open
	}
  def top = new MainFrame {
	title = "NeuroGenesisInterface"
	object fChooser extends FileChooser { multiSelectionEnabled_=(true) }
	object openFiles extends Button { text_=("Select Data") }
	readConfig //
	val sizeField = new TextField("Subpopulation size: "+popSizeSlider.value)
	sizeField.editable_=(false)
	
	contents = new GridPanel(1,2) {
	  val lpane1 = new GridPanel(7,1) {
	    contents += openFiles
	    contents += dataCounter
	    contents += sizeField
	    contents += popSizeSlider
	    contents += modeSelector
	    contents += start
	    contents += fitnessLabel
	    border = Swing.EtchedBorder(Swing.Raised,rCol,sCol)
	  }
	  val lpane2 = new SplitPane(Orientation.Horizontal,lpane1,new ScrollPane(dataPanel))
	  lpane2 //.border_=(Swing.EtchedBorder)
	  val rpane = new GridPanel(1,1) {
	    contents += new ScrollPane(reportArea)
	  }
	  val splitPane = new SplitPane(Orientation.Vertical,lpane2,rpane)
	  splitPane.border_=(Swing.EtchedBorder)
	  contents += splitPane
	}
	val generateData = new MenuItem("Generate Data") { enabled_=(true) }
	val stopEvolution = new MenuItem("Stop!") { enabled_=(false) }
	val restartEvolution = new MenuItem("Restart") { enabled_=(true) }
	val plotData = new MenuItem("Plot Data") { enabled_=(false) }
	val runLeastSquares = new MenuItem("Test Linear Regression") { enabled_=(false) }
	val runSVMRegression = new MenuItem("Test SVM Regression") { enabled_=(false) }
	
	val calculateValidationError = new MenuItem("Validate best solution") { enabled_=(false)}
	val writeConfigNow = new MenuItem("Write Config")
	val writeAllTheBestNets = new MenuItem("Save RNNs") { enabled_=(false) }
	val restoreRNNs = new MenuItem("Read RNNs")
	val configureNow = new MenuItem("Configure")

	val readMatrix = new MenuItem("Read Matrix")
	val readRNN = new MenuItem("Read RNN")
	val readEvolver = new MenuItem("Read Evolver")
	val readData2 = new MenuItem("Read data(*Single file)")
	//val autoSaveButton = new CheckMenuItem("AutoSave")
	val displayNet = new MenuItem("Display Best RNN") { enabled_=(false) }
	val layoutSelector = new ComboBox(Seq[String]("RNN","FR","KK","ISOM","Spring"))
	val layoutOK = new Button("Draw!")
	val clearReportArea = new MenuItem("Clear Messages")
	val clearData = new MenuItem("Clear data!") { enabled_=(false) }
	val debugRun = new MenuItem("Test RNND") { enabled_=(false) }
	val runDiagnostics = new MenuItem("Run Diagnostics") { enabled_=(false) }
	val writeBestPopulation = new MenuItem("Write Best Population!") { enabled_=(false) }
	val lFrame = new Frame // used when selecting the layout of the net that will be displayed
	
	val questionWindow = new Frame
	val lengthSlider = new Slider
	lengthSlider.max_=(10000)
	lengthSlider.min_=(1000)
	lengthSlider.value_=(1000)
	lengthSlider.enabled_=(false)
	var dataLength = 1000
	val lengthLabel = new Label("Data length: "+dataLength)
	
	val mBar = new MenuBar() {
	  contents += new Menu("File") {
	    
	    //contents += readMatrix
	    contents += readRNN
	    contents += readEvolver
	    contents += restoreRNNs
	    contents += new Separator
	    contents += writeBestNet
	    contents += writeAllTheBestNets
	    contents += writeConfigNow
	    contents += new Separator
	    contents += clearData
	    contents += new Separator
	    contents += readData2
	  }
	  contents += new Menu("Util") {
	    
	    
	    //contents += new Separator
	    contents += clearReportArea
	    contents += new Separator
	    contents += displayNet
	    contents += plotData
	    contents += makePredictions
	    contents += new Separator
	    
	    contents += new Separator
	    contents += runLeastSquares
	    contents += runSVMRegression
	    contents += new Separator
	    contents += calculateValidationError
	    contents += new Separator
	    contents += debugRun
	    contents += new Separator
	    contents += generateData
	    contents += new Separator
	    contents += runDiagnostics
	  }
	  contents += new Menu("Config") {
	    contents += configureNow
	  }
	  contents += new Menu("Control") {
	    contents += stopEvolution
	    contents += new Separator
	    //contents += restartEvolution //the supervisor seemed to halt sometimes.. this tells it to go on again
	    
	    
	  }
	  //name_=()
	}
	menuBar_=(mBar)
	minimumSize_=(new Dimension(dimX,dimY))
	listenTo(openFiles,start,stopEvolution,popSizeSlider,plotData,runDiagnostics,readData2,
	    configureNow,writeConfigNow,readRNN,readEvolver,restoreRNNs,generateData,modeSelector.selection,
	    configurationReady,calculateValidationError,runSVMRegression,scheduleChooser.selection,
	    writeBestNet,displayNet,runLeastSquares,clearReportArea,functionChooser.selection,
	    makePredictions,clearData,repopulatorSelector,writeBestPopulation,measureChooser.selection,
	    debugRun,restartEvolution,lengthSlider)
	//listenTo(configurationReady,saveDirField,maxStepsField,functionChooser)
	//listenTo(writeBestNet,displayNet,modeSelector)
	var networksDrawn = 0
	var netList:List[RNND] = List[RNND]()
	
	dworker.start()
	iWorker.start
	reactions += {
	  case ButtonClicked(`openFiles`) => {
	    val reval = fChooser.showOpenDialog(contents.head)
	    if (reval.toString.equals("Approve")) {
	      //dworker.readDoubles(fChooser.selectedFile)
	      dworker ! LoadData(fChooser.selectedFiles)
	      plotData.enabled_=(true)
	      clearData.enabled_=(true)
	      generateData.enabled=(false)
	      if (netReady) {
	        makePredictions.enabled_=(true)
	      }
	    }
	    
	  }
	  case ButtonClicked(`readData2`) => {
	    val reval = fChooser.showOpenDialog(contents.head)
	    if (reval.toString.equals("Approve")) {
	      dworker.setDivideData(true)
	      dworker ! LoadData(fChooser.selectedFiles)
	      plotData.enabled_=(true)
	      clearData.enabled_=(true)
	      generateData.enabled=(false)
	      if (netReady) {
	        makePredictions.enabled_=(true)
	      }
	      reportArea.append("Beware! Creating the whole data set from a single data array.\n")
	      reportArea.append("Use 'Select Data' if this is not what you want.\n")
	    }
	  }
	  case ButtonClicked(`generateData`) => {
	    lengthSlider.enabled_=(true)
        object lengthAction extends Action("Generate Now!") {
          //title_=
          def apply : Unit = {
            dworker.generateData(lengthSlider.value)
            start.enabled_=(true)
            clearData.enabled_=(true)
            openFiles.enabled_=(false)
            plotData.enabled_=(true)
            if (netReady) {
              makePredictions.enabled_=(true)
            }
            questionWindow.close
            lengthSlider.enabled_=(false)
            generateData.enabled_=(false)
            reportArea.append("Generating simple data for testing purposes...\n")
          }
        }
        val qpanel1 = new GridPanel(2,1) {
	      contents += lengthLabel
	      contents += lengthSlider          
        }
	    val questionPanel = new GridPanel(1,2) {
	      contents += qpanel1
	      contents += new Button(lengthAction)
	    }
	    questionWindow.contents_=(questionPanel)
	    questionWindow.pack
	    questionWindow.visible_=(true)

	  }
	  case ValueChanged(`lengthSlider`) => {
	    lengthLabel.text_=("Data length: "+lengthSlider.value)
	  }
	  /*
	  case ButtonClicked(`readMatrix`) => {
	    val reval = fChooser.showOpenDialog(contents.first)
	    if (reval.toString.equals("Approve")) {
	      dworker.readMatrix(fChooser.selectedFile)
	    }
	  }
	  */
	  case ButtonClicked(`start`) => {
        if (modeSelector.selection.item == "Evolino" && dworker.getCount < 4) {
          reportArea.append("Evolino requires 4 data arrays.\n" + "Please load more data.\n")
        } 
        else {
          if (!startedBefore) {
            if (evolverInMemory) {
              initEvolvers
            } else {
              initPopulation
            }

            startedBefore = true
            start.enabled_=(false)
            stopEvolution.enabled_=(true)
            if (dworker.getCount > 4 || (modes.indexOf(modeSelector.selection.item) < 2 && dworker.getCount > 2)) {
              makePredictions.enabled_=(true)
            }
          } else {
            fitnessLabel.text_=("0")
            supervisor = new EvolutionSupervisor(reportArea, fitnessLabel, numThreads)
            if (evolverInMemory) {
              initEvolvers
            } else {
              initPopulation
            }
            reportArea.append("Started with a fresh population.\n")
            start.enabled_=(false)
            writeBestPopulation.enabled_=(false)
            writeAllTheBestNets.enabled_=(false)
            runDiagnostics.enabled_=(false)
          }
        }
	    restartEvolution.enabled_=(true)
	  }
	  case ButtonClicked(`restartEvolution`) => {
	    supervisor.restart2
	  }
	  case ButtonClicked(`readEvolver`) => {
	    val reval = fChooser.showOpenDialog(contents.head)
	    if (reval.toString.equals("Approve")) {
	      restoreEvolver(fChooser.selectedFile)
	    }
	  }
	  case ButtonClicked(`restoreRNNs`) => {
	    val reval = fChooser.showOpenDialog(contents.head)
	    if (reval.toString.equals("Approve")) {
	      val c = loadRNNs(fChooser.selectedFile)
	      reportArea.append("Total number of restored networks: "+c+"\n")
	    }
	  }
	  case ButtonClicked(`runLeastSquares`) => {
	    val bestRNN = supervisor.getSuperStar
	    bestRNN.reset
	    val trData1 = bestRNN.evolinoFeed(dworker.getData(0),actFun,washoutTime)
	    var trData2 = bestRNN.evolinoFeed(dworker.getData(2),actFun,0)
	    NeuralOps.runLinearRegression(trData1,dworker.getData(1).drop(washoutTime),trData2,dworker.getData(3),reportArea)
	    /*
	    val rnnRep = bestRNN.toXML
	    reportArea.append("\n"+"Used network representation: \n")
	    prettyPrint(rnnRep)
	    */
	  }
	  case ButtonClicked(`runSVMRegression`) => {
	    dworker.initSVM
	    
	    val bestRNN = supervisor.getSuperStar
	    bestRNN.reset
	    val svmParam = getSVMParameter(bestRNN)
        val svmCols = dworker.getCols(0)
        val results = bestRNN.svmRegression(dworker.getData(0),svmCols,actFun,svmParam,dworker.getData(2))
        val q = dworker.getData(3).toArray
        NeuralOps.plotResults(results,q)
        
	  }
	  case ValueChanged(`popSizeSlider`) => {
	    sizeField.text_=("Subpopulation size: "+popSizeSlider.value)
	  }
	  case ButtonClicked(`stopEvolution`) => {
	    supervisor ! "Exit"
	    reportArea.append("Sending messages to all evolvers so that they will stop after finishing the current step.\n")
	    reportArea.append("This may take some time...\n")
	    start.text_=("Start Again!")
	    start.enabled_=(true)
	    calculateValidationError.enabled_=(true)
	    writeBestPopulation.enabled_=(true)
	    displayNet.enabled_=(true)
	    writeBestNet.enabled_=(true)
	    if (dworker.getCount > 3) {
	      runSVMRegression.enabled_=(true)
	    }
	    runLeastSquares.enabled_=(true)
	    writeAllTheBestNets.enabled_=(true)
	    makePredictions.enabled_=(true)
	    listenTo(writeAllTheBestNets)
	    runDiagnostics.enabled_=(true)
	  }
	  case ButtonClicked(`clearData`) => {
	    dworker.removeAllData
	    clearData.enabled_=(false)
	    start.enabled_=(false)
	    runLeastSquares.enabled_=(false)
	    calculateValidationError.enabled_=(false)
	    makePredictions.enabled_=(false)
	    dataCounter.text_=("Data arrays: 0")
	    iWorker.reset
	    dataPanel.text_=("")
	    openFiles.enabled_=(true)
	    generateData.enabled_=(true)
	    dworker.setDivideData(false)
	  }
	  case ButtonClicked(`calculateValidationError`) => {
	    val bestRNN = supervisor.getSuperStar.makeClone
	    bestRNN.feedData(dworker.getData(0),actFun)
	    val res1 = bestRNN.feedData(dworker.getData(2),actFun)
	    reportArea.append("NOTE: Not yet implemented correctly if you are using either Evolino or SVMBoost.\n")
	    reportArea.append("Validation error was: "+NeuralOps.totalError(dworker.getData(3),res1.toList)+"\n")
	    dworker ! AnotherArray(res1)
	  }
	  case ButtonClicked(`makePredictions`) => {
	    evolutionMode = modeSelector.selection.index
	    //println(evolutionMode)
	    predict(dworker.getCount-1)
	  }
	  case ButtonClicked(`clearReportArea`) => {
	    reportArea.text_=("All clear!\n")
	  }
	  case ButtonClicked(`runDiagnostics`) => {
	    val dvals = supervisor.runDiagnostics
	    for (v <- dvals) {
	      reportArea.append("Diagnostic results: "+v+"\n")
	    }
	  }
	  case ButtonClicked(`writeBestNet`) => {
	    writeSuperStar("superstar",true) //Write in XML
	  }
	  case ButtonClicked(`writeAllTheBestNets`) => {
	    val bunch = supervisor.gatherBest
	    if (writeAllRNNs(bunch)) {
	      reportArea.append("Wrote the networks.\n")
	    }
	    else {
	      reportArea.append("Did not write the networks because a previous save existed with the same name.\n")
	    }
	  }
	  case SelectionChanged(`functionChooser`) => {
	    val s = functionChooser.selection.item
	    s match {
	      case "Bipolar Sigmoid" => actFun = new OutFun
	      case "Sigmoid" => actFun = new SigmoidExp
	      case "Ramping" => actFun = new RampingFun
	      case _ => actFun = new SigmoidExp
	    }
	    reportArea.append("Changed actFun to: "+actFun.toString+"\n")
	  }
	  case SelectionChanged(`scheduleChooser`) => {
	    val s = scheduleChooser.selection.item
	    s match {
	      case "SimpleSchedule" => schedule = new SimpleSchedule(mutProb,flipProb,maxSteps)
	      case "AdaptiveSchedule" => schedule = new AdaptiveSchedule(mutProb,flipProb,maxSteps)
	      case _ => reportArea.append("Error while changing the Cooling Schedule.\n")
	    }
	  }
	  case SelectionChanged(`modeSelector`) => {
	    reportArea.append("Selected mode is now: "+modeSelector.selection.item+"\n")
	  }
	  case SelectionChanged(`measureChooser`) => {
	    reportArea.append("Complexity measure is now: "+measureChooser.selection.item+"\n")
	  }
	  case ButtonClicked(`displayNet`) => {
	    //displayBestNet using the Jung library but first creates a window used to choose the layout algorithm
	    val loPane = new GridPanel(2,1) {
	      contents += new GridPanel(1,2) {
	        contents += new Label("Layout:")
	        contents += layoutSelector
	      }
	      contents += layoutOK
	    }
	    listenTo(layoutOK,layoutSelector)
	    lFrame.contents_=(loPane)
	    lFrame.minimumSize_=(new Dimension(200,100))
	    lFrame.pack
	    lFrame.visible_=(true)
	  }
	  case ButtonClicked(`layoutOK`) => {
	    val gWorker = new GraphWorker(layoutSelector.selection.item)
	    gWorker.start
	    if (networksDrawn == 0) {
	      netList = netList ++ supervisor.gatherBest
	      netList = netList.sortWith(_.getFitness > _.getFitness)
	    }
	    val rnn = netList.apply(networksDrawn)
	    gWorker ! AnotherRNN(rnn)
	    prettyPrint(rnn.toXML)
	    networksDrawn += 1
	    layoutOK.text_=("Draw next!")
	    if (networksDrawn == netList.size) {
	      lFrame.close()
	      layoutOK.text_=("Draw!")
	    }
	    
	    //
	  }

	  case ButtonClicked(`plotData`) => {
	    //dworker.plotColumns
	    val pFrame:Frame = new Frame { 
	      title_=("SelectionFrame")
	    }
	    val dCount = dworker.getCount
	    val seBox = new ComboBox(Range(0,dCount,1).toSeq)
	    val plotNow = new Button("PlotNow!") {
	        object plotNowAction extends Action("PlotNow!") {
	          def apply : Unit = {
	            dworker.makeSubplots(seBox.selection.item,pFrame)
	          }
	        }
	        action_=(plotNowAction)
	    }
	    pFrame.contents_=(new GridPanel(2,1) {
	      contents += new GridPanel(1,2) {
	        contents += new Label("Array:")
	        contents += seBox
	      }
	      contents += plotNow
	    })
	    pFrame.preferredSize_=(new Dimension(260,160))
	    pFrame.centerOnScreen()
	    pFrame.pack
	    pFrame.visible_=(true)
	  }
	  case ButtonClicked(`writeConfigNow`) => {
	    writeConfig
	  }
	  case ButtonClicked(`configurationReady`) => {
	    confWindow.close
	  }
	  case ButtonClicked(`configureNow`) => {
	    configure
	  }
	  
	  case ButtonClicked(`readRNN`) => {
	    reportArea.append("Trying to read a RNN.\n")
	    val reval = fChooser.showOpenDialog(contents.head)
	    if (reval.toString.equals("Approve")) {
	      restoreRNND(fChooser.selectedFile)
	      debugRun.enabled_=(true)
	    }

	    else {
	      reportArea.append("Cancelled the procedure for loading an xml representation of a neural net.\n")
	    }
	  }
	  case ButtonClicked(`debugRun`) => {
	    def debugRun(l:List[Array[Double]],rnn:RNND) : Unit = {
	      object f extends Function1[Double,Double] {
            def apply(x:Double) : Double = { val y = 2/(1+scala.math.exp(-x))-1; y }
          }
          def easyPrint(a:Array[Double]) : Unit = { for (i <- 0 until a.length) { reportArea.append(a(i).toString+" ")}; reportArea.append("\n")}
          reportArea.append("Running a basic test to see how the network works.\n")
          reportArea.append("First the results obtained by feeding the first data set to the RNN:\n")
          for (item <- l) {
            easyPrint(rnn.activate(item,f))
          }
          reportArea.append("Evolino results: \n")
          for (item <- l) {
            easyPrint(rnn.evolinoActivate(item,f))
          }
	    }
	    if (dworker.getCount > 0) {
	      debugRun(dworker.getData(0),bRNN)
	    }
	  }
	}
  }

  def predict(idx:Int) : Unit = {
    val rnn = if (!netReady) supervisor.getSuperStar else bRNN
    if (netReady) {
      reportArea.append("Using a restored network in prediction...\n")
    }
    if (rnn != null) {
    var j = 0
    var res = List[Array[Array[Double]]]() //
    rnn.reset
    val resArea = new TextArea
    def append2Area(resA:Array[Array[Double]]) : Unit = {
      for (k <- 0 until resA.length) {
        for (m <- 0 until resA(k).length) {
          resArea.append(resA(k)(m).toString+" ")
        }
        resArea.append("\n")
      }
    }
    resArea.border_=(new TitledBorder(new LineBorder(java.awt.Color.black),"Predicted Values:"))
    if (evolutionMode < 2) {
      while (j <= idx) {
        val a1 = dworker.getData(j) //
        val res2 = rnn.feedData(a1,actFun)
        res = res.:+(res2)
        resArea.append("Output for input: "+j+"\n")
        append2Area(res2)
        j += 2
      }
    }
    else if (evolutionMode == 2 && idx >= 3) {
      if (idx == 3) {
        val res2 = rnn.linearPredict(dworker.getData(0),dworker.getData(2),NeuralOps.list2Matrix(dworker.getData(1)),actFun)
        append2Area(res2)
        res = res.:+(res2)
      }
      else {
        val B = rnn.linearRegression(dworker.getData(0),NeuralOps.list2Matrix(dworker.getData(1)),actFun)
        val out1 = rnn.evolinoFeed(dworker.getData(2)++dworker.getData(4),actFun)
        val Y = out1 * B
        val res2 = Array.ofDim[Double](Y.numRows,Y.numCols)
        for (i <- 0 until Y.numRows) {
          for (j <- 0 until Y.numCols) {
            resArea.append(Y.apply(i,j).toString+" ")
            res2(i)(j) = Y.apply(i,j)
          }
          resArea.append("\n")
        }
        res = res.:+(res2)
      }
    }
    else if (evolutionMode == 3 && idx >= 3) {
      dworker.initSVM
      val p = getSVMParameter(rnn)
      val data2 = if (idx < 5) dworker.getData(2) else dworker.getData(2) ++ dworker.getData(4)
      val res2 = rnn.svmRegression(dworker.getData(0),dworker.getCols(0),actFun,p,data2)
      append2Area(res2)
      res = res.:+(res2)
    }
    else {
      resArea.append("Not yet implemented for selected options...\n")
    }
    
    val rnnDescription = new TextArea("") {
      border_=(new TitledBorder(new LineBorder(java.awt.Color.black),"The best RNN:"))
    }
    runPrettyPrint(rnn.toXML,rnnDescription)
    object drawPredictions extends MenuItem("Plot Predictions") {
      object plotAction extends Action("Plot Predictions") {
        def apply : Unit = {
          if (idx == 3) {
            val l2 = List(dworker.getData(1).toArray,dworker.getData(3).toArray)
            NeuralOps.plotResults(res,l2)
          }
          else if (idx > 3 ){
            val data2 = dworker.getData(3)
            val (rows,cols) = (data2.size,data2.head.length)
            var emptyList = List[Array[Double]]()
            for (i <- rows until res.length) {
              emptyList = emptyList.:+(new Array[Double](cols))
            }
            NeuralOps.plotResults(res.apply(1),(data2++emptyList).toArray)
          }
        }
      }
      action_=(plotAction)
    }
    object savePredictions extends MenuItem("Save") {
      object saveAction extends Action("Save") {
        def apply : Unit = {
          val fs = new FileChooser
          val reval = fs.showSaveDialog(new FlowPanel)
          if (reval.toString == "Approve") {
            val sf = fs.selectedFile
            Storage.storetxt(new FileOutputStream(sf),NeuralOps.array2Matrix(res.apply(res.size-1)))
          }
        }
      }
      action_=(saveAction)
    }
    val predictionsPane = new GridPanel(2,1) {
      /*
      val panelX = new GridPanel(2,1) {
        contents += new ScrollPane(rnnDescription)
        contents += drawPredictions
      }
      */
      contents += new ScrollPane(resArea)
      contents += new ScrollPane(rnnDescription)//panelX
    }

    val resFrame = new Frame {
      title_=("PredictionsFrame")
      contents_=(predictionsPane)
      val predFiles = new Menu("File") {
        contents += savePredictions
      }
      val predMenu = new Menu("Predictions") {
        contents += drawPredictions
      }
      val predMBar = new MenuBar {
        contents += predFiles
        contents += predMenu
      }
      menuBar_=(predMBar)
    }
    resFrame.preferredSize_=(new Dimension(550,440))
    resFrame.pack
    resFrame.visible_=(true)
    }
    else {
      reportArea.append("No best network to be found.\n")
    }
  }
  def createNullRNN : RNND = {
    val il = new Array[InCellD](2)
    val bl = new Array[CellBlockD](2)
    val ol = new Array[OutCellD](2)
    new RNND(il,bl,ol)
  }
  /*
  def readElem(f:File) : Elem = {
    val e = XML.load(Source.fromFile(f))
    println(e.toString)
    e
  }
  */
  def appendText(f:File) : Unit = {
    val br = new BufferedReader(new FileReader(f))
	while (br.ready) {
	  reportArea.append(br.readLine+"\n")
	}
	br.close
  }
  def prettyPrint(e:Elem) : Unit = {
    val s = e.toString
    val ls = 50
    var idx = 0
    var idx2 = ls
    val l = s.length
    while (idx2 < l) {
      reportArea.append(s.substring(idx,idx2)+"\n")
      idx = idx2
      idx2 += ls
    }
    reportArea.append(s.substring(idx,l))
  }
  def readXML(f:File) : String = {
    val sb = new StringBuilder
    val br = new BufferedReader(new FileReader(f))
    while (br.ready) {
      sb.append(br.readLine)
    }
    br.close
    sb.toString
  }
  /*Writes the best network to a file f either in xml or a format provided by JUNG
   */
  def writeSuperStar(f:String,useXML:Boolean) : Unit = {
    val sDir = new File(saveDirectory)
    if (!sDir.exists()) {
      sDir.mkdir()
    }
    val bestNet = supervisor.getSuperStar
    val f0 = bestNet.getFitness
    if (f0 > 0) {
    val fitness = f0.toString
    val fs = if (fitness.length > 6) fitness.substring(0,6) else fitness
    try {
      val frep = saveDirectory+fsep+f+"f"+fs+".graph"
      val writer = new FileWriter(frep)
      if (useXML) {
        writer.write(bestNet.toXML.toString)
        writer.flush
      }
      else {
        val graphRep = bestNet.toGraph
      
        val gWriter = new GraphMLWriter[Int,String]
        gWriter.save(graphRep,writer)//
      }
      writer.close()
      reportArea.append("Wrote the net into a file: "+frep+"\n")
    } catch {
      case _ => reportArea.append("Could not write the net for some reason.\n")
    }
    }
    else {
      reportArea.append("Could not find any decent network to save.\n")
    }
  }
  def writeAllRNNs(lRNN:List[RNND]) : Boolean = {
    val f = new File(saveDirectory+fsep+"allthebest.xml.zip")
    if (!f.exists) {
      val fout = new FileOutputStream(f)
      val compressor = new DeflaterOutputStream(fout)
      val tag = "<BestNets>".getBytes
      compressor.write(tag,0,tag.length)
      for (rnn <- lRNN) {
        val rep = rnn.toXML.toString.getBytes
        val rl = rep.length
        compressor.write(rep,0,rl)
      }
      val etag = "</BestNets>".getBytes
      compressor.write(etag,0,etag.length)
      compressor.flush
      compressor.close
      fout.close
      true
    }
    else {
      false
    }
  }
  def graph2Img(g:SparseGraph[Int,String]) : Image = {
    val lOut = new FRLayout(g)
    lOut.initialize()
    val imgServer = new VisualizationImageServer(lOut,new Dimension(640,480))
    imgServer.getImage(new Point(370,240),new Dimension(640,480))
  }
  /*
  def displayBestNet : Unit = {
    val bestNet = supervisor.getSuperStar
    val graphRep = bestNet.toGraph
    val img = graph2Img(graphRep)
    //ImageIO.write(img,"jpg",new File(saveDirectory+"bestNet.jpg"))
    val displayWindow = new JFrame
    val displayPanel = new DisplayPanel(img)
    displayPanel.setSize(640,480)
    //val g = img.getGraphics()
    displayWindow.setContentPane(displayPanel)
    //g.drawImage(img,0,0,displayPanel)
    displayWindow.pack()
    displayWindow.setVisible(true)
  }
  */
  def loadRNNs(file:File) : Int = {
    val fIn = new FileInputStream(file)
    val inflater = new InflaterInputStream(fIn)
    var lb = List[Byte]()
    val bl = 1024
    var buffer = new Array[Byte](bl)
    while (inflater.available == 1) {
      val r = inflater.read(buffer,0,bl)
      for (i <- 0 until r) {
        lb = lb.+:(buffer(i))
      }
    }
    inflater.close
    fIn.close
    val s = new java.lang.String(lb.reverse.toArray)
    
    /*
    println("Length of the read string is: "+s.length)
    for (i <- 0 until 256) {
      println(s.charAt(i))
    }
    */
    val xml = XML.loadString(s)
    prettyPrint(xml)
    var res = List[Node]()
    xml\\"RNND" foreach{(entry) => res = res.+:(entry) }
    var c = 0
    for (net <- res) {
      val rnn = RNND.fromXML(net)
      restoredRNNs = restoredRNNs.:+(rnn)
      c += 1
    }
    c
  }
  def restoreEvolver(file:File) : Unit = {
    val fIn = new FileInputStream(file)
    val inflater = new InflaterInputStream(fIn) //
    var lb = List[Byte]()
    val bl = 100
    var buffer = new Array[Byte](bl)
    while (inflater.available == 1) {
      val r = inflater.read(buffer,0,bl)
      for (i <- 0 until r) {
        lb = lb.+:(buffer(i))
      }
    }
    inflater.close
    fIn.close
    val s = new java.lang.String(lb.reverse.toArray)
    val xml = XML.loadString(s)
    val f = (xml \\ "Fitness").head.text.toDouble
    val bNets = (xml \\ "BestNets")
    val cpop = CellPopulationD.fromXML(xml \\ "CellPopulation")
    val npop = NetPopulationD.fromXML(xml \\ "NetPopulationD")
    val e = new NeuralEvolver(cpop,npop,supervisor,supervisor.getReporter,rnd,discardRate)
    if (bNets != NodeSeq.Empty) {
      reportArea.append("Restoring the best nets...\n")
      e.restoreBestNets(bNets)
    }
    else {
      reportArea.append("Could not find any saved Best Networks.\n")
    }
    restoredCount += 1
    //e.setFitness(f)
    evolvers = evolvers.+:(e)
    //runPrettyPrint(evolvers.apply(0).toXML)
    evolverInMemory = true
    if (dworker.getCount > 1) {
      start.enabled_=(true)
    }
    reportArea.append("Restored a previously saved NeuralEvolver.\n")
    reportArea.append("You can start the learning procedure after loading compatible data.\n")
  }
  def restoreRNND(f:File) : Unit = {
    val rnnXML = XML.loadFile(f)
    bRNN = RNND.fromXML(rnnXML)
    netReady = true
    if (dworker.getCount > 3) {
      makePredictions.enabled_=(true)
    }
    reportArea.append("Restored a previously saved RNND.\n")
  }
  def runPrettyPrint(e:Elem) : Unit = {
    val s = e.toString
    var i = 60
    while (i < s.length) {
      val s2 = s.substring(i-60,i)
      reportArea.append(s2+"\n")
      i += 60
    }
    reportArea.append(s.substring(i-60,s.length))
  }
  def runPrettyPrint(e:Elem,area:TextArea) : Unit = {
    //val printer = new PrettyPrinter(66,10)
    val s = e.toString
    var i = 60
    while (i < s.length) {
      val s2 = s.substring(i-60,i)
      area.append(s2+"\n")
      i += 60
    }
    area.append(s.substring(i-60,s.length))
  }
  def getSVMParameter(rnn:RNND) : svm_parameter = {
    val svmParam = new svm_parameter
    svmParam.svm_type = svm_parameter.EPSILON_SVR
    svmParam.kernel_type = svm_parameter.RBF
    svmParam.degree = 3
    svmParam.gamma = 1.0/(rnn.getMid(0).getNumOfCells+rnn.out)
    svmParam.coef0 = 0
    svmParam.nu = 0.5
    svmParam.cache_size = 100
    svmParam.C = 1
    svmParam.eps = 1e-3
    svmParam.p = 0.1
    svmParam.shrinking = 1
    svmParam.probability = 0
    svmParam.nr_weight = 0
    svmParam
  }

}