package neurogenesis.doubleprecision

import scala.actors.Actor
import edu.uci.ics.jung.algorithms.layout.FRLayout
import edu.uci.ics.jung.algorithms.layout.KKLayout
import edu.uci.ics.jung.algorithms.layout.ISOMLayout
import edu.uci.ics.jung.visualization.VisualizationImageServer
import java.awt.Image
import java.awt.image.RenderedImage
import java.awt.Point
import javax.imageio.ImageIO
import java.awt.Graphics2D
import javax.swing.JFrame
import javax.swing.JPanel
import edu.uci.ics.jung.graph.SparseGraph
import java.awt.Dimension
import javax.swing.border.EtchedBorder
import edu.uci.ics.jung.algorithms.layout.AbstractLayout
import edu.uci.ics.jung.algorithms.layout.SpringLayout

class GraphWorker(method:String) extends Actor {
  var layoutMethod = method
  def act : Unit = {
    loop {
      react {
        case AnotherRNN(rnn) => {
          val graphRep = rnn.toGraph
          val img = graph2Img(graphRep)
    
          val displayWindow = new JFrame("Sketch of the best network found using layout: "+layoutMethod)
          //displayWindow.se
          displayWindow.setPreferredSize(new Dimension(640,480))
          val displayPanel = new DisplayPanel(img)
          displayPanel.setBorder(new EtchedBorder)
          displayPanel.setPreferredSize(new Dimension(640,480))
          displayWindow.setContentPane(displayPanel)
          displayWindow.pack()
          displayWindow.setVisible(true)
        }
      }
    }
  }
  def graph2Img(g:SparseGraph[Int,Int]) : Image = {
    val fGraph = new SparseGraph[Int,Int]()
    
    var lOut:AbstractLayout[Int,Int] = new FRLayout(fGraph)
    layoutMethod match {
      case "FR" => {
        lOut = new FRLayout(g)
      }
      case "ISOM" => {
        lOut = new ISOMLayout(g)
      }
      case "KK" => {
        lOut = new KKLayout(g)
      }
      case "Spring" => {
        lOut = new SpringLayout(g)
      }
      case _ => lOut = new FRLayout(g)
    }
    
    lOut.initialize()
    val imgServer = new VisualizationImageServer(lOut,new Dimension(640,480))
    imgServer.getImage(new Point(400,300),new Dimension(800,600))
  }
  def setMethod(s:String) : Unit = { layoutMethod = s }
}