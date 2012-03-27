package neurogenesis.doubleprecision

import javax.swing.JPanel
import java.awt.Graphics
import java.awt.Image
import java.awt.geom.AffineTransform

class DisplayPanel(img:Image) extends JPanel {
  override def paint(g:Graphics) : Unit = {
    val pSize = getSize()
    val h = pSize.getHeight//img.getHeight(null)
    val w = pSize.getWidth()//img.getWidth(null)
    g.drawImage(img,0,0,w.toInt,h.toInt,null)
  }
}