package neurogenesis.util
import edu.uci.ics.jung.algorithms.layout.StaticLayout
import edu.uci.ics.jung.graph.SparseGraph
import java.awt.geom.Point2D.Float
import java.awt.geom.Point2D
import org.apache.commons.collections15.Transformer
import org.apache.commons.collections15.TransformerUtils
import java.util.TreeMap
import java.awt.Dimension
class RNNLayout(graph:SparseGraph[Int,String],in:Int,blocks:Int,mcells:Int,out:Int) {
  def getLayout(width:Int,height:Int) : StaticLayout[Int,String] = {
    val vl = graph.getVertices.toArray.length
    val map = new TreeMap[Int,Point2D]()
    val mid = in + blocks*(mcells+3)
    val cells = mid + out
    val h0 = height-40
    val w0 = width-40
    val synapses = (mcells+3)
    val h1 = h0/(blocks+1)
    val h2 = h0/(out+1)
    for (i <- 0 until vl) {
      if (i < in) {
        val p = new Point2D.Float(25,i*h0/in+20)
        map.put(i,p)
      }
      else if (i < mid) { //
        val aux = i - in
        val gate = aux % synapses
        val block = aux/synapses
        if (gate < mcells) {
          if (gate < mcells/2) {
            map.put(i,new Point2D.Float(width/2-(mcells/2-gate)*20,(block+1)*h1+20))
          }
          else if (gate == mcells/2) {
            map.put(i,new Point2D.Float(width/2,(block+1)*h1+20))
          }
          else {
            map.put(i,new Point2D.Float(width/2+(gate-mcells/2)*20,(block+1)*h1+20))
          }
        }
        else {
          val aux2 = gate - mcells
          map.put(i,new Point2D.Float(width/2+27*(aux2-1),(block+1)*h1+5))
        }
      }
      else if (i < cells) {
        val aux = i - mid
        map.put(i,new Point2D.Float(width-25,(aux+1)*h2+20))
      }
      else {
        map.put(i,new Point2D.Float(width-20,20+i*3))
      }
    } //[Int,V]
    val initializer = TransformerUtils.mapTransformer(map)
    new StaticLayout[Int,String](graph,initializer,new Dimension(width,height))
  }
}