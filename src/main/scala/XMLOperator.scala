package neurogenesis.util

import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.Node
import scala.xml.TopScope
import scala.swing.TextArea
import scala.collection.immutable.Queue
object XMLOperator {
  def filterXML(feed:Elem) : Seq[Node] = {
    var res = Queue[Node]()
    feed\\"cnn" foreach{(entry) => res = res.+:(entry) }
    res.reverse
  }
  def filterNodeSeq(feed:NodeSeq) : Seq[Node] = {
    var res = Queue[Node]()
    feed\\"cnn" foreach{(entry) => res = res.+:(entry) }
    res.reverse
  }
  def customFilter(feed:NodeSeq,str:String) : Seq[Node] = {
    var res = Queue[Node]()
    feed\\str foreach{(entry) => res = res.+:(entry) }
    res.reverse
  }
  def toElem(seq:NodeSeq) : Elem = {
    
    val min = (seq \\ "Min")
    val max = (seq \\ "Max")
    val nrep = filterNodeSeq(seq)
    val a = new Array[Node](nrep.size+2)
    a(0) = min.head
    a(1) = max.head
    var idx = 2
    for (s <- nrep) {
      a(idx) = s
      idx += 1
    }
    new Elem(null,"NeuralConnsD",null,TopScope,a: _*)
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
}