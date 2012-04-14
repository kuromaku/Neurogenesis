package neurogenesis.util

import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.Node
import scala.xml.TopScope

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
}