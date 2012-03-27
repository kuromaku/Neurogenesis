package neurogenesis.doubleprecision
import scala.actors.Actor
import scala.swing.TextArea
import scala.swing.Label
import scala.swing.Button

class InterfaceWorker(area:TextArea,lbl:Label,btn:Button) extends Actor {
  var count = 0
  def act : Unit = {
    loop {
      react {
        case AnotherArray(a) => {
          count += 1
          lbl.text_=("Data Arrays: "+count)
          var maxLines = 100
          area.append("Array: "+count+"\n")
          if (a.length < maxLines) {
            maxLines = a.length
          }
          for (i <- 0 until maxLines) {
            for (j <- 0 until a(i).length) {
              area.append(a(i)(j).toString+" ")
            }
            area.append("\n")
          }
          if (count == 2) {
            btn.enabled_=(true)
          }
          area.append("---showing the first "+maxLines+" rows of data.\n")
        }
      }
    }
  }
  def getCount : Int = count
}