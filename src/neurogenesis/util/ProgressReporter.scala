package neurogenesis.util
import neurogenesis.msg.ProgressMessage

import scala.actors.Actor
import scala.swing.TextArea
class ProgressReporter(textArea:TextArea) extends Actor {
  def act : Unit = {
    loop {
      react {
        case ProgressMessage(msg) => {
          if (msg.length < 100) {
            textArea.append(msg+"\n")
          }
          else {
            prettyPrint(msg)
          }
        }
        case "Exit" => exit
      }
    }
  }
  def prettyPrint(s:String) : Unit = {
    val ls = 70
    var idx = 0
    var idx2 = ls
    val l = s.length
    while (idx2 < l) {
      textArea.append(s.substring(idx,idx2)+"\n")
      idx = idx2
      idx2 += ls
    }
    textArea.append(s.substring(idx,l))
  }
}