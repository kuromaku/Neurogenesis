package neurogenesis.doubleprecision

import scala.actors.Actor
import scala.swing.TextArea
class ProgressReporter(textArea:TextArea) extends Actor {
  def act : Unit = {
    loop {
      react {
        case ProgressMessage(msg) => {
          textArea.append(msg+"\n")
        }
        case "Exit" => exit
      }
    }
  }
}