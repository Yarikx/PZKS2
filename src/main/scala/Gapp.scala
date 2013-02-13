import swing._

object Gapp extends SimpleSwingApplication {
  println("hello world")

  def top = new MainFrame {
    title = "Graphs"
    contents = new Button {
      text = "click me"

    }
  }
}
