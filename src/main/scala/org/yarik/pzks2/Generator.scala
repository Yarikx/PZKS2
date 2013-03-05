package org.yarik.pzks2

import scala.annotation.tailrec
import scala.swing.Button
import scala.swing.FlowPanel
import scala.swing.TextField
import scala.swing.event.ButtonClicked
import scala.util.Random
import scalax.collection.mutable.Graph
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import UiHelper._;

class Generator(gui: Gui) {
  def g = gui.g

  val r = new Random

  def createControls = {
    val numberField = new TextField(5) { text = "10" }
    val koefField = new TextField(5) { text = "1.5" }

    val number = createGet(numberField)
    val koef = createGetD(koefField)

    val genButton = new Button("generate")

    new FlowPanel(l("number"), numberField, l("koef"), koefField, genButton) {
      listenTo(genButton)
      reactions += {
        case ButtonClicked(`genButton`) =>
          for {
            n <- number()
            k <- koef()
          } {
            generateGraph(n, k, gui)
          }
      }
    }
  }

  def generateGraph(n: Int, k: Double, gui: Gui) {
    val lo = 10
    val hi = 50

    val mid = (lo + hi) / 2
    val vs = (0 until n).map(Vertex(_, r.nextInt(mid - lo) + lo))
    val vsum = vs.map(_.value).sum
    val esum = (vsum * k - vsum).toInt

    @tailrec
    def genVals(collected: List[Int]): List[Int] = {
      val sum = collected.sum
      if (sum >= esum) {
        collected
      } else {
        val diff = esum - sum
        val c = r.nextInt(mid - lo) + lo
        val item = if (c < diff) c else diff
        genVals(item :: collected)
      }
    }

    val evalues = genVals(List())

    @tailrec
    def genFromTo(acum: List[(Int, Int)]): List[(Int, Int)] = {
      val from = r.nextInt(n - 1) + 1
      val to = r.nextInt(from)

      val t = (from, to)

      if (acum.contains(t)) {
        genFromTo(acum)
      } else {
        val list = t :: acum
        if (list.size >= evalues.size) {
          list
        } else {
          genFromTo(list)
        }
      }
    }

    val edges = evalues.zip(genFromTo(List())).map {
      case (v, (fromI, toI)) =>
        val from = vs(fromI)
        val to = vs(toI)
        from ~> to % v
    }

    g.clear
    vs.foreach(g += _)
    edges.foreach(g += _)
    println(s"values sum = $vsum")
    println(s"edges sum = $esum")
    println(esum / vsum + 1)
    gui.update
  }
}
