package org.yarik.pzks2

import UiHelper._
import scala.annotation.tailrec
import scala.swing.{ Button, FlowPanel, TextField }
import scala.swing.event.ButtonClicked
import scala.util.Random
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.mutable.Graph

class Generator(gui: Gui) {
  def g = gui.g

  val r = new Random

  def createControls = {
    val loField = new TextField(5) { text = "10" }
    val hiField = new TextField(5) { text = "50" }
    val numberField = new TextField(5) { text = "10" }
    val koefField = new TextField(5) { text = "0.5" }

    val number = createGet(numberField)
    val koef = createGetD(koefField)
    val low = createGet(loField)
    val high = createGet(hiField)

    val genButton = new Button("generate")

    new FlowPanel(l("lo"), loField, l("hi"), hiField, l("number"), numberField, l("koef"), koefField, genButton) {
      listenTo(genButton)
      reactions += {
        case ButtonClicked(`genButton`) =>
          for {
            n <- number()
            k <- koef()
            l <- low()
            h <- high()
            if h > l
            if n > 1
            if k > 0
          } {
            generateGraph(n, l, h, k, gui)
          }
      }
    }
  }

  def generateGraph(n: Int, lo: Int, hi: Int, k: Double, gui: Gui) {

    val mid = (lo + hi) / 2
    val vs = (0 until n).map(Vertex(_, r.nextInt(mid - lo) + lo))
    val vsum = vs.map(_.value).sum
    val esum = (vsum / k - vsum).toInt

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

    val edges =
      evalues.view
        .map { v =>
          val from = r.nextInt(n - 1) + 1
          val to = r.nextInt(from)
          (v, (from, to))
        }
        .groupBy { case (_, t) => t }
        .map {
          case ((f, t), es) =>
            val from = vs(f)
            val to = vs(t)
            val value = es.map(_._1).sum
            from ~> to % value
        }

    g.clear
    vs.foreach(g += _)
    edges.foreach(g += _)
    println(s"values sum = $vsum")
    println(s"edges sum = $esum")
    println(vsum / (esum + vsum))
    gui.update
  }
}
