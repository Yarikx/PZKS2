package org.yarik.pzks2

import scalax.collection.mutable.Graph
import scalax.collection.edge.WDiEdge
import scalax.collection.edge.WUnDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scala.swing.Button
import scala.swing.FlowPanel
import scala.swing.event.ButtonClicked
import UiHelper._
import scalax.collection.GraphEdge.UnDiEdge

class GraphSched() {

  val button = new Button("show")

  val panel = new FlowPanel(l("show"), button) {

    listenTo(button)

    reactions += {
      case ButtonClicked(`button`) =>
        show()
    }
  }

  val hasAsyncIO = false;

  def show() {
    val systemDiGraph = SystemUi.g
    val taskGraph = TaskUi.g

    val systemGraph = {
      val edges = systemDiGraph.edges.map { edge =>
        val l = Proc(edge.from.value.id)
        val r = Proc(edge.to.value.id)
        l ~ r
      }.toArray

      Graph[Proc, UnDiEdge](edges: _*)
    }

    val sorted = new Sorter(taskGraph).sort

    val sortedSystem = systemGraph.nodes.toList.sortBy(x => x.degree).map(_.value).reverse

    println(sorted)
    println(sortedSystem)
  }

}

case class Proc(id: Int)
object Modeller {
  type Time = Int
  
  class Dep (task: Task, value: Time)
  class Task(id: Int, w: Time, dependsOn: Seq[Dep])
  
  abstract class State
  object Idle extends State
  case class Work(start: Time, w: Time) extends State
  case class Move(fromTaslfrom: Proc, to: Proc, w: Time) extends State

  class Line(
    val proc: Proc,
    val states: Seq[State])

  class Env(procs: List[Proc]) {

  }
}


