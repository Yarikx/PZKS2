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
import SchedUtils._
import scala.annotation.tailrec

class GraphSched() {
  import Modeller._

  val button = new Button("show")

  val panel = new FlowPanel(l("show"), button) {

    listenTo(button)

    reactions += {
      case ButtonClicked(`button`) =>
        show()
    }
  }

  val hasAsyncIO = false;

  def show() = {
    val systemDiGraph = SystemUi.g
    val taskGraph = TaskUi.g
    transformAndSchedule(systemDiGraph, taskGraph)
  }

  def transformAndSchedule(systemDiGraph: Graph[Vertex, WDiEdge], taskGraph: Graph[Vertex, WDiEdge]): Env = {

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

    val sortedTasks = buildTasks(sorted, taskGraph)

    schedule(sortedSystem, sortedTasks)
  }

  def schedule(procs: List[Proc], tasks: List[Task]): Env = {
    val startEnv = Modeller.buildStartEnv(procs)
    makeStep(0, startEnv, tasks)(procs)
  }

}

case class Proc(id: Int) {
  override def toString = s"P[$id]"
}
object Modeller {
  type Time = Int

  //tasks
  case class Dep(task: Task, value: Time)
  case class Task(id: Int, w: Time, dependsOn: Seq[Dep]){
    override def toString=s"$id($w)"
  }
  //state machine
  abstract class State
  case object Idle extends State
  case class Work(start: Time, task: Task) extends State {
    override def toString = s"W(task:$task)"
  }
  case class Move(task: Task, from: Proc, to: Proc, w: Time) extends State

  case class Line(proc: Proc, slots: Seq[State], calculated: Set[Task]) {
    def updSlot(index: Time, state: State) = Line(proc, slots.updated(index, state), calculated)
    override def toString = s"$proc slots:[${slots.mkString(", ")}] data:${calculated.mkString(",")}"
  }

  case class Env(lines: List[Line]) {
    def apply(proc: Proc) = lines.find(l => l.proc == proc).get
    def freeAt(t: Time) = lines.filter(l => l.slots.size < t || l.slots(t) == Idle)
    def prepare(t: Time) = Env(lines.map { l =>
      if (t == 0) {
        l.slots :+ Idle
        Line(l.proc, l.slots :+ Idle, Set())
      } else {
        val (opData, next) = l.slots(t - 1) match {
          case status @ Work(start, task) =>
            if (start + task.w >= t) (Some(task), Idle)
            else (None, status)
          case Idle => (None, Idle)
        }

        val calc = opData.map(l.calculated + _).getOrElse(l.calculated)
        Line(l.proc, l.slots :+ next, calc)
      }
    })

    def isDone(task: Task) = lines.exists(_.calculated.contains(task))
    def isPreparedFor(task: Task) = task.dependsOn.map(_.task).forall(isDone)

    def startTask(time: Time, line: Line, task: Task): Env = {
      require(isPreparedFor(task))
      val lineNum = lines.indexOf(line)
      val newLine = line.updSlot(time, Work(time, task))
      val updLines = lines.updated(lineNum, newLine)
      Env(updLines)
    }
  }
  //stuff

  def buildStartEnv(procs: Seq[Proc]): Env =
    Env(procs.toList.map(p => Line(p, Seq(), Set())))

  def makeStep(time: Time, prevEnv: Env, tasks: List[Task])(implicit procPriors: List[Proc]): Env = {
    val env = prevEnv.prepare(time)

    @tailrec def loop(env: Env, tasks: List[Task]): (Env, List[Task]) =
      if (tasks.isEmpty) (env, Nil)
      else env.freeAt(time) match {
        case Nil => (env, tasks)
        case firstLine :: restLines =>
          val task :: taskTail = tasks
          val updEnv = env.startTask(time, firstLine, task)
          loop(updEnv, taskTail)
      }

    val readyTasks = tasks.filter(env.isPreparedFor)

    val (newEnv, rest) = loop(env, readyTasks)
    newEnv
  }
}


