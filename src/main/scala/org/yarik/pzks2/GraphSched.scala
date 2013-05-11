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

  def show() = {
    val systemDiGraph = SystemUi.g
    val taskGraph = TaskUi.g
    transformAndSchedule(systemDiGraph, taskGraph)
  }

  def transformAndSchedule(systemDiGraph: Graph[Vertex, WDiEdge], taskGraph: Graph[Vertex, WDiEdge]): Env = {

    val systemGraph = {
      val edges = systemDiGraph.edges.map { edge =>
        val l = Proc(edge.from.value.id, edge.from.neighbors.map(n => n.id).toList)
        val r = Proc(edge.to.value.id, edge.to.neighbors.map(n => n.id).toList)
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
    //    makeStep(0, startEnv, tasks)(procs)
    startEnv
  }

}

case class Proc(id: Int, neighbors: List[Int]) {
  override def toString = s"P[$id]"
}
object Modeller {
  type Time = Int

  //tasks
  case class Dep(task: Task, value: Time)
  case class Task(id: Int, w: Time, dependsOn: Seq[Dep]) {
    override def toString = s"$id($w)"
  }
  //state machine
  abstract class State
  case object Idle extends State
  case class Work(start: Time, task: Task) extends State {
    override def toString = s"W[$start]$task"
  }
  case class Move(task: Task, w: Time) extends State

  case class Env(lines: List[TimeLine]) {
    def apply(proc: Proc) = lines.find(l => l.proc == proc).get

    def isDone(task: Task) = lines.exists(_.cpu.contains(task))
    def isPreparedFor(task: Task) = task.dependsOn.map(_.task).forall(isDone)

    def startTask(time: Time, line: TimeLine, task: Task): Env = {
      require(isPreparedFor(task))
      val lineNum = lines.indexOf(line)
      val newLine = line.updCpu(time, Work(time, task))
      val updLines = lines.updated(lineNum, newLine)
      Env(updLines)
    }

    def move(task: Task, path: List[Proc], w: Time): Env = {
      require(path.size > 1)
      val from :: toGo = path
      val startTime =
        apply(from)
          .calculationTime(task)
          .getOrElse(
            throw new IllegalStateException(s"can't find caclucated $task"))

      @tailrec def loop(startTime: Time, path: List[Proc], env: Env): Env = 
        path match{
        case from :: to :: rest => 
          val line = env(from)
          val index = env.lines.indexOf(line)
          val firstSpace = line.findSpace(startTime, w, to.id)
          val updLine = line.updLink(to.id, firstSpace, w, task)
          val updEnv = Env(env.lines.updated(index, updLine))
          loop(firstSpace +  w, to :: rest, updEnv)
        case _ => env
      } 

      loop(startTime, path, this)
    }

    override def toString = lines.mkString("\n=============\n")
  }

  case class TimeLine(proc: Proc, cpu: List[State], links: Map[Int, List[State]]) {
    def calculationTime(task: Task): Option[Time] = cpu.collect {
      case Work(start, `task`) => start + task.w
    }.headOption

    def calculatedAt(t: Time) = calculatedHereAt(t) ++ hasDataFromMovesAt(t)

    def calculatedHereAt(time: Time) = cpu.collect {
      case Work(start, task @ Task(_, w, _)) if time >= start + w => task
    }.toSet

    def hasDataFromMovesAt(time: Time) = links.values.map(_.take(time + 1)).flatten.collect {
      case Move(task, _) => task
    }.toSet

    def updCpu(time: Time, work: Work): TimeLine = {
      val range = time until (time + work.task.w)
      require(range.forall(i => cpu(i) == Idle))
      val updCpu = range.foldLeft(cpu)((ss, i) => ss.updated(i, work))
      TimeLine(proc, updCpu, links)
    }
    
    def findSpace(from: Time, size: Time, procLinkId: Int): Time=
      links(procLinkId).drop(from).sliding(size).indexWhere(_.forall(_ == Idle)) + from
      
    def updLink(procId: Int, from: Int, w: Int, task: Task) ={
      val slots = links(procId)
      val range = from until (from + w)
      require(range.forall(i => slots(i) == Idle))
      val updSlots = range.foldLeft(slots)((ss, i) => ss.updated(i, Move(task, w)))
      val updLinks = links + (procId -> updSlots)
      TimeLine(proc, cpu, updLinks)
    }

    override def toString = {
      val cpuStr = cpu.mkString(", ")
      val linksStr = links.map { case (p, ss) => s"\nL[$p] [${ss.mkString(", ")}]" }.mkString
      s"$proc [$cpuStr] $linksStr"
    }
  }

  object TimeLine {
    private val N = 10
    private val startSlots = (1 to N).map(_ => Idle).toList
    private def buildLinks(p: Proc) = p.neighbors.map(n => (n -> startSlots)).toMap
    def apply(p: Proc) = new TimeLine(p, startSlots, buildLinks(p))
  }

  //stuff

  def buildStartEnv(procs: Seq[Proc]): Env =
    Env(procs.toList.map(p => TimeLine(p)))

  //  @tailrec def makeStep(time: Time, env: Env, tasks: List[Task])(implicit procPriors: List[Proc]): Env = {
  //    //place as many tasks as possible
  //    @tailrec def loop(env: Env, tasks: List[Task]): (Env, List[Task]) =
  //      if (tasks.isEmpty) (env, Nil)
  //      else env.freeAt(time) match {
  //        case Nil => (env, tasks)
  //        case firstLine :: restLines =>
  //          val task :: taskTail = tasks
  //          val updEnv = env.startTask(time, firstLine, task)
  //          loop(updEnv, taskTail)
  //      }
  //
  //    val readyTasks = tasks.filter(env.isPreparedFor)
  //
  //    val (newEnv, restReady) = loop(env, readyTasks)
  //    //not elegant variant of tasks -- (readyTasks -- restReady)
  //    val tasksToProcess = tasks.filterNot(readyTasks.filterNot(restReady contains _) contains _)
  //    if (newEnv.lines.forall(_.procSlots(time) == Idle)) newEnv
  //    else {
  //      println(s"=========time($time)======")
  //      newEnv.lines.foreach(println)
  //      println(s"=========time($time)======")
  //      makeStep(time + 1, newEnv, tasksToProcess)
  //    }
  //  }
}


