package org.yarik.pzks2

import scala.annotation.tailrec
import scala.swing.Button
import scala.swing.Component
import scala.swing.FlowPanel
import scala.swing.TextField
import scala.swing.event.ButtonClicked
import SchedUtils.buildTasks
import SchedUtils.makeUi
import UiHelper._
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef.any2EdgeAssoc
import scalax.collection.edge.WDiEdge
import scalax.collection.mutable.Graph
import scala.swing.CheckBox

class GraphSched(update: Component => Unit) {
  import Modeller._
  val maxIoField = new TextField(5) { text = "3" }
  val ioF = createGet(maxIoField)
  val duplexCheck = new CheckBox("duplex")
  val button = new Button("show")
  val panel = new FlowPanel(l("io:"), maxIoField, duplexCheck, l("show"), button) {
    listenTo(button)
    reactions += {
      case ButtonClicked(`button`) =>
        for (io <- ioF()) show(io, duplexCheck.selected)
    }
  }

  def show(implicit maxIo: Int, duplex: Boolean) = {
    val systemDiGraph = SystemUi.g
    val taskGraph = TaskUi.g
    Env.duplex = duplex
    val currentTime = System.currentTimeMillis()
    val env = transformAndSchedule(systemDiGraph, taskGraph)
    val taked = System.currentTimeMillis() - currentTime;
    println(s"taked $taked to calculate system")
    update(makeUi(env))
  }

  def transformAndSchedule(systemDiGraph: Graph[Vertex, WDiEdge], taskGraph: Graph[Vertex, WDiEdge])(implicit maxIo: Int): Env = {

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

    schedule(sortedSystem, sortedTasks, systemGraph)
  }

  def schedule(procs: List[Proc], tasks: List[Task], systemGraph: Graph[Proc, UnDiEdge])(implicit maxIo: Int): Env = {
    val startEnv = Modeller.buildStartEnv(procs, maxIo)
    makeStep(startEnv, tasks)(procs, systemGraph)
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
  case class Move(start: Time, task: Task, w: Time) extends State {
    override def toString = s"M[$task]"
  }

  object Env {
    var duplex = false
  }
  case class Env(lines: List[TimeLine]) {
    def apply(proc: Proc) = lines.find(l => l.proc == proc).get

    def isDone(task: Task) = lines.exists(_.tasksData.contains(task))
    def isPreparedFor(task: Task) = task.dependsOn.map(_.task).forall(isDone)

    def cpuSum = lines.map(_.lastCpu).sum
    def cpuMax = lines.map(_.lastCpu).max

    def startTask(time: Time, line: TimeLine, task: Task): Env = {
      require(isPreparedFor(task))
      val lineNum = lines.indexOf(line)
      val newLine = line.updCpu(time, Work(time, task))
      val updLines = lines.updated(lineNum, newLine)
      Env(updLines)
    }

    def findSpace(startTime: Time, size: Time, from: Proc, to: Proc): Time = {
      val ziped = this(from).sends(to.id) zip this(to).receives(from.id)
      val oks = ziped.map {
        case (Idle, Idle) => true
        case _ => false
      }
      val slided = oks.drop(startTime).sliding(size)
      slided.indexWhere(_.forall(x => x)) + startTime
    }

    @tailrec final def finSpaceRecur(startTime: Time, lineFrom: TimeLine, lineTo: TimeLine, w: Time, task: Task): Time = {
      val from = lineFrom.proc
      val to = lineTo.proc
      val firstSpace = findSpace(startTime, w, from, to)
      val ok = {
        val updLineFrom = lineFrom.moveFrom(to.id, firstSpace, w, task)
        val updLineTo = lineTo.moveTo(from.id, firstSpace, w, task)
        val frees = List(updLineFrom, updLineTo).map(line => line.free.drop(firstSpace).take(w))
        frees.forall(l => l.forall(_ <= lineFrom.maxIO))
      }

      if (ok) firstSpace
      else finSpaceRecur(firstSpace + 1, lineFrom, lineTo, w, task)
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
        path match {
          case from :: to :: rest =>
            val lineFrom = env(from)
            val lineTo = env(to)
            val indexFrom = env.lines.indexOf(lineFrom)
            val indexTo = env.lines.indexOf(lineTo)
            val firstSpace = finSpaceRecur(startTime, lineFrom, lineTo, w, task)
            val updLineFrom = lineFrom.moveFrom(to.id, firstSpace, w, task)
            val updLineTo = lineTo.moveTo(from.id, firstSpace, w, task)
            val updEnv = Env(env.lines.updated(indexFrom, updLineFrom).updated(indexTo, updLineTo))
            loop(firstSpace + w, to :: rest, updEnv)
          case _ => env
        }

      loop(startTime, path, this)
    }

    override def toString = lines.mkString("\n=============\n")
  }

  case class TimeLine(proc: Proc, cpu: List[State], sends: Map[Int, List[State]], receives: Map[Int, List[State]], maxIO: Int) {
    def calculationTime(task: Task): Option[Time] = cpu.collect {
      case Work(start, `task`) => start + task.w
    }.headOption

    def alreadyCalculated = cpu.collect {
      case Work(_, task @ Task(_, w, _)) => task
    }.toSet

    def tasksData = alreadyCalculated ++ receives.values.flatMap(l => l.collect {
      case Move(_, task, _) => task
    }).toSet

    def calculatedAt(t: Time) = calculatedHereAt(t) ++ hasDataFromMovesAt(t)

    def calculatedHereAt(time: Time) = cpu.collect {
      case Work(start, task @ Task(_, w, _)) if time >= start + w => task
    }.toSet

    def hasDataFromMovesAt(time: Time) = receives.values.map(_.take(time)).flatten.collect {
      case Move(start, task, w) if time >= start + w => task
    }.toSet

    def updCpu(time: Time, work: Work): TimeLine = {
      val range = time until (time + work.task.w)
      require(range.forall(i => cpu(i) == Idle))
      val updCpu = range.foldLeft(cpu)((ss, i) => ss.updated(i, work))
      TimeLine(proc, updCpu, sends, receives, maxIO)
    }

    def lastCpu = scala.math.max(cpu.lastIndexWhere(_ != Idle), 0)

    def moveFrom(procId: Int, from: Int, w: Int, task: Task) =
      move(procId, from, w, task, true)

    def moveTo(procId: Int, from: Int, w: Int, task: Task) =
      move(procId, from, w, task, false)

    private def move(procId: Int, from: Int, w: Int, task: Task, in: Boolean) = {
      val map = if (in) sends else receives
      val slots = map(procId)
      val range = from until (from + w)
      require(range.forall(i => slots(i) == Idle))
      val updSlots = range.foldLeft(slots)((ss, i) => ss.updated(i, Move(from, task, w)))
      val updLinks = map + (procId -> updSlots)
      if (in) TimeLine(proc, cpu, updLinks, receives, maxIO)
      else TimeLine(proc, cpu, sends, updLinks, maxIO)
    }

    def free = {
      val keys = sends.keys.toList
      val to = keys.map(sends)
      val from = keys.map(receives)
      (to zip from).map {
        case (l1, l2) =>
          val zipped = l1 zip l2
          zipped.map {
            case (Idle, Idle) => 0
            case (m1: Move, Idle) => 1
            case (Idle, m1: Move) => 1
            case _ => if (Env.duplex) 1 else 2
          }
      }.reduce { (l1, l2) =>
        l1.zip(l2).map {
          case (x1, x2) => x1 + x2
        }
      }

    }

    def timeForTask(task: Task) = {
      @tailrec def loop(t: Time): Time = {
        val calc = calculatedAt(t)
        if (task.dependsOn.forall(dep => calc.contains(dep.task))
          && cpu.drop(t).take(task.w).forall(_ == Idle)) t
        else loop(t + 1)
      }
      loop(0)
    }

    override def toString = {
      val cpuStr = cpu.mkString(", ")
      val linksStr = sends.map { case (p, ss) => s"\nL[$p] [${ss.mkString(", ")}]" }.mkString
      val resStr = receives.map { case (p, ss) => s"\nL[$p] [${ss.mkString(", ")}]" }.mkString
      s"$proc [$cpuStr] $linksStr \n$resStr"
    }
  }

  object TimeLine {
    val N = 1000
    private val startSlots = (1 to N).map(_ => Idle).toList
    private def buildLinks(p: Proc) = p.neighbors.map(n => (n -> startSlots)).toMap
    def apply(p: Proc, maxIo: Int) = new TimeLine(p, startSlots, buildLinks(p), buildLinks(p), maxIo)
  }

  //stuff

  def buildStartEnv(procs: Seq[Proc], maxIo: Int): Env =
    Env(procs.toList.map(p => TimeLine(p, maxIo)))

  def makeStep(env: Env, tasks: List[Task])(implicit procPriors: List[Proc], systemG: Graph[Proc, UnDiEdge]): Env =
    if (tasks isEmpty) env
    else {
      def startTask(envArg:Env, line: TimeLine, task:Task) = {
        val time = line.timeForTask(task)
        envArg.startTask(time, line, task)
      }
      
      val headTask :: rst = tasks
      val results = env.lines.par.map { dst =>
        val tasksData = dst.tasksData
        if (headTask.dependsOn.map(_.task).forall(tasksData.contains)) {
          startTask(env, dst, headTask)
        } else {
          //ok, find tasks to move
          val toMove = headTask.dependsOn.map {
            case Dep(depTask, w) =>
              val line = env.lines.find {
                line => line.alreadyCalculated.contains(depTask)
              }.get
              val path = {
                val from = systemG.nodes.get(line.proc)
                val to = systemG.nodes.get(dst.proc)
                from.shortestPathTo(to).get.nodes.map(_.value)
              }
              (depTask, path, w)
          }.filterNot { case (dt, _, _) => tasksData.contains(dt) }

          @tailrec def putMoves(env: Env, toMove: List[(Task, List[Proc], Time)]): Env = {
            toMove match {
              case Nil => env
              case (task, path, w) :: Nil => env.move(task, path, w)
              case list =>
                val ((task, path, w) :: tail) = toMove.sortBy {
                  case (depTask, from :: to :: _, w) =>
                    val startTime =
                      env.apply(from)
                        .calculationTime(depTask)
                        .get

                    val lineFrom = env(from)
                    val lineTo = env(to)
                    val firstSpace = env.finSpaceRecur(startTime, lineFrom, lineTo, w, depTask)
                    firstSpace
                }
                val updEnv = env.move(task, path, w)
                putMoves(updEnv, tail)
            }
          }

          val updEnv = putMoves(env, toMove.toList)
          val newLine = updEnv(dst.proc)
          startTask(updEnv, newLine, headTask)
        }
      }
      val nextEnv = results.seq.sortBy(_.cpuSum).sortBy(_.cpuMax).head
      makeStep(nextEnv, rst)
    }
}
