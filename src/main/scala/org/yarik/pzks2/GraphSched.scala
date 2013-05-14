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

    schedule(sortedSystem, sortedTasks, systemGraph)
  }

  def schedule(procs: List[Proc], tasks: List[Task], systemGraph: Graph[Proc, UnDiEdge]): Env = {
    val startEnv = Modeller.buildStartEnv(procs)
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

  case class Env(lines: List[TimeLine]) {
    def apply(proc: Proc) = lines.find(l => l.proc == proc).get

    def isDone(task: Task) = lines.exists(_.tasksData.contains(task))
    def isPreparedFor(task: Task) = task.dependsOn.map(_.task).forall(isDone)

    def startTask(time: Time, line: TimeLine, task: Task): Env = {
      require(isPreparedFor(task))
      val lineNum = lines.indexOf(line)
      val newLine = line.updCpu(time, Work(time, task))
      val updLines = lines.updated(lineNum, newLine)
      Env(updLines)
    }
    
    def findSpace(startTime: Time, size: Time, from: Proc, to:Proc): Time ={
      val ziped = this(from).links(to.id) zip this(to).receive(from.id)
      val slided = ziped.drop(startTime).sliding(size)
      slided.indexWhere(_.forall(_ == (Idle, Idle))) + startTime
    }

    def move(task: Task, path: List[Proc], w: Time): Env = {
      require(path.size > 1)
      val from :: toGo = path
      val startTime =
        apply(from)
          .calculationTime(task)
          .getOrElse(
            throw new IllegalStateException(s"can't find caclucated $task"))

      @tailrec def finSpaceRecur(startTime: Time, lineFrom: TimeLine, lineTo: TimeLine): Time = {
        val from = lineFrom.proc
        val to = lineTo.proc
        val firstSpace = findSpace(startTime, w, from, to)
        val loadedFrom = lineFrom.links.values.collect {
          case ls if ls(firstSpace) != Idle => 1
        }.sum
        val loadedTo = lineTo.receive.values.collect {
          case ls if ls(firstSpace) != Idle => 1
        }.sum
        
        if (loadedFrom < lineFrom.maxIO && loadedTo < lineTo.maxIO) firstSpace
        else finSpaceRecur(firstSpace + 1, lineFrom, lineTo)
      }

      @tailrec def loop(startTime: Time, path: List[Proc], env: Env): Env =
        path match {
          case from :: to :: rest =>
            val lineFrom = env(from)
            val lineTo = env(to)
            val indexFrom = env.lines.indexOf(lineFrom)
            val indexTo = env.lines.indexOf(lineTo)
            val firstSpace = finSpaceRecur(startTime, lineFrom, lineTo) //line.findSpace(startTime, w, to.id)
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

  case class TimeLine(proc: Proc, cpu: List[State], links: Map[Int, List[State]], receive: Map[Int, List[State]], maxIO: Int) {
    def calculationTime(task: Task): Option[Time] = cpu.collect {
      case Work(start, `task`) => start + task.w
    }.headOption

    def alreadyCalculated = cpu.collect {
      case Work(_, task @ Task(_, w, _)) => task
    }.toSet

    def tasksData = alreadyCalculated ++ receive.values.flatMap(l => l.collect {
      case Move(_, task, _) => task
    }).toSet

    def calculatedAt(t: Time) = calculatedHereAt(t) ++ hasDataFromMovesAt(t)

    def calculatedHereAt(time: Time) = cpu.collect {
      case Work(start, task @ Task(_, w, _)) if time >= start + w => task
    }.toSet

    def hasDataFromMovesAt(time: Time) = receive.values.map(_.take(time)).flatten.collect {
      case Move(start, task, w) if time >= start + w => task
    }.toSet

    def updCpu(time: Time, work: Work): TimeLine = {
      val range = time until (time + work.task.w)
      require(range.forall(i => cpu(i) == Idle))
      val updCpu = range.foldLeft(cpu)((ss, i) => ss.updated(i, work))
      TimeLine(proc, updCpu, links, receive, maxIO)
    }

    def moveFrom(procId: Int, from: Int, w: Int, task: Task) = 
      move(procId, from, w, task, true)
    
    def moveTo(procId: Int, from: Int, w: Int, task: Task) = 
      move(procId, from, w, task, false)
    
    private def move(procId: Int, from: Int, w: Int, task: Task, in:Boolean) = {
      val map = if(in)links else receive
      val slots = map(procId)
      val range = from until (from + w)
      require(range.forall(i => slots(i) == Idle))
      val updSlots = range.foldLeft(slots)((ss, i) => ss.updated(i, Move(from, task, w)))
      val updLinks = map + (procId -> updSlots)
      if(in) TimeLine(proc, cpu, updLinks, receive, maxIO)
      else TimeLine(proc, cpu, links, updLinks, maxIO)
    }

    def timeForTask(task: Task) = {
      @tailrec def loop(t: Time): Time = {
        val calc = calculatedAt(t)
        if (task.dependsOn.forall(dep => calc.contains(dep.task))
          && cpu.drop(t).take(task.w).forall(_ == Idle)) t
        else loop(t + 1)
      }
      if(task.id == 3)
        println("thats it")
      loop(0)
    }

    override def toString = {
      val cpuStr = cpu.mkString(", ")
      val linksStr = links.map { case (p, ss) => s"\nL[$p] [${ss.mkString(", ")}]" }.mkString
      s"$proc [$cpuStr] $linksStr"
    }
  }

  object TimeLine {
    val N = 10
    private val maxIO = 5
    private val startSlots = (1 to N).map(_ => Idle).toList
    private def buildLinks(p: Proc) = p.neighbors.map(n => (n -> startSlots)).toMap
    def apply(p: Proc) = new TimeLine(p, startSlots, buildLinks(p), buildLinks(p), maxIO)
  }

  //stuff

  def buildStartEnv(procs: Seq[Proc]): Env =
    Env(procs.toList.map(p => TimeLine(p)))

  def makeStep(env: Env, tasks: List[Task])(implicit procPriors: List[Proc], systemG: Graph[Proc, UnDiEdge]): Env =
    if (tasks isEmpty)
      env
    else {
      //try to find ready to calculate tasks
      val goodPair = tasks.map { task =>
        val depTasks = task.dependsOn.map(_.task)
        val okLines = env.lines.filter { line =>
          val set = line.tasksData
          depTasks.forall(set.contains)
        }.map { line =>
          val time = line.timeForTask(task)

          (task, line, time)
        }
        okLines.sortBy(_._3).headOption
      }.collect { case Some(o) => o }.sortBy(_._3).headOption

      goodPair match {
        case Some((task, line, time)) =>
          val updEnv = env.startTask(time, line, task)
          println("start new task")
          println(updEnv)
          makeStep(updEnv, tasks.filter(_ != task))
        case None =>
          //ok, find tasks to move
          val task = tasks.head
          require(!task.dependsOn.isEmpty)
          val linesWithData = (for {
            line <- env.lines
            dep <- task.dependsOn
            depTask = dep.task
            if line.tasksData contains depTask
          } yield line).distinct
          
          val dst = linesWithData.sortBy(line => procPriors.indexOf(line.proc)).head
          val toMove = task.dependsOn.map {
            case Dep(depTask, w) =>
              val line = env.lines.find{
                line => line.alreadyCalculated.contains(depTask)
              }.get
              val path = {
                val from = systemG.nodes.get(line.proc)
                val to = systemG.nodes.get(dst.proc)
                from.shortestPathTo(to).get.nodes.map(_.value)
              }
              (depTask, path, w)
          }.filterNot{case (dt, _, _) => dst.alreadyCalculated.contains(dt)}
          val updEnv = toMove.foldLeft(env) { (tmpEnv, move) => move match{
            case (task, path, w) => tmpEnv.move(task, path, w)
          }}

          println("moved tasks")
          println(updEnv)
          makeStep(updEnv, tasks)
      }
    }

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


