package org.yarik.pzks2

import Modeller._
import scalax.collection.Graph
import scalax.collection.GraphPredef.any2EdgeAssoc
import scalax.collection.edge.Implicits.edge2WDiEdgeAssoc
import scalax.collection.edge.WDiEdge
import scala.swing.Label
import java.awt.Graphics2D
import java.awt.Color
import java.awt.Dimension
import scala.swing.ScrollPane

object SchedUtils {
  private def buildTask(v: Vertex, g: Graph[Vertex, WDiEdge]): Task = {
    val node = g.nodes.get(v)
    val depends = node.diPredecessors.map { pred =>
      val task = buildTask(pred, g)
      val edgeVal = g.edges.find(pred.value ~> v % 0).get.weight
      Dep(task, edgeVal.toInt)
    }.toSeq
    Task(v.id, v.value, depends)
  }

  def buildTasks(vs: List[Vertex], g: Graph[Vertex, WDiEdge]): List[Task] =
    vs.map(v => buildTask(v, g))


  def makeUi(env: Env)= new ScrollPane(new Label{
    val cpuHeight = 20
    val linkHeight = 20
    val margin = 20
    val stepWidth = 20
    
    val width = env.cpuMax*stepWidth + 100
    val height = env.lines.map(line => 1 + line.sends.size * 2).sum * linkHeight + linkHeight
    
    minimumSize_=(new Dimension(width, height))
    preferredSize_=(new Dimension(width, height))

    private def draw(i: Int,  y: Int, state: State, g: Graphics2D){
      val x = margin +i*stepWidth
      state match{
        case Work(_, Task(id, _, _)) => 
          g.setColor(Color.RED)
          g.fillRect(x, y, stepWidth, cpuHeight)
          g.setColor(Color.BLACK)
          g.drawString(""+id, x+1, y+cpuHeight/2)
        case Move(_,task, _) => 
          g.fillRect(x, y, stepWidth, linkHeight/2)
          g.setColor(Color.WHITE)
          g.drawString(""+task.id, x+1, y+linkHeight/2)
        case _ =>
      }
    }

    private def paintLine(line: TimeLine, y: Int, g: Graphics2D):Int={
      g.setColor(Color.BLACK)
      g.drawString(""+line.proc, 0, y)
      g.setColor(Color.RED)
      for{
        i <- 0 until TimeLine.N
        cpu = line.cpu(i)
      } draw(i, y, cpu, g)

      val keys = line.sends.keys.toList
      for{ 
        j <- 0 until keys.size
        key = keys(j)
        my = y+cpuHeight+j*linkHeight
        _ = g.drawString(""+key, 0, my+linkHeight/2)
        _ = g.drawLine(0, my, 5000, my)
        sends = line.sends(key)
        receives = line.receives(key)
        i <- 0 until TimeLine.N
        send = sends(i)
        receive = receives(i)
      } {
        g.setColor(Color.BLUE)
        draw(i, my, send, g)
        g.setColor(Color.GREEN)
        draw(i, my+linkHeight/2, receive, g)
      }
      cpuHeight + 10 + keys.size * linkHeight
    }

    override def paint(g: Graphics2D){
      g.setBackground(Color.WHITE)

      env.lines.foldLeft(50)((y, line) => y + paintLine(line, y, g))
    }
  })
}