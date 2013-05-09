package org.yarik.pzks2

import Modeller.Dep
import Modeller.Task
import scalax.collection.Graph
import scalax.collection.GraphPredef.any2EdgeAssoc
import scalax.collection.edge.Implicits.edge2WDiEdgeAssoc
import scalax.collection.edge.WDiEdge

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
}