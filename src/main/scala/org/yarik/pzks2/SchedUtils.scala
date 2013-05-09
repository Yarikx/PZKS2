package org.yarik.pzks2

import Modeller._
import scalax.collection.Graph
import scalax.collection.edge.WDiEdge
import scalax.collection.edge.WUnDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._

object SchedUtils {
  private def buildTask(v: Vertex, g: Graph[Vertex, WDiEdge]): Task = {
      val node = g.nodes.get(v)
      val depends = node.diPredecessors.map { pred =>
        val task = buildTask(pred, g)
        val edgeVal = g.edges.find(pred.value ~> v % 0).get.weight
        Dep(task, edgeVal.toInt)
      }.toSeq
      val task = Task(v.id, v.value, depends)
      task
    }
  
  
  def tasks(vs: Seq[Vertex], g: Graph[Vertex, WDiEdge]):Seq[Task] =
    vs.map(v => buildTask(v, g))
}