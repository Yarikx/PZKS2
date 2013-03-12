package org.yarik.pzks2

import UiHelper._
import scala.annotation.tailrec
import scalax.collection.GraphTraversal.VisitorReturn._
import scala.swing.{ Button, FlowPanel }
import scala.swing.event.ButtonClicked
import scalax.collection.edge.WDiEdge
import scalax.collection.mutable.Graph
import scalax.collection.GraphTraversal._

class Sorter(val g: Graph[Vertex, WDiEdge]) {
  val button = new Button("sort")

  val panel = new FlowPanel(l("sort"), button) {

    listenTo(button)

    reactions += {
      case ButtonClicked(`button`) =>
        sort()
    }
  }

  def sort() {

    type NodeT = Graph[Vertex, WDiEdge]#NodeT

    val dst = g.nodes.filter(_.outDegree == 0)

    def recurNumNodes[A <: NodeT](step: Int, cur: Set[NodeT], acum: Map[NodeT, Int]): Map[NodeT, Int] = {
      val curStep = step + 1
      val set: Set[NodeT] = cur.flatMap(_.diPredecessors.toList)
      if (!set.isEmpty) {
        val map = set.foldLeft(acum)((m, x) => m.+(x -> (1 + step)))
        recurNumNodes(curStep, set, map)
      } else {
        acum
      }
    }

    val startMap: Map[NodeT, Int] = dst.map(n => n -> 0).toMap

    val result = recurNumNodes(0, dst.toSet, startMap)

    val good = for {
      node <- g.nodes
      d <- dst
      path <- node.pathTo(d, edgeFilter = { edge =>
        val n1 = edge.from
        val n2 = edge.to

        n1.pathTo(n2, edgeFilter = _ != edge).isEmpty
      })
    } yield {
      val sum = path.nodes.map(_.value.value).sum
      val n = result(node)
      println(s"for node $node, length = $n and sum = $sum")

      (node, sum, n)
    }

    val bad = for {
      node <- g.nodes
      goodNodes = good.map(_._1)
      if !goodNodes.contains(node)
    } yield {
      val sum = node.value.value
      println(s"for node $node, length = 0 and sum = $sum")
      (node, sum, 0)
    }

    val all = good ++ bad
    val maxW = all.maxBy(_._2)._2
    val maxP = all.maxBy(_._3)._3.toDouble
    val res = all.map {
      case (node, w, n) =>
        node -> (w / maxW + n.toDouble / maxP)
    }.toMap.toList.sortBy(_._2).reverse

    println("===============")
    res.map{ case (node, w) => s"${node.value.id} -> $w"}.foreach(println)

  }

}
