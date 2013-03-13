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

    type Path = List[NodeT]
    @tailrec
    def findAllPathes(cur: Set[Path]): Set[Path] = {
      val np = cur.flatMap { path =>
        val sucs = path.head.diSuccessors.toList
        val newPaths = sucs.map(x => x :: path)
        println(s"new path = $newPaths")
        newPaths
      }.toSet
      if (np.isEmpty) {
        cur
      } else {
        findAllPathes(np)
      }
    }

    def bestPath(node: NodeT)={
      findAllPathes(Set(List(node)))
        .maxBy(_.map(_.value.value).sum)
    }

    val good = for {
      node <- g.nodes
      d <- dst
      path <- node.pathTo(d, edgeFilter = { edge =>
        val n1 = edge.from
        val n2 = edge.to

        n1.pathTo(n2, edgeFilter = _ != edge).isEmpty
      })
    } yield {
      val n = result(node) + 1
      (node, n)
    }

    val bad = (g.nodes -- good.map(_._1)).map(x => (x, 1))



    val all = good ++ bad
    val sums = g.nodes.map(x=> (x, bestPath(x).map(_.value.value).sum)).toMap
    val maxW = sums.values.max
    val maxP = all.maxBy(_._2)._2.toDouble

    val res = all.map {
      case (node, n) =>
        node -> (sums(node).toDouble / maxW + n.toDouble / maxP)
    }.toMap.toList.sortBy(_._2).reverse

    println("===============")
    res.map { case (node, w) => s"${node.value.id} -> $w" }.foreach(println)

  }

}
