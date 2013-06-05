package org.yarik.pzks2

import UiHelper._
import scala.annotation.tailrec
import scalax.collection.GraphTraversal.VisitorReturn._
import scala.swing.{ Button, FlowPanel }
import scala.swing.event.ButtonClicked
import scalax.collection.edge.WDiEdge
import scalax.collection.Graph
import scalax.collection.GraphTraversal._

class Sorter(g: Graph[Vertex, WDiEdge], algorithm: Int) {
    val pathFromPred = algorithm != 7

    private[this] def neighbours(node: NodeT) = (if (pathFromPred) node.diPredecessors else node.diSuccessors).toList
    private[this] def degree(node: NodeT) = if (pathFromPred) node.outDegree else node.inDegree

    type NodeT = Graph[Vertex, WDiEdge]#NodeT
    type Path = List[NodeT]
    private[this] def recurBuildCriticalPathNums[A <: NodeT](step: Int, cur: Set[NodeT], acum: Map[NodeT, Int]): Map[NodeT, Int] = {
      val curStep = step + 1
      val set: Set[NodeT] = cur.flatMap(neighbours)
      if (!set.isEmpty) {
        val map = set.foldLeft(acum)((m, x) => m + (x -> (1 + step)))
        recurBuildCriticalPathNums(curStep, set, map)
      } else {
        acum
      }
    }
    @tailrec
    private[this] def findAllPathes(cur: Set[Path]): Set[Path] = {
      val np = cur.flatMap { path =>
        val neig = path.head.diSuccessors

        val sucs = neig.toList
        val newPaths = sucs.map(x => x :: path)
        newPaths
      }.toSet
      if (np.isEmpty) cur
      else findAllPathes(np)
    }

    private[this] def criticalPath(node: NodeT) = {
      findAllPathes(Set(List(node)))
        .maxBy(_.map(_.value.value).sum)
    }
    
    def normalizeMap[K](map: Map[K, Int])={
      val max = map.values.max
      map.map{case (x, y) => x -> y.asInstanceOf[Double]/max}
    }

    def sort = {
      val nodes = g.nodes.toList
      val weigths = nodes.map(x => x -> criticalPath(x).map(_.value.value).sum).toMap
      //find nodes that has degree == 0
      val boundaryNodes = nodes.filter(degree(_) == 0)

      //boundary nodes has critical path = 0 
      val startMap: Map[NodeT, Int] = boundaryNodes.map(n => n -> 0).toMap

      val criticalPathLengths = recurBuildCriticalPathNums(0, boundaryNodes.toSet, startMap)

      val sorted = algorithm match {
        case 3 => nodes.sortBy(weigths).reverse
        case 1 =>
          val normalizedWeights = normalizeMap(weigths)
          val normalizedPaths = normalizeMap(criticalPathLengths)
          nodes.sortBy(n => normalizedWeights(n) + normalizedPaths(n)).reverse
        case 7 => 
          val connectivity = nodes.map(x => x -> x.neighbors.size).toMap
          nodes.sortBy(connectivity).reverse.sortBy(criticalPathLengths)
      }

      sorted.map(_.value)
    }
  }

object Sorter{
  val algs = Seq(1,3,7)
}
