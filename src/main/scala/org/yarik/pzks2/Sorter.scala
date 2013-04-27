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

  def sort() = {

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
    def findAllPathes(down: Boolean)(cur: Set[Path]): Set[Path] = {
      val np = cur.flatMap { path =>
        val neig = if(down) path.head.diSuccessors
        else path.head.diPredecessors
          
        val sucs = neig.toList
        val newPaths = sucs.map(x => x :: path)
        newPaths
      }.toSet
      if (np.isEmpty) {
        cur
      } else {
        findAllPathes(down)(np)
      }
    }
//
//    def bestPath(node: NodeT)={
//      findAllPathes(false)(Set(List(node)))
//        .maxBy(_.map(_.value.value).sum)
//    }
    
    def maxWe(node: NodeT)={
      findAllPathes(true)(Set(List(node)))
        .maxBy(_.map(_.value.value).sum)
    }

//    val good = for {
//      node <- g.nodes
//      d <- dst
//      path <- node.pathTo(d, edgeFilter = { edge =>
//        val n1 = edge.from
//        val n2 = edge.to
//
//        n1.pathTo(n2, edgeFilter = _ != edge).isEmpty
//      })
//    } yield {
//      val n = result(node) + 1
//      (node, n)
//    }

    //val bad = (g.nodes -- good.map(_._1)).map(x => (x, 1))
    
    
    //val all = (good ++ bad).toMap
    //val sums = g.nodes.map(x=> (x, bestPath(x).map(_.value.value).sum)).toMap
    val ws = g.nodes.map(x=> (x, maxWe(x).map(_.value.value).sum)).toMap
    //val maxW = sums.values.max
    //val maxP = all.maxBy(_._2)._2.toDouble
    //val connectivity = g.nodes.map(x => x -> (x.diSuccessors.size + x.diPredecessors.size)).toMap

    val sorted = g.nodes.toList.sortBy(ws(_)).reverse

    sorted.foreach{ node =>
      println(s"node ${node.value.id} has maxW ${ws(node)}")
    }
    
    sorted

  }

}
