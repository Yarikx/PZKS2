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

class GraphSched() {
  
  val button = new Button("show")

  val panel = new FlowPanel(l("show"), button) {

    listenTo(button)

    reactions += {
      case ButtonClicked(`button`) =>
        show()
    }
  }
  
  val hasAsyncIO = false;

  def show() {
    val systemDiGraph = SystemUi.g
    val taskGraph = TaskUi.g
    
    val systemGraph = {
    val edges = systemDiGraph.edges.map { edge =>
      val l = edge.from.value
      val r = edge.to.value
      val w = edge.value.weight
      l ~ r % w
    }.toArray 

    Graph[Vertex, WUnDiEdge](edges: _*)
  }
  
  val sorted = new Sorter(taskGraph).sort
  
  val sortedSystem = systemGraph.nodes.toList.sortBy(x => x.degree).reverse
    
    println(sorted)
    println(sortedSystem)
  }
  

}