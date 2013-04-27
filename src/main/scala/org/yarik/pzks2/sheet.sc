package org.yarik.pzks2

import scalax.collection.GraphPredef._
import scalax.collection.edge.{ WDiEdge, WUnDiEdge }
import scalax.collection.edge.Implicits._
import scalax.collection.mutable.Graph

object sheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  
  val g:Graph[Int, WDiEdge] = Graph[Int, WDiEdge](1~>2%1)
                                                  //> g  : scalax.collection.mutable.Graph[Int,scalax.collection.edge.WDiEdge] = G
                                                  //| raph(1, 2, 1~>2 %1)
  val q = g.edges.map{edge =>
		val l = edge.from.value
		val r = edge.to.value
		val w = edge.value.weight
		l ~ r % w
  }.toArray                                       //> q  : Array[scalax.collection.edge.WUnDiEdge[Int]] = Array(1~2 %1)
  
  val un = Graph[Int, WUnDiEdge](q:_*)            //> un  : scalax.collection.mutable.Graph[Int,scalax.collection.edge.WUnDiEdge] 
                                                  //| = Graph(1, 2, 1~2 %1)
                           
  
  
}