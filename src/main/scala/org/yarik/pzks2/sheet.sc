package org.yarik.pzks2

import scalax.collection.GraphPredef._
import scalax.collection.edge.{ WDiEdge, WUnDiEdge }
import scalax.collection.edge.Implicits._
import scalax.collection.mutable.Graph

object sheet{
  val tg = {
  	val v1 = Vertex(1); v1.value = 3
  	val v2 = Vertex(2); v2.value = 2
  	val v3 = Vertex(3); v3.value = 1
  	
  	Graph(v1 ~> v3 % 1, v2 ~> v3 % 2)
  }                                               //> tg  : scalax.collection.mutable.Graph[org.yarik.pzks2.Vertex,scalax.collecti
                                                  //| on.edge.WDiEdge] = Graph(1 (3), 2 (2), 3 (1), 1 (3)~>3 (1) %1, 2 (2)~>3 (1) 
                                                  //| %2)
  
  val sg = {
  	val v1 = Vertex(1);
  	val v2 = Vertex(2);
  	val v3 = Vertex(3);
  	
  	Graph(v1 ~> v3 % 1, v2 ~> v3 % 2)
  }                                               //> sg  : scalax.collection.mutable.Graph[org.yarik.pzks2.Vertex,scalax.collecti
                                                  //| on.edge.WDiEdge] = Graph(1 (0), 2 (0), 3 (0), 1 (0)~>3 (0) %1, 2 (0)~>3 (0) 
                                                  //| %2)
  
  val sched = new GraphSched                      //> sched  : org.yarik.pzks2.GraphSched = org.yarik.pzks2.GraphSched@28d3335d
  sched.transformAndSchedule(sg, tg).lines.foreach(println)
                                                  //> TimeLine(P[3],List(Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idl
                                                  //| e),Map(2 -> List(Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle)
                                                  //| , 1 -> List(Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle)))
                                                  //| TimeLine(P[2],List(Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idl
                                                  //| e),Map(3 -> List(Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle)
                                                  //| ))
                                                  //| TimeLine(P[1],List(Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idl
                                                  //| e),Map(3 -> List(Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle, Idle)
                                                  //| ))
    
  
  
}