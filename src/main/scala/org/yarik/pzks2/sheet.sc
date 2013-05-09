package org.yarik.pzks2

import scalax.collection.GraphPredef._
import scalax.collection.edge.{ WDiEdge, WUnDiEdge }
import scalax.collection.edge.Implicits._
import scalax.collection.mutable.Graph

object sheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val tg = {
  	val v1 = Vertex(1); v1.value = 3
  	val v2 = Vertex(2); v1.value = 2
  	val v3 = Vertex(3); v1.value = 1
  	
  	Graph(v1 ~> v3 % 1, v2 ~> v3 % 2)
  }                                               //> tg  : scalax.collection.mutable.Graph[org.yarik.pzks2.Vertex,scalax.collecti
                                                  //| on.edge.WDiEdge] = Graph(1 (1), 2 (0), 3 (0), 1 (1)~>3 (0) %1, 2 (0)~>3 (0) 
                                                  //| %2)
  
  val sg = {
  	val v1 = Vertex(1);
  	val v2 = Vertex(2);
  	val v3 = Vertex(3);
  	
  	Graph(v1 ~> v3 % 1, v2 ~> v3 % 2)
  }                                               //> sg  : scalax.collection.mutable.Graph[org.yarik.pzks2.Vertex,scalax.collecti
                                                  //| on.edge.WDiEdge] = Graph(1 (0), 2 (0), 3 (0), 1 (0)~>3 (0) %1, 2 (0)~>3 (0) 
                                                  //| %2)
  
  val sched = new GraphSched                      //> sched  : org.yarik.pzks2.GraphSched = org.yarik.pzks2.GraphSched@e893b10
  val env = sched.transformAndSchedule(sg, tg)    //> node 1 has maxW 1
                                                  //| node 3 has maxW 0
                                                  //| node 2 has maxW 0
                                                  //| env  : org.yarik.pzks2.Modeller.Env = Env(List(P[3] slots:[W(task:1(1))] dat
                                                  //| a:, P[1] slots:[W(task:2(0))] data:, P[2] slots:[Idle] data:))
  
  env.lines.foreach(println)                      //> P[3] slots:[W(task:1(1))] data:
                                                  //| P[1] slots:[W(task:2(0))] data:
                                                  //| P[2] slots:[Idle] data:
  
  
  
}