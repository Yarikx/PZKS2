package org.yarik.pzks2

import scalax.collection.GraphPredef._
import scalax.collection.edge.{ WDiEdge, WUnDiEdge }
import scalax.collection.edge.Implicits._
import scalax.collection.mutable.Graph

object sheet {
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
  
  val sched = new GraphSched                      //> sched  : org.yarik.pzks2.GraphSched = org.yarik.pzks2.GraphSched@7e6a6034
  sched.transformAndSchedule(sg, tg).lines.foreach(println)
                                                  //> node 1 has maxW 4
                                                  //| node 2 has maxW 3
                                                  //| node 3 has maxW 1
                                                  //| =========time(0)======
                                                  //| P[3] slots:[W[0](task:1(3))] data:
                                                  //| P[1] slots:[W[0](task:2(2))] data:
                                                  //| P[2] slots:[Idle] data:
                                                  //| =========time(0)======
                                                  //| =========time(1)======
                                                  //| P[3] slots:[W[0](task:1(3)), W[0](task:1(3))] data:
                                                  //| P[1] slots:[W[0](task:2(2)), W[0](task:2(2))] data:
                                                  //| P[2] slots:[Idle, Idle] data:
                                                  //| =========time(1)======
                                                  //| =========time(2)======
                                                  //| P[3] slots:[W[0](task:1(3)), W[0](task:1(3)), W[0](task:1(3))] data:
                                                  //| P[1] slots:[W[0](task:2(2)), W[0](task:2(2)), Idle] data:2(2)
                                                  //| P[2] slots:[Idle, Idle, Idle] data:
                                                  //| =========time(2)======
                                                  //| =========time(3)======
                                                  //| P[3] slots:[W[0](task:1(3)), W[0](task:1(3)), W[0](task:1(3)), W[3](task:3(1
                                                  //| ))] data:1(3)
                                                  //| P[1] slots:[W[0](task:2(2)), W[0](task:2(2)), Idle, Idle] data:2(2)
                                                  //| P[2] slots:[Idle, Idle, Idle, Idle] data:
                                                  //| =========time(3)======
                                                  //| P[3] slots:[W[0](task:1(3)), W[0](task:1(3)), W[0](task:1(3)), W[3
                                                  //| Output exceeds cutoff limit.
  
  
  
}