package org.yarik.pzks2

import scalax.collection.GraphPredef._
import scalax.collection.edge.{ WDiEdge, WUnDiEdge }
import scalax.collection.edge.Implicits._
import scalax.collection.mutable.Graph
import org.yarik.pzks2.Modeller._
import scalax.collection.GraphEdge.UnDiEdge

object TestApp extends App {
  val tg = {
    val v1 = Vertex(1); v1.value = 3
    val v2 = Vertex(2); v2.value = 2
    val v3 = Vertex(3); v3.value = 1

    Graph(v1 ~> v3 % 1, v2 ~> v3 % 2)
  }

  val sg = {
    val v1 = Vertex(1);
    val v2 = Vertex(2);
    val v3 = Vertex(3);
    val v4 = Vertex(4);

    Graph(v1 ~> v3 % 1, v2 ~> v3 % 2, v4 ~> v2 %1)
  } 

  val sched = new GraphSched 
  val env0 = sched.transformAndSchedule(sg, tg)
  
  val task = Task(1,3, Seq())
  val task2 = Task(2,2, Seq())
  val withTask = env0.startTask(0, env0.lines(0), task)
  // println(withTask)
  // println
  // println
  // val p3 = env0.lines.map(_.proc).find(_.id == 3).get
  // val p2 = env0.lines.map(_.proc).find(_.id == 2).get
  // val p4 = env0.lines.map(_.proc).find(_.id == 4).get
  // val withMove = withTask.move(task, List(p3, p2, p4), 2)
  // println(withMove)
  // println("++++++++++++")
  // val withMove2 = withMove.move(task, List(p3, p2, p4), 2)
  // println(withMove2)
//  implicit val systemG = {
//      val edges = sg.edges.map { edge =>
//        val l = Proc(edge.from.value.id, edge.from.neighbors.map(n => n.id).toList)
//        val r = Proc(edge.to.value.id, edge.to.neighbors.map(n => n.id).toList)
//        l ~ r
//      }.toArray
//
//      Graph[Proc, UnDiEdge](edges: _*)
//    }
//  implicit val procPriors = withTask.lines.map(_.proc)
//  
//  val res = makeStep(env0, List(task, task2))
  val res = sched.transformAndSchedule(sg, tg)
  println("result")
  println(res)
  
}