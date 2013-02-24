package org.yarik.pzks2

import com.mxgraph.model.mxGeometry
import java.awt.BorderLayout
import javax.swing.JPanel
import org.jgrapht.{ DirectedGraph, UndirectedGraph }
import org.jgrapht.alg.{ ConnectivityInspector, CycleDetector }
import org.jgrapht.graph.{ ListenableDirectedWeightedGraph, ListenableUndirectedWeightedGraph, MyEdge }
import scala.collection.JavaConversions._
import scala.swing.event.Key
import scala.swing.{ BorderPanel, Button, FlowPanel, TextField }
import scala.swing.event.{ ButtonClicked, MouseClicked }
import com.mxgraph.swing.mxGraphComponent
import java.awt.event.MouseEvent
import java.awt.event.MouseAdapter
import java.awt.event.InputEvent

object UiHelper {

  var lastId = 0;

  def createTopView(isDirected: Boolean) = {

    def createMyModel = {
      if (isDirected)
        new ListenableDirectedWeightedGraph[Vertex, MyEdge](classOf[MyEdge]);
      else new ListenableUndirectedWeightedGraph[Vertex, MyEdge](classOf[MyEdge]);
    }

    abstract case class Vertex(id: Int) {
      val value: Int
      override def toString = "%d (%d)".format(id, value)
    }

    val valueField = new TextField(5)
    valueField.text = "2"
    implicit def int2Vertex(id: Int) = new Vertex(id) { val value = valueField.text.toInt }

    val graph = createMyModel
    graph.addVertex(new Vertex(4) { val value = 5 })

    val adapter = new JGraphXAdapter(graph)

    def createPoint(id: Int, x: Int, y: Int) {
      val v: Vertex = id

      adapter.getModel().beginUpdate()
      graph.addVertex(v)
      //adapter.addJGraphTVertex(v)
      val cell = adapter.getVertexToCellMap().get(v)
      cell.setGeometry(new mxGeometry(x.toDouble, y.toDouble, 20.0, 20.0))
      adapter.getModel().endUpdate()
    }

    def connect(from: Int, to: Int) {
      val edge = graph.addEdge(from, to)
      graph.setEdgeWeight(edge, valueField.text.toInt)
    }

    val top = {

      val b1 = new Button("add vertex")
      val from = new TextField(5)
      val to = new TextField(5)
      val b2 = new Button("add edge")
      val action = new Button(if (isDirected) "find cycle" else "find blind")
      val del = new Button("delete")
      val graphComp = new SGraph(adapter)

      val contents = new BorderPanel() {
        add(graphComp, BorderPanel.Position.Center)
        add(new FlowPanel(valueField, b1, from, to, b2, action, del), BorderPanel.Position.South)

        listenTo(b1, from, b2, action, del, graphComp.mouse.clicks)
        
        graphComp.peer.getGraphControl().addMouseListener(new MouseAdapter(){
          override def mouseClicked(evt: MouseEvent){
            val x = evt.getX()
            val y = evt.getY()
            println("clicked "+evt.getModifiers())
            
            if ((evt.getModifiers() & InputEvent.CTRL_MASK) != 0) {
              println("adding")
              lastId += 1
              createPoint(lastId, x, y)
            }
          }
        });

        reactions += {
          case ButtonClicked(`b1`) =>
            lastId += 1

            createPoint(lastId, 300, 300)
            println(asScalaSet(graph.vertexSet()).mkString)
            println(graph.edgeSet().mkString(", "))

          case ButtonClicked(`b2`) =>
            connect(from.text.toInt, to.text.toInt)

          case ButtonClicked(`action`) =>
            graph match {
              case d: DirectedGraph[_, _] =>
                val cd = new CycleDetector(d)
                val hasCycles = cd.detectCycles()
                println(if (hasCycles) "has cycles" else "do not has cycles")
              case ug: UndirectedGraph[_ , _] =>
                val ci = new ConnectivityInspector(ug)
                val sets = ci.connectedSets()
                println("connectivity sets")
                println(if (sets.size() == 1) "ok" else "fail")
            }

          case MouseClicked(`graphComp`, point, mod, c, t) =>
            val x = point.getX().intValue()
            val y = point.getY().intValue()
            println("clicked")

            if (mod == Key.Modifier.Control) {
              println("adding")
              lastId += 1
              createPoint(lastId, x, y)
            }

          case ButtonClicked(`del`) =>
            graph.removeVertex(valueField.text.toInt)

        }
      }
      contents
    }

    top
  }

}
