package org.yarik.pzks2

import java.awt.Dimension
import java.awt.Rectangle
import scala.collection.JavaConversions
import scala.collection.immutable.HashMap
import scala.swing.BorderPanel
import scala.swing.Button
import scala.swing.FlowPanel
import scala.swing.MainFrame
import scala.swing.TextField
import org.jgraph.graph.DefaultEdge
import org.jgraph.graph.GraphConstants
import org.jgrapht.DirectedGraph
import org.jgrapht.alg.CycleDetector
import org.jgrapht.ext.JGraphModelAdapter
import org.jgrapht.graph.ListenableDirectedWeightedGraph
import org.jgrapht.graph.ListenableUndirectedWeightedGraph
import scala.swing.event.ButtonClicked
import scala.swing.event.MouseClicked
import scala.swing.Panel

object UiHelper {

  var lastId = 0;

  def createTopView(isDirected: Boolean) = {

    abstract case class Vertex(id: Int) {
      val value: Int
      override def toString = "%d (%d)".format(id, value)

    }

    def createMyModel =
      if (isDirected)
        new ListenableDirectedWeightedGraph[Vertex, DefaultEdge](classOf[DefaultEdge]);
      else new ListenableUndirectedWeightedGraph[Vertex, DefaultEdge](classOf[DefaultEdge]);

    val graph = createMyModel
    val graphComp = new SGraph
    val jg = graphComp.peer
    val model = new JGraphModelAdapter(graph)

    val valueField = new TextField(5)

    implicit def int2Vertex(id: Int) = new Vertex(id) { val value = valueField.text.toInt }

    def createPoint(id: Int, x: Int, y: Int) {
      val v: Vertex = id

      graph.addVertex(v)

      val cell = model.getVertexCell(v)
      val attr = cell.getAttributes()
      val b = GraphConstants.getBounds(attr);
      GraphConstants.setBounds(attr, new Rectangle(x, y, b.getWidth().intValue(), b.getHeight().intValue()));
      val cellAttr = HashMap(cell -> attr);
      val jmap = JavaConversions.mapAsJavaMap(cellAttr)
      model.edit(jmap, null, null, null)
    }

    def connect(from: Int, to: Int) {
      val edge = graph.addEdge(from, to)

    }

    val top = {

      val graphComp = new SGraph
      val jg = graphComp.peer

      jg.setModel(model)

      val b1 = new Button("add vertex")
      val from = new TextField(5)
      val to = new TextField(5)
      val b2 = new Button("add edge")
      val action = new Button(if (isDirected) "find cycle" else "find blind")

      val contents = new BorderPanel() {
        add(graphComp, BorderPanel.Position.Center)
        add(new FlowPanel(valueField, b1, from, to, b2, action), BorderPanel.Position.South)

        listenTo(b1, from, b2, graphComp.mouse.clicks, action)

        reactions += {
          case ButtonClicked(`b1`) =>
            lastId += 1

          case ButtonClicked(`b2`) =>
            connect(from.text.toInt, to.text.toInt)

          case ButtonClicked(`action`) =>
            graph match {
              case d: DirectedGraph[_, _] =>
                val cd = new CycleDetector(d)
                val hasCycles = cd.detectCycles()
                println(if (hasCycles) "has cycles" else "do not has cycles")
              case _ => println("another mode")
            }

          case MouseClicked(`graphComp`, point, mod, c, t) =>
            val x = point.getX().intValue()
            val y = point.getY().intValue()
            lastId += 1
            createPoint(lastId, x, y)
        }
      }
      contents
    }

    top
  }

}