package org.yarik.pzks2

import scala.swing.BorderPanel
import scala.swing.Button
import scala.swing.Dimension
import scala.swing.FlowPanel
import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication
import scala.swing.TextField
import scala.swing.event.ButtonClicked
import org.jgrapht.ext.JGraphModelAdapter
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.ListenableDirectedGraph
import org.jgraph.graph.DefaultGraphCell
import org.jgraph.graph.DefaultPort
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import scala.swing.event.MouseEvent
import scala.swing.event.MouseClicked
import scala.swing.event.MouseClicked
import scala.collection.JavaConversions._
import org.jgraph.graph.GraphConstants
import java.awt.Rectangle
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions

object Gapp extends SimpleSwingApplication {

  implicit def int2Vertex(id: Int) = Vertex(id)
  implicit def int2Edge(id: Int) = Edge(id)

  case class Vertex(id: Int)
  case class Edge(id: Int)
  val graph = createMyModel
  val graphComp = new SGraph
  val jg = graphComp.peer
  val model = new JGraphModelAdapter(graph)
  var lastId = 0;

  def top = new MainFrame {
    title = "Graphs"

    val graphComp = new SGraph
    val jg = graphComp.peer
    
    jg.setModel(model)

    val b1 = new Button("add vertex")
    val edit = new TextField
    val b2 = new Button("add edge")

    contents = new BorderPanel() {
      add(graphComp, BorderPanel.Position.Center)
      add(new FlowPanel(b1, b2), BorderPanel.Position.South)

      listenTo(b1, edit, b2, graphComp.mouse.clicks)

      reactions += {
        case ButtonClicked(`b1`) =>
          lastId += 1

        case ButtonClicked(`b2`) =>
          lastId += 1
          graph.addVertex(lastId)

        case MouseClicked(`graphComp`, point, mod, c, t) =>
          val x = point.getX().intValue()
          val y = point.getY().intValue()
          lastId += 1
          createPoint(lastId, x, y)
      }
    }
    size = new Dimension(500, 400)

  }

  def createPoint(id: Int, x: Int, y: Int) {
    val v = new Vertex(id)
    
    
    graph.addVertex(v)
    
    val cell = model.getVertexCell(v)
    val attr = cell.getAttributes()
    val b    = GraphConstants.getBounds( attr );
    GraphConstants.setBounds( attr, new Rectangle( x, y, b.getWidth().intValue(), b.getHeight().intValue() ) );
    val cellAttr = HashMap(cell -> attr);
    val jmap = JavaConversions.mapAsJavaMap(cellAttr)
    model.edit(jmap, null, null, null)
  }

  def createMyModel = 
    new ListenableDirectedGraph[Vertex, DefaultEdge](classOf[DefaultEdge]);

}

