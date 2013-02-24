package org.yarik.pzks2

import scala.collection.JavaConversions._;
import scala.swing.TextField
import com.mxgraph.model.mxGeometry
import org.jgrapht.graph.ListenableUndirectedWeightedGraph
import org.jgrapht.graph.ListenableDirectedWeightedGraph
import org.jgrapht.graph.DefaultListenableGraph
import org.jgrapht.graph.MyEdge
import org.jgrapht.WeightedGraph
import org.jgrapht.Graph
import scala.swing.BorderPanel
import scala.swing.Button
import scala.swing.Component
import java.awt.event.MouseAdapter
import scala.swing.FlowPanel
import scala.swing.event.ButtonClicked
import org.jgrapht.DirectedGraph
import java.awt.event.MouseEvent
import org.jgrapht.alg.CycleDetector
import java.awt.event.InputEvent
import org.jgrapht.UndirectedGraph
import org.jgrapht.alg.ConnectivityInspector
import scala.swing.event.MouseClicked
import scala.swing.event.Key
import scala.swing.FileChooser
import java.io.File
import java.io.ObjectOutputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.FileInputStream

object UiHelper {

  var lastId = 0;

  type MyGraph = DefaultListenableGraph[Vertex, MyEdge] with WeightedGraph[Vertex, MyEdge] with Serializable
  type Adapter = JGraphXAdapter[Vertex, MyEdge] with Serializable

  abstract case class Vertex(id: Int) {
    val value: Int
    override def toString = "%d (%d)".format(id, value)
  }

  var valueField: TextField = null
  implicit def int2Vertex(id: Int) = new Vertex(id) { val value = valueField.text.toInt }

  def createTopView(isDirected: Boolean, g: Option[MyGraph], a: Option[Adapter]): Component = {

    def createMyModel = {
      if (isDirected)
        new ListenableDirectedWeightedGraph[Vertex, MyEdge](classOf[MyEdge]);
      else new ListenableUndirectedWeightedGraph[Vertex, MyEdge](classOf[MyEdge]);
    }

    val graph = g.getOrElse(createMyModel)

//    val adapter = a.getOrElse(new JGraphXAdapter(graph));
//    {
//      adapter.getModel().beginUpdate();
//      adapter.refresh();
//      adapter.getModel().endUpdate();
//      adapter.refresh();
//    }
    val adapter = new JGraphXAdapter(graph);
    {
      val cells = adapter.getVertexToCellMap().values().toSeq
            val size = cells.size
            adapter.getModel().beginUpdate()
            for (i <- 0 until size;
            	 cell = cells(i);
            	 y = (i / 5) * 40 + 100;
            	 x = (i % 5) * 40 + 100) {
              adapter.getModel().setGeometry(cell, new mxGeometry(x, y, 20, 20));
            }
            adapter.getModel().endUpdate()
    }

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

    val top: Component = {

      val b1 = new Button("add vertex")
      val from = new TextField(5)
      val to = new TextField(5)
      val b2 = new Button("add edge")
      val action = new Button(if (isDirected) "find cycle" else "find blind")
      val del = new Button("delete")

      val save = new Button("save")
      val load = new Button("load")

      val graphComp = new SGraph(adapter)

      val contents = new BorderPanel() {
        val bp = this
        add(graphComp, BorderPanel.Position.Center)
        valueField = new TextField(5)
        valueField.text = "1"
        add(new FlowPanel(b1, valueField, from, to, b2, action, del, save, load), BorderPanel.Position.South)

        listenTo(b1, from, b2, action, del, graphComp.mouse.clicks, save, load)

        graphComp.peer.getGraphControl().addMouseListener(new MouseAdapter() {
          override def mouseClicked(evt: MouseEvent) {
            val x = evt.getX()
            val y = evt.getY()
            println("clicked " + evt.getModifiers())

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
              case ug: UndirectedGraph[_, _] =>
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

          case ButtonClicked(`save`) =>
            val fc = new FileChooser(new File("/tmp"))
            fc.showSaveDialog(this)
            val file = fc.selectedFile;
            val dos = new ObjectOutputStream(new FileOutputStream(file))
            dos.writeObject(graph)
            dos.writeObject(adapter)
            dos.close()

          case ButtonClicked(`load`) =>
            val fc = new FileChooser(new File("/tmp"))
            fc.showOpenDialog(this)
            val file = fc.selectedFile;
            val dos = new ObjectInputStream(new FileInputStream(file))
            val g = dos.readObject().asInstanceOf[MyGraph]
            val a = dos.readObject().asInstanceOf[Adapter]
            dos.close()
            val newView = createTopView(isDirected, Some(g), Some(a))
            Gapp.replace(isDirected, newView)

          case ButtonClicked(`b2`) =>
            connect(from.text.toInt, to.text.toInt)

        }
      }
      contents
    }

    def init(graph: Graph[Vertex, MyEdge]) {

    }

    top
  }

}
