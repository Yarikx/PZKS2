package org.yarik.pzks2

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.FileWriter
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import javax.swing.ImageIcon
import scala.collection.JavaConversions._
import scala.swing.FileChooser
import scala.swing.{ BorderPanel, BoxPanel, Button, Component, FlowPanel, Label, Orientation, TextField }
import scala.swing.event.ButtonClicked
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge
import scalax.collection.io.dot._
import scalax.collection.mutable.Graph

object UiHelper {
  var lasGraph: Graph[Vertex, WDiEdge] = null
}

// type MyGraph = DefaultListenableGraph[Vertex, MyEdge] with WeightedGraph[Vertex, MyEdge] with Serializable
//  type Adapter = JGraphXAdapter[Vertex, MyEdge] with Serializable

case class Vertex(id: Int, value: Double) {
  override def toString() = s"$id ($value)"
}

class Gui(val isDirected: Boolean) {

  private val imagePath = "/tmp/graph_image.png"
  private lazy val image = new ImageIcon(imagePath)

  val g: Graph[Vertex, WDiEdge] = Graph()

  def findV(id: Int): Option[Vertex] =
    g.nodes.find((v: Vertex) => v.id == id).map(_.value)

  def findE(from: Vertex, to: Vertex) =
    g.find(from ~> to % 1)

  def edgeTransformer(root: DotRootGraph)(innerEdge: scalax.collection.Graph[Vertex, WDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
    val edge = innerEdge.edge
    val label = edge.value.weight.toString
    Some((root,
      DotEdgeStmt(edge.from.toString,
        edge.to.toString, List(DotAttr("label", label)))))
  }

  def nodeTransformer(root: DotRootGraph)(innerNode: scalax.collection.Graph[Vertex, WDiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] = {
    val node = innerNode.value
    val label = node.toString
    Some((root, DotNodeStmt(node.toString, Seq(DotAttr("label", label)))))
  }

  def display(s: String) {
    Dialog.showMessage(
      message = s,
      title = " ")
  }

  private def updateTop() = content.revalidate()

  val content: Component = {

    // def createMyModel = {
    //   if (isDirected)
    //     new ListenableDirectedWeightedGraph[Vertex, MyEdge](classOf[MyEdge]);
    //   else new ListenableUndirectedWeightedGraph[Vertex, MyEdge](classOf[MyEdge]);
    // }

    // val graph = g.getOrElse(createMyModel)

    // val adapter = new JGraphXAdapter(graph);
    // if(!isDirected){
    //   val s = adapter.getStylesheet().getDefaultEdgeStyle()
    //   s.remove(mxConstants.STYLE_ENDARROW)
    // }

    // a.foreach { seq =>
    //   adapter.getModel().beginUpdate()
    //   val cells = adapter.getVertexToCellMap().values().toSeq
    //   cells.zip(seq).foreach {
    //     case (cell, geom) => adapter.getModel().setGeometry(cell, geom);
    //   }

    //   adapter.getModel().endUpdate()
    // }

    // def createPoint(id: Int, x: Int, y: Int) {
    //   val v: Vertex = id

    //   adapter.getModel().beginUpdate()
    //   graph.addVertex(v)
    //   //adapter.addJGraphTVertex(v)
    //   val cell = adapter.getVertexToCellMap().get(v)
    //   cell.setGeometry(new mxGeometry(x.toDouble, y.toDouble, 30.0, 30.0))
    //   adapter.getModel().endUpdate()
    // }

    // def connect(from: Int, to: Int) {
    //   val edge = graph.addEdge(from, to)
    //   graph.setEdgeWeight(edge, valueField.text.toInt)
    // }

    // def disconnect(from: Int, to: Int) {
    //   val edge = graph.getEdge(from, to);
    //   graph.removeEdge(edge)
    // }

    val top: Component = {

      var lastId = 0;

      val addVertex = new Button("V")
      val removeEdge = new Button("remove edge")
      val fromField = new TextField(5)
      val toField = new TextField(5)
      val addEdge = new Button("E")
      val action = new Button(if (isDirected) "find cycle" else "find blind")
      val del = new Button("delete Vertex")

      val editVertex = new Button("V")
      val editEdge = new Button("E")

      val save = new Button("save")
      val load = new Button("load")

      val graphComp = new Label("this is graph");

      def update() {
        println(g)
        val root = DotRootGraph(isDirected, None, false, Seq())
        val f = edgeTransformer(root) _
        val dot = g.toDot(root, f, iNodeTransformer = Some(nodeTransformer(root) _))
        //File magic
        val file = new FileWriter("/tmp/pzks.dot")
        file.write(dot)
        file.close()

        val r = Runtime.getRuntime()
        r.exec("dot -Tpng -o " + imagePath + " /tmp/pzks.dot ").waitFor()
        //r.exec("eog /tmp/pzks.png")

        image.getImage().flush()
        println("image size = %dx%d" format (image.getIconWidth, image.getIconHeight()))
        graphComp.text = ""
        graphComp.icon = image
        graphComp.repaint
        UiHelper.lasGraph = g
        updateTop()
      }

      val contents = new BorderPanel() {
        add(graphComp, BorderPanel.Position.Center)
        val valueField = new TextField(5)
        valueField.text = "1"

        def createGet(ef: TextField) = () =>
          try {
            Some(ef.text.toInt)
          } catch {
            case _: Throwable => None
          }

        val from = createGet(fromField)
        val to = createGet(toField)
        val value = createGet(valueField)

        def l(s: String) = new Label(s)

        add(new BoxPanel(Orientation.Vertical) {
          contents += new FlowPanel(valueField, fromField, toField, l("add"), addVertex, addEdge, new Label("edit"), editVertex, editEdge)
          contents += new FlowPanel(removeEdge, action, del, save, load)
        }, BorderPanel.Position.South)

        listenTo(addVertex, removeEdge, fromField, addEdge, action, del, graphComp.mouse.clicks, save, load, editVertex, editEdge)

        reactions += {

          case ButtonClicked(`addVertex`) =>
            println("adding")

            lastId += 1
            val we = value().getOrElse(1)
            val vertex = Vertex(lastId, we)
            g += vertex
            update()
          case ButtonClicked(`removeEdge`) =>
            for {
              f <- from();
              t <- to();
              v1 <- findV(f);
              v2 <- findV(t);
              e <- findE(v1, v2)
            } {
              g -= e
              update()
            }

          case ButtonClicked(`del`) =>
            val v = findV(fromField.text.toInt)
            g.remove(v)

          case ButtonClicked(`addEdge`) =>
            for {
              f <- from();
              t <- to();
              v1 <- findV(f);
              v2 <- findV(t)
            } {
              g += (v1 ~> v2 % 1)
              update()
            }

          //       case ButtonClicked(`editVertex`) =>
          //         findV(from.text.toInt).foreach { v =>
          //           v.value = value.text.toInt
          //           adapter.updateV(v)
          //         }

          //       case ButtonClicked(`editEdge`) =>
          //         for {
          //           from <- findV(from.text.toInt);
          //           to <- findV(to.text.toInt);
          //           edge <- findE(from, to);
          //           weight = value.text.toDouble
          //         } {
          //           edge.w = weight
          //           graph.setEdgeWeight(edge, weight)
          //           adapter.updateE(edge)
          //         }

          case ButtonClicked(`action`) =>
            if (isDirected) {
              display(if (g.isAcyclic) "has no sycles" else "has cycles")
            } else {

            }

          case ButtonClicked(`save`) =>
            val fc = new FileChooser(new File("/tmp"))
            fc.showSaveDialog(this)
            val file = fc.selectedFile;
            val dos = new ObjectOutputStream(new FileOutputStream(file))
            dos.writeObject(g)
            dos.close()

          case ButtonClicked(`load`) =>
            val fc = new FileChooser(new File("/tmp"))
            fc.showOpenDialog(this)
            val file = fc.selectedFile;
            val dos = new ObjectInputStream(new FileInputStream(file))
            println("before load")
            try {
              val ng = dos.readObject().asInstanceOf[Graph[Vertex, WDiEdge]]
              println(ng)
              g.clear

              g ++= ng

              dos.close()
              update()
            } catch {
              case e: Throwable => e.printStackTrace()
            }

        }

      }
      contents
    }

    top

  }

}
