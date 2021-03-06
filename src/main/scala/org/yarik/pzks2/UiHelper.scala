package org.yarik.pzks2

import UiHelper._
import java.awt.{ Dimension, Graphics2D }
import java.io.{ File, FileInputStream, FileOutputStream, FileWriter, ObjectInputStream, ObjectOutputStream }
import javax.swing.ImageIcon
import scala.collection.JavaConversions._
import scala.swing.{ BorderPanel, BoxPanel, Button, Component, Dialog, FileChooser, FlowPanel, Label, Orientation, ScrollPane, TextField }
import scala.swing.event.ButtonClicked
import scalax.collection.GraphPredef._
import scalax.collection.edge.{ WDiEdge, WUnDiEdge }
import scalax.collection.edge.Implicits._
import scalax.collection.io.dot._
import scalax.collection.Graph
import scalax.collection.mutable.{ Graph => MGraph }

object UiHelper {

  def createGetD(ef: TextField) = () =>
    try {
      Some(ef.text.toDouble)
    } catch {
      case _: Throwable => None
    }

  def createGet(ef: TextField) = () =>
    try {
      Some(ef.text.toInt)
    } catch {
      case _: Throwable => None
    }

  def l(s: String) = new Label(s)

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

  def buildImage(g: Graph[Vertex, WDiEdge], isDirected: Boolean, imagePath: String) {
    val root = DotRootGraph(isDirected, None, false, Seq())
    val f = edgeTransformer(root) _
    val dot = g.toDot(root, f, iNodeTransformer = Some(nodeTransformer(root) _))

    val pref = if (isDirected) "d" else "u"
    //File magic
    val fileName = s"/tmp/${pref}_pzks.dot"
    val file = new FileWriter(fileName)
    file.write(dot)
    file.close()

    val r = Runtime.getRuntime()

    r.exec(s"dot -Tpng -o $imagePath $fileName").waitFor()
  }
}

case class Vertex(id: Int, value: Int = 0) {
  override def toString() = s"$id ($value)"
}

object TaskUi extends Gui {
  override def isDirected = true
}

object SystemUi extends Gui {
  override def isDirected = false
}

abstract class Gui() {
  val selfGui = this
  lazy val pref = if (isDirected) "d" else "u"
  def isDirected: Boolean
  private lazy val imagePath = s"/tmp/graph_image_$pref.png"
  private lazy val image = new ImageIcon(imagePath)

  val g: MGraph[Vertex, WDiEdge] = MGraph()

  def findV(id: Int): Option[Vertex] =
    g.nodes.find((v: Vertex) => v.id == id).map(_.value)

  def findE(from: Vertex, to: Vertex) =
    g.find(from ~> to % 1)

  def display(s: String) {
    Dialog.showMessage(
      message = s,
      title = " ")
  }

  val graphComp = new Label("this is graph") {
    override def paint(g: Graphics2D) {
      if (icon != null) {
        icon.paintIcon(this.peer, g, 0, 0)
      } else {
        super.paint(g)
      }
    }
  };

  def update() {
    buildImage(g, isDirected, imagePath)

    image.getImage().flush()
    graphComp.text = ""
    graphComp.icon = image
    val dim = new Dimension(image.getImage().getWidth(null), image.getImage().getHeight(null))
    graphComp.preferredSize = dim
    graphComp.minimumSize = dim
    graphComp.repaint
    content.revalidate()
  }

  val content: Component = {
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

      val contents = new BorderPanel() {
        add(new ScrollPane(graphComp), BorderPanel.Position.Center)
        val valueField = new TextField(5)
        valueField.text = "1"

        val from = createGet(fromField)
        val to = createGet(toField)
        val value = createGet(valueField)

        add(new BoxPanel(Orientation.Vertical) {
          contents += new FlowPanel(valueField, fromField, toField, l("add"), addVertex, addEdge, new Label("edit"), editVertex, editEdge)
          contents += new FlowPanel(removeEdge, action, del, save, load)
          contents += new Generator(isDirected).createControls { ng =>
            g.clear;
            g ++= ng.nodes
            g ++= ng.edges
            update();
          }
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
            v.foreach(g -= _)
            update()

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

          case ButtonClicked(`editVertex`) =>
            for (f <- from(); v <- value(); ver <- findV(f)) {
              val updVer = Vertex(ver.id, v)
              val nodeT = g.nodes.get(ver)
              val oldEdges = nodeT.edges
              val newEdges = oldEdges.map{edge =>
                val from = if(edge.from == nodeT) updVer else edge.from.value
                val to = if(edge.to == nodeT) updVer else edge.to.value
               	from ~> to % edge.weight
              }
              
              g -= ver
              g ++= newEdges
              update()
            }

          case ButtonClicked(`editEdge`) =>
            for (
              f <- from();
              t <- to();
              v <- value();
              v1 <- findV(f);
              v2 <- findV(t);
              edge <- findE(v1, v2)
            ) {
              val newEdge = v1 ~> v2 % v
              g -= edge
              g += newEdge
              update()
            }

          case ButtonClicked(`action`) =>
            display(if (isDirected) {
              if (g.isAcyclic) "has no sycles" else "has cycles"
            } else {
              if (g.isConnected) "connected" else "disconected"
            })

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
