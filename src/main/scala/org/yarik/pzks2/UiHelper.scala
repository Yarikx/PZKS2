package org.yarik.pzks2

import scala.collection.JavaConversions._
import scala.swing.event.ButtonClicked
import scala.swing.{ BorderPanel, BoxPanel, Button, Component, FlowPanel, Label, Orientation, TextField }
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge
import scalax.collection.io.dot._
import scalax.collection.mutable.Graph

object UiHelper {

}

// type MyGraph = DefaultListenableGraph[Vertex, MyEdge] with WeightedGraph[Vertex, MyEdge] with Serializable
//  type Adapter = JGraphXAdapter[Vertex, MyEdge] with Serializable

case class Vertex(id: Int, value: Double)

class Gui(val isDirected: Boolean) {

  val g: Graph[Vertex, WDiEdge] = Graph()

  def findV(id: Int): Option[Vertex] = {
    g.nodes.find((v: Vertex) => v.id == id).map(_.value)
  }

  def edgeTransformer(root: DotRootGraph)(innerEdge: scalax.collection.Graph[Vertex, WDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
    val edge = innerEdge.edge
    val label = edge.value.weight.toString
    Some((root,
      DotEdgeStmt(edge.from.toString,
        edge.to.toString,
        if (label.nonEmpty) List(DotAttr("label", label))
        else
          Nil)))
  }

  //, val g: Option[MyGraph], val a: Option[Seq[mxGeometry]]){

  // def display(s: String) {
  //   Dialog.showMessage(
  //     message = s,
  //     title = " ")
  // }

  // abstract case class Vertex(id: Int) {
  //   var value: Int = 1
  //   override def toString = "%d (%d)".format(id, value)
  // }

  // var valueField: TextField = null
  // implicit def int2Vertex(id: Int) = new Vertex(id) { value = valueField.text.toInt }

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
      val from = new TextField(5)
      val to = new TextField(5)
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
        val dot = g.toDot(root, f)
        graphComp.text = dot
      }

      val contents = new BorderPanel() {
        add(graphComp, BorderPanel.Position.Center)
        val value = new TextField(5)
        //        valueField = value
        value.text = "1"

        def l(s: String) = new Label(s)

        add(new BoxPanel(Orientation.Vertical) {
          contents += new FlowPanel(value, from, to, l("add"), addVertex, addEdge, new Label("edit"), editVertex, editEdge)
          contents += new FlowPanel(removeEdge, action, del, save, load)
        }, BorderPanel.Position.South)

        listenTo(addVertex, removeEdge, from, addEdge, action, del, graphComp.mouse.clicks, save, load, editVertex, editEdge)

        //     graphComp.peer.getGraphControl().addMouseListener(new MouseAdapter() {
        //       override def mouseClicked(evt: MouseEvent) {
        //         val x = evt.getX()
        //         val y = evt.getY()
        //         println("clicked " + evt.getModifiers())

        //         if ((evt.getModifiers() & InputEvent.CTRL_MASK) != 0) {
        //           println("adding")
        //           lastId += 1
        //           createPoint(lastId, x, y)
        //         }
        //       }
        //     });

        reactions += {

          case ButtonClicked(`addVertex`) =>
            println("adding")
            lastId += 1
            val v = Vertex(lastId, 1)
            g += v
            update()
          //       case ButtonClicked(`removeEdge`) =>
          //         disconnect(from.text.toInt, to.text.toInt)

          //       case ButtonClicked(`del`) =>
          //         val v = findV(from.text.toInt)
          //         v.foreach(graph.removeVertex(_))

          case ButtonClicked(`addEdge`) =>
            for {
              v1 <- findV(lastId);
              v2 <- findV(lastId - 1)
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

          //       case ButtonClicked(`action`) =>
          //         graph match {
          //           case d: DirectedGraph[_, _] =>
          //             val cd = new CycleDetector(d)
          //             val hasCycles = cd.detectCycles()
          //             display(if (hasCycles) "has cycles" else "do not has cycles")
          //           case ug: UndirectedGraph[_, _] =>
          //             val ci = new ConnectivityInspector(ug)
          //             val sets = ci.connectedSets()
          //             println("connectivity sets")
          //             display(if (sets.size() == 1) "graph connected" else "graph disconected")
          //         }

          //       case MouseClicked(`graphComp`, point, mod, c, t) =>
          //         val x = point.getX().intValue()
          //         val y = point.getY().intValue()
          //         println("clicked")

          //         if (mod == Key.Modifier.Control) {
          //           println("adding")
          //           lastId += 1
          //           createPoint(lastId, x, y)
          //         }

          //       case ButtonClicked(`save`) =>
          //         val fc = new FileChooser(new File("/tmp"))
          //         val geoms = adapter.getVertexToCellMap().values().map(adapter.getCellGeometry(_)).toSeq;
          //         fc.showSaveDialog(this)
          //         val file = fc.selectedFile;
          //         val dos = new ObjectOutputStream(new FileOutputStream(file))
          //         dos.writeObject(graph)
          //         dos.writeObject(geoms)
          //         dos.close()

          //       case ButtonClicked(`load`) =>
          //         val fc = new FileChooser(new File("/tmp"))
          //         fc.showOpenDialog(this)
          //         val file = fc.selectedFile;
          //         val dos = new ObjectInputStream(new FileInputStream(file))
          //         val g = dos.readObject().asInstanceOf[MyGraph]
          //         val geoms = dos.readObject().asInstanceOf[Seq[mxGeometry]]
          //         dos.close()
          //         val newView = createTopView(isDirected, Some(g), Some(geoms))
          //         Gapp.replace(isDirected, newView)

          //       case ButtonClicked(`addEdge`) =>
          //         connect(from.text.toInt, to.text.toInt)
        }

      }
      //   }
      contents
    }

    top

  }

}
