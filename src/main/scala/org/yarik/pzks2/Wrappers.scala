package org.yarik.pzks2

import UiHelper._
import com.mxgraph.swing.mxGraphComponent
import scala.collection.JavaConversions._
import scala.swing.Component
import javax.swing.JButton

class SGraph[V,E](graph: JGraphXAdapter[V,E]) extends Component{
  override lazy val peer =  new mxGraphComponent(graph);
  //override lazy val peer =  new JButton("test");
}
