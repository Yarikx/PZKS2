package org.yarik.pzks2

import scala.swing.Dimension
import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication

object Gapp extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Graphs"
    contents = UiHelper.createTopView(true);
    size = new Dimension(500, 400)

  }


}

