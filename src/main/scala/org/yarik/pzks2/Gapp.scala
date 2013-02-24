package org.yarik.pzks2

import scala.swing.{ Dimension, MainFrame, SimpleSwingApplication, TabbedPane }
import scala.swing.TabbedPane.Page

object Gapp extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Graphs"
    contents = new TabbedPane {
      pages += new Page("task", UiHelper.createTopView(true))
      pages += new Page("system", UiHelper.createTopView(false))
    }
    size = new Dimension(700, 600)

  }

}

