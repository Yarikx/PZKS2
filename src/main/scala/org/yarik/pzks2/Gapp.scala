package org.yarik.pzks2

import scala.swing.{ Dimension, MainFrame, SimpleSwingApplication, TabbedPane }
import scala.swing.TabbedPane.Page
import scala.swing.Component

object Gapp extends SimpleSwingApplication {

  val tp = new TabbedPane {
    pages += new Page("task", UiHelper.createTopView(true, None, None))
    pages += new Page("system", UiHelper.createTopView(false, None, None))
  }

  def top = new MainFrame {
    title = "Graphs"
    contents = tp
    size = new Dimension(700, 600)

  }

  def replace(first: Boolean, c: Component) = {
    println("replacing")
    val title = if(first) "task1" else "system1"
    tp.pages.update(if(first) 0 else 1, new Page(title, c))

  }

}

