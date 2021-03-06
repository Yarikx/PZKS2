package org.yarik.pzks2

import scala.swing.{ Component, Dimension, MainFrame, SimpleSwingApplication, TabbedPane }
import scala.swing.TabbedPane.Page
import scala.swing.BorderPanel

object Gapp extends SimpleSwingApplication {

  val sched = new GraphSched((comp, name) => {
    val page = new Page(name, comp)
//    if (tp.pages.size < 3) 
      tp.pages += page
//    else tp.pages.update(2, page)
//    tp.selection.index = 2
  })

  val tp = new TabbedPane {
    pages += new Page("task", TaskUi.content)
    pages += new Page("system", SystemUi.content)
  }

  def top = new MainFrame {
    title = "Graphs"
    contents = new BorderPanel() {
      add(tp, BorderPanel.Position.Center)
      add(sched.panel, BorderPanel.Position.South)
    }
    size = new Dimension(700, 600)
  }

  def replace(first: Boolean, c: Component) = {
    println("replacing")
    val title = if (first) "task" else "system"
    tp.pages.update(if (first) 0 else 1, new Page(title, c))

  }

}

