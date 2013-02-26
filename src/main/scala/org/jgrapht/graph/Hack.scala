package org.jgrapht.graph

class MyEdge extends DefaultWeightedEdge() {
  
  def w_= (value:Double):Unit = weight = value
  def w = weight
  
  override def toString(): String = {
    return this.weight.toString
  }
}

