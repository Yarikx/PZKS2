package org.jgrapht.graph

class MyEdge extends DefaultWeightedEdge() {
  override def toString(): String = {
    return this.weight.toString
  }
}

