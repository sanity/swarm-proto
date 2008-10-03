package dpl

import scala.collection.mutable._
import scala.collection.immutable._

class Cluster(numNodes : Int, program : IntMap[Instruction]) {
  val maxImbalance = 4
  val nodes = new Array[Node](numNodes)
  
  for (i <- 1 to numNodes) {
    nodes(i-1) = new Node(this, program)
  }
  
  def loadBalance(thisNode : Node) : Option[Node] = {
    var smallest : Node = null
    var smallestSz = Integer.MAX_VALUE
    for (n <- nodes) {
      val nSz = n.store.size
      println(""+n+" : "+nSz)
      if (nSz < smallestSz) {
        smallest = n
        smallestSz = nSz
      }
    }
    
    val thisSz = thisNode.store.size
    
    val imbalance = thisSz - smallestSz
    
    if (imbalance > maxImbalance) {
      return Some(smallest)
    } else {
      return None
    }
  }
}
