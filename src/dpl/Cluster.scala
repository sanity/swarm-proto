package dpl

import scala.collection.mutable._
import scala.collection.immutable._

class Cluster(numNodes : Int, storeSize : Int, program : IntMap[Instruction]) {
  val maxImbalance = 0.5
  val nodes = new Array[Node](numNodes)
  
  for (i <- 1 to numNodes) {
    nodes(i-1) = new Node(this, storeSize, program)
  }
  
  def loadBalance(thisNode : Node) : Option[Node] = {
    var smallest : Node = null
    var smallestSz = Integer.MAX_VALUE
    for (n <- nodes) {
      val nSz = n.store.size
      if (nSz < smallestSz) {
        smallest = n
        smallestSz = nSz
      }
    }
    
    val thisSz = thisNode.store.size
    
    if ((thisSz - smallestSz) > (storeSize * maxImbalance)) {
      return Some(smallest)
    } else {
      return None
    }
  }
}
