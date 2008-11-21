package dpl
import scala.collection.mutable.HashSet

case class ObjectRef(location : Node, ref : Int) {
  ObjectRef.refs += this
  private var usageCount = 0
  def resetUsageCount = {
    usageCount = 0
  }
  def incrUsageCount = {
    usageCount += 1
  }
}

object ObjectRef {
  var refs = new HashSet[ObjectRef]
  
  def resetAll = {
    for (r <- refs) {
      r.resetUsageCount
    }
  }
  
}