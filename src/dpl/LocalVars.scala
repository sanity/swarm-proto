package dpl
import scala.collection.immutable._
import scala.actors.Future

class LocalVars(m : Map[Int, Any]) {
  def this() = {
    this(new HashMap())
  }
  
  def apply(i : Int) : Any = {
    m(i) match {
      case v : Future[_] => v()
      case v : Any => v
    }
  }
  
  def update(i : Int, v : Any) : LocalVars = {
    new LocalVars(m(i) = v)
  }

  override def toString() = m.toString()
}
