package dpl
import scala.actors.Future

class Stack(list : List[Any]) {
  
  def this() = this(List())
  
  def push(v : Any) : Stack = {
    new Stack(v :: list)
  }
  
  def pushAll(l : List[Any]) = {
    new Stack(l ::: list)
  }
  
  def pop : (Any, Stack) = {
    val p :: rest = list
    p match {
      case pc : Future[_] => (pc(), new Stack(rest))
      case pc => (pc, new Stack(rest))
	}
  }
  
  def pop2 : (Any, Any, Stack) = {
    val (p1, s1) = pop
    val (p2, s2) = s1.pop
    (p1, p2, s2)
  }
  
  def pop3 : (Any, Any, Any, Stack) = {
    val (p1, p2, s1) = pop2
    val (p3, s2) = s1.pop
    (p1, p2, p3, s2)
    }

  override def toString() = list.toString()
}
