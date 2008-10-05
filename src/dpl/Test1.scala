package dpl
import scala.collection.immutable._
import scala.actors.Actor
import scala.actors.Actor._

object Test1 {
  def main(args : Array[String]) : Unit = {
    val program : List[ProgramLine] = List(
      PushValue(0),
      PopToLocalVar(0), // LocalVar(0) is our counter
      LL("loop"),
      PushValue("v1"),
      Create(),
      Write("f1"),
      PushFromLocalVar(0),
      PushValue(1),
      AddOp,
      PopToLocalVar(0),
      PushFromLocalVar(0),
      PushValue(20),
      NotEqualsCond,
      If(LL("loop"))
    )
    val compiled = Interpreter.compile(program)
    println(compiled)
    val cluster = new Cluster(5, 10, compiled)
    for (n <- cluster.nodes) {
      n.start
    }
    cluster.nodes(0) ! ()
  }
}
