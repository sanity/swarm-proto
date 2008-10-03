package dpl
import scala.collection.immutable._
import scala.actors.Actor
import scala.actors.Actor._

object Test1 {
  def main(args : Array[String]) : Unit = {
    val program : IntMap[Instruction] = IntMap(
      0 -> PushValue(0),
      1 -> PopToLocalVar(0), // LocalVar(0) is our counter
      2 -> PushValue("v1"),
      3 -> Create(),
      4 -> Write("f1"),
      5 -> PushFromLocalVar(0),
      6 -> PushValue(1),
      7 -> AddOp,
      8 -> PopToLocalVar(0),
      9 -> PushFromLocalVar(0),
      10 -> PushValue(20),
      11 -> NotEqualsCond,
      12 -> If(2)
    )
    val cluster = new Cluster(5, program)
    for (n <- cluster.nodes) {
      n.start
    }
    cluster.nodes(0) ! ()
  }
}
