package dpl

import scala.collection.immutable._

/*
 * LocalVar(0) contains the root
 * LocalVar(1) contains the new node
 * LocalVar(2) contains the value of the new node
 * LocalVar(3) contains the current node
 * LocalVar(4) contains counter
 */

object Test2 {
  val numObs = 10;
  
  def main(args : Array[String]) : Unit = {
    val x : Int = 1;
    val program : IntMap[Instruction] = IntMap(
      // Create a root object, add a "val" field and put 50 in it
      0 -> Create(),
      1 -> PopToLocalVar(0),
      2 -> PushValue(50),
      3 -> PushFromLocalVar(0),
      4 -> Write("val"),
      5 -> PushValue("null"),
      6 -> PushFromLocalVar(0),
      7 -> Write("left"),
      8 -> PushValue("null"),
      9 -> PushFromLocalVar(0),
      10 -> Write("right"),
      // Init counter
      11 -> PushValue(0),
      12 -> PopToLocalVar(4),
      ///// OUTER_LOOP_LABEL
      // Increment counter and test for completion
      13 -> PushFromLocalVar(4),
      14 -> PushValue(numObs),
      15 -> EqualsCond,
      16 -> If(-1), // Terminate if we've reached completion
      17 -> PushFromLocalVar(4), // Otherwise, increment counter
      18 -> PushValue(1),
      19 -> AddOp,
      20 -> PopToLocalVar(4),
      21 -> GenRandom(),
      22 -> PopToLocalVar(2), // put the random in LV(2)
      23 -> Create(),
      24 -> PopToLocalVar(1),
      25 -> PushFromLocalVar(2),
      26 -> PushFromLocalVar(1),
      27 -> Write("val"),
      28 -> PushValue("null"),
      29 -> PushFromLocalVar(1),
      30 -> Write("left"),
      31 -> PushValue("null"),
      32 -> PushFromLocalVar(1),
      33 -> Write("right"),
      // Now we have our new node in LV(1), figure out where to add it
      34 -> PushFromLocalVar(0),
      35 -> PopToLocalVar(3),
      // INNER_LOOP_LABEL
      36 -> PushFromLocalVar(3), // Is the new number greater than the current node's value?
      37 -> Read("val"),
      38 -> PushFromLocalVar(2),
      39 -> GreaterThanCond,
      40 -> If(54), // GREATER_THAN_LABEL
      41 -> PushFromLocalVar(3), // We are going left
      42 -> Read("left"),
      43 -> PushValue("null"),
      44 -> EqualsCond,
      45 -> If(50), // ADD_TO_LEFT LABEL
      46 -> PushFromLocalVar(3), // We are going left
      47 -> Read("left"),
      48 -> PopToLocalVar(3),
      49 -> Goto(36), // INNER_LOOP LABEL
      ////// ADD_TO_LEFT LABEL
      50 -> PushFromLocalVar(1),
      51 -> PushFromLocalVar(3),
      52 -> Write("left"),
      53 -> Goto(13), // Change to OUTER_LOOP LABEL
      ////// GREATER_THAN_LABEL
     54 -> PushFromLocalVar(3), // We are going right
      55 -> Read("right"),
      56 -> PushValue("null"),
      57 -> EqualsCond,
      58 -> If(63), // CHANGE TO ADD_TO_RIGHT LABEL
      59 -> PushFromLocalVar(3), // We are going left
      60 -> Read("right"),
      61 -> PopToLocalVar(3),
      62 -> Goto(36), // CHANGE TO INNER_LOOP LABEL
      ////// ADD_TO_RIGHT LABEL
      63 -> PushFromLocalVar(1),
      64 -> PushFromLocalVar(3),
      65 -> Write("right"),
      66 -> Goto(13) // Change to OUTER_LOOP LABEL
    )
    val cluster = new Cluster(5, program)
    for (n <- cluster.nodes) {
      n.start
    }
    cluster.nodes(0) ! ()
    
    Thread.sleep(2000)
    
    for (node <- cluster.nodes) {
      println(node.id+" "+node.store.map)
      println()
    }

}
}