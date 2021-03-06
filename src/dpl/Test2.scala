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
  val numNodes = 5
  val nodeSize = 50
  
  def main(args : Array[String]) : Unit = {
    val numObs = 200
    // for (numObs <- 1 to 100)     {
	    val program : List[ProgramLine] = List(
	      // Create a root object, add a "val" field and put 50 in it
	      Create(),
	      PopToLocalVar(0),
	      PushValue(50),
	      PushFromLocalVar(0),
	      Write("val"),
	      PushValue("null"),
	      PushFromLocalVar(0),
	      Write("left"),
	      PushValue("null"),
	      PushFromLocalVar(0),
	      Write("right"),
	      // Init counter
	      PushValue(0),
	      PopToLocalVar(4),
	      LL("OUTERLOOP"),
	      // Increment counter and test for completion
	      PushFromLocalVar(4),
	      PushValue(numObs),
	      EqualsCond,
	      If(LL("END")), // Terminate if we've reached completion
	      PushFromLocalVar(4), // Otherwise, increment counter
	      PushValue(1),
	      AddOp,
	      PopToLocalVar(4),
	      GenRandom(),
	      PopToLocalVar(2), // put the random in LV(2)
	      Create(),
	      PopToLocalVar(1),
	      PushFromLocalVar(2),
	      PushFromLocalVar(1),
	      Write("val"),
	      PushValue("null"),
	      PushFromLocalVar(1),
	      Write("left"),
	      PushValue("null"),
	      PushFromLocalVar(1),
	      Write("right"),
	      // Now we have our new node in LV(1), figure out where to add it
	      PushFromLocalVar(0),
	      PopToLocalVar(3),
	      LL("INNERLOOP"),
	      PushFromLocalVar(3), // Is the new number greater than the current node's value?
	      Read("val"),
	      PushFromLocalVar(2),
	      GreaterThanCond,
	      If(LL("GREATERTHAN")), // GREATER_THAN_LABEL
	      PushFromLocalVar(3), // We are going left
	      Read("left"),
	      PushValue("null"),
	      EqualsCond,
	      If(LL("ADDTOLEFT")), // ADD_TO_LEFT LABEL
	      PushFromLocalVar(3), // We are going left
	      Read("left"),
	      PopToLocalVar(3),
	      Goto(LL("INNERLOOP")), // INNER_LOOP LABEL
	      LL("ADDTOLEFT"),
	      PushFromLocalVar(1),
	      PushFromLocalVar(3),
	      Write("left"),
          PushFromLocalVar(1), // [*] This is a bit of a cheat, we
          Read("val"),         // move the continuation back
          Drop(),              // to the new node
	      Goto(LL("OUTERLOOP")), // Change to OUTER_LOOP LABEL
	      LL("GREATERTHAN"),
       PushFromLocalVar(3), // We are going right
	      Read("right"),
	      PushValue("null"),
	      EqualsCond,
	     If(LL("ADDTORIGHT")), // CHANGE TO ADD_TO_RIGHT LABEL
	      PushFromLocalVar(3), // We are going left
	      Read("right"),
	      PopToLocalVar(3),
	      Goto(LL("INNERLOOP")), // CHANGE TO INNER_LOOP LABEL
	      LL("ADDTORIGHT"),
	      PushFromLocalVar(1),
	      PushFromLocalVar(3),
	      Write("right"),
          PushFromLocalVar(1),  // Same cheat again (see [*]
          Read("val"),
          Drop(),
	      Goto(LL("OUTERLOOP")), // Change to OUTER_LOOP LABEL
          LL("END")
	    )
     
     val compiled = Interpreter.compile(program)
     
	    val cluster = new Cluster(numNodes, nodeSize, compiled)
	    for (n <- cluster.nodes) {
	      n.start
	    }
	    cluster.nodes(0) ! ()
	    
	    Thread.sleep(500)
	    /*
	   // println(""+numObs+", "+Stats.instructionsExecuted+", "+Stats.moves)
     println(""+numObs+", "+((Stats.moves:Float) / Stats.instructionsExecuted))
     */
     
//    }

     val nodeColorMap = Map(0 -> "pink", 1 -> "green", 2 -> "lightblue", 
         3 -> "yellow", 4 -> "orange")
     
     var nodeCount = 0
     // Dump to .dot format
     for {node <- cluster.nodes
          (objid, objmap) <- node.store.map} {
         println("o"+objid+" [label=\""+objmap("val")+"\", style=filled, color="+nodeColorMap(node.id)+"]")
         nodeCount += 1
     }
     
 //    println("Nodecount: "+nodeCount)
     
     for {node <- cluster.nodes
          (objid, objmap) <- node.store.map} {
       val Some(left) = objmap.get("left")
       val Some(right) = objmap.get("right")
       if (left.isInstanceOf[ObjectRef]) {
         println("o"+objid+" -> o"+left.asInstanceOf[ObjectRef].ref+";")
       }
       if (right.isInstanceOf[ObjectRef]) {
         println("o"+objid+" -> o"+right.asInstanceOf[ObjectRef].ref+";")
       }
       
  }
  
	

}
}	