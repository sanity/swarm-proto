package dpl
import scala.collection.immutable.IntMap

class Interpreter(myNode : Node) {
  def interpret(program : IntMap[Instruction]) : (Stack, LocalVars) = {
    interpret(program, (new Stack, new LocalVars, 0))
  }
  
  def interpret(program : IntMap[Instruction], state : (Stack, LocalVars, Int)) : (Stack, LocalVars) = {
    if (program.contains(state._3)) {
      println(""+state+" -> "+state._3+":"+program(state._3))
    } else {
      println("End")
    }
    val (stack, localVars, pc) = state;
    program.get(pc) match {
      case Some(i) => interpret(program, i.execute(myNode, stack, localVars, pc))
      case None => (stack, localVars)
    }
  }
}
