package dpl
import scala.collection.immutable.IntMap
import scala.collection.mutable.HashMap

class Interpreter(myNode : Node) {
  
  def interpret(program : IntMap[Instruction]) : (Stack, LocalVars) = {
    interpret(program, (new Stack, new LocalVars, 0))
  }
  
  def interpret(program : IntMap[Instruction], state : (Stack, LocalVars, Int)) : (Stack, LocalVars) = {
    Stats.instructionsExecuted += 1
    if (program.contains(state._3)) {
//      println(""+state+" -> "+state._3+":"+program(state._3))
    } else {
//      println("End")
    }
    val (stack, localVars, pc) = state;
    program.get(pc) match {
      case Some(i) => interpret(program, i.execute(myNode, stack, localVars, pc))
      case None => (stack, localVars)
    }
  }
}

class LineLabel(label : String) extends ProgramLine {
  var ln = -1
  
  def lineNo = {
    if (ln != -1) ln else throw new RuntimeException("Uninitialized label: "+this)
  }
    override def toString() : String = label+":"+lineNo
}

object LL {
  var llMap = HashMap[String, LineLabel]() 
  
  def apply(name : String) : LineLabel = {
    llMap.get(name) match {
      case Some(o) => o
      case None => {
        val newll = new LineLabel(name)
        llMap + ((name, newll))
        newll
      }
    }
  }
  

}

object Interpreter {
  def compile(input : List[ProgramLine]) : IntMap[Instruction] = {
    var line : Int = 0;
    var program : IntMap[Instruction] = IntMap()
    for (i <- input) {
      i match {
        case in : Instruction => {
          program = program.update(line, in)
          line += 1;
        }
        case ll : LineLabel => {
          ll.ln = line
        }
      }
    }
    program
  }
}