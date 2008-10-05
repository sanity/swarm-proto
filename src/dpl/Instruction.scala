package dpl
import scala.collection.immutable.IntMap
import scala.List
import scala.collection.immutable.EmptySet
import scala.collection.immutable.Set1

abstract class Instruction extends ProgramLine {
  
  def execute(node : Node, stack : Stack, localVars : LocalVars, pc : Int) : (Stack, LocalVars, Int)
  
  val depends : (Int, Int, Set[Int], Set[Int])
}

case class BiOp[A](op : (A, A) => A) extends Instruction {
  def execute(node : Node, stack : Stack, localVars : LocalVars, pc : Int) : (Stack, LocalVars, Int) = {
    val (v1, v2, ns) = stack.pop2
    (v1, v2) match {
      case (v1 : A, v2 : A) => (ns.push(op(v1, v2)), localVars, pc+1)
      case _ =>  throw new RuntimeException
    }
  }
  
  val depends = (2, 1, new EmptySet[Int], new EmptySet[Int])
} 

object AddOp extends BiOp((a : Int, b : Int) => a+b)
object SubtractOp extends BiOp((a : Int, b : Int) => a-b)
object MultiplyOp extends BiOp((a : Int, b : Int) => a*b)
object DivideOp extends BiOp((a : Int, b : Int) => a/b)

case class Cond[A, B](op : (A, B) => Boolean) extends Instruction {
  def execute(node : Node, stack : Stack, localVars : LocalVars, pc : Int) : (Stack, LocalVars, Int) = {
    val (v1, v2, ns) = stack.pop2
    (v1, v2) match {
      case (v1 : A, v2 : B) => (ns.push(op(v1, v2)), localVars, pc+1)
      case _ =>  throw new RuntimeException("v1="+v1+", v2="+v2)
    }
  }
  
  val depends = (2, 1, new EmptySet[Int], new EmptySet[Int])
}

object EqualsCond extends Cond((a : Any, b : Any) => a == b)
object NotEqualsCond extends Cond((a : Any, b : Any) => a != b)
object GreaterThanCond extends Cond((a : Any, b : Any) => {
  (a,b) match {
    case (ai : Int, bi : Int) => ai > bi
  }
})

case class PushValue(value : Any) extends Instruction {
  def execute(node : Node, stack : Stack, localVars : LocalVars, pc : Int) : (Stack, LocalVars, Int) = {
     (stack.push(value), localVars, pc+1)
   }
   
   val depends = (0, 1, new EmptySet[Int], new EmptySet[Int])
}

case class Dupe() extends Instruction {
     def execute(node : Node, stack : Stack, localVars : LocalVars, pc : Int) : (Stack, LocalVars, Int) = {
     val (v, ns) = stack.pop
     val ns2 = stack.pushAll(List(v, v))
     (ns2, localVars, pc+1)
   }
   
   val depends = (1, 2, new EmptySet[Int], new EmptySet[Int]())
}

case class PopToLocalVar(varPos : Int) extends Instruction {
   def execute(node : Node, stack : Stack, localVars : LocalVars, pc : Int) : (Stack, LocalVars, Int) = {
     val (v, ns) = stack.pop
     (ns, localVars(varPos) = v, pc+1)
   }
   
   val depends = (1, 0, new EmptySet[Int], new Set1[Int](varPos))
}

case class PushFromLocalVar(varPos : Int) extends Instruction {
   def execute(node : Node, stack : Stack, localVars : LocalVars, pc : Int) : (Stack, LocalVars, Int) = {
       (stack.push(localVars(varPos)), localVars, pc+1)
   }
   
   val depends = (0, 1, new Set1[Int](varPos), new EmptySet[Int])
}

case class Goto(moveTo : LineLabel) extends Instruction {
   def execute(node : Node, stack : Stack, localVars : LocalVars, pc : Int) : (Stack, LocalVars, Int) = {
      (stack, localVars, moveTo.lineNo)
  }
  
  val depends = (0, 0, new EmptySet[Int], new EmptySet[Int])
}

case class If(moveTo : LineLabel) extends Instruction {
   def execute(node : Node, stack : Stack, localVars : LocalVars, pc : Int) : (Stack, LocalVars, Int) = {
     var (v, ns) = stack.pop
      v match {
        case true => (ns, localVars, moveTo.lineNo)
        case false => (ns, localVars, pc + 1)
        case _ => throw new RuntimeException
      }
    }
  
    val depends = (1, 0, new EmptySet[Int], new EmptySet[Int])
}

case class Create() extends Instruction {
    def execute(node : Node, stack : Stack, localVars : LocalVars, pc : Int) : (Stack, LocalVars, Int) = {
      node.cluster.loadBalance(node) match {
        case None => {
          node.store.create match {
            case Some(id) => (stack.push(new ObjectRef(node, id)), localVars, pc+1)
            // TODO: Deal with full store
          }
          
        }
        case Some(remoteNode : Node) => {
          throw new RemoteReferenceException(remoteNode, (stack, localVars, pc))
        }
      }
    }
    
    val depends = (0, 1, new EmptySet[Int], new EmptySet[Int])
}

case class Nop() extends Instruction {
    def execute(node : Node, stack : Stack, localVars : LocalVars, pc : Int) : (Stack, LocalVars, Int) = {
      (stack, localVars, pc+1)
  }
  
  val depends = (0, 0, new EmptySet[Int], new EmptySet[Int])
}

case class Read(field : String) extends Instruction {
  def execute(node : Node, stack : Stack, localVars : LocalVars, pc : Int) : (Stack, LocalVars, Int) = {
    val (v, ns) = stack.pop
    v match {
      case or : ObjectRef if or.location == node => (ns.push(node.store(or.ref, field)), localVars, pc+1)
      case or : ObjectRef if or.location != node => throw new RemoteReferenceException(or.location, (stack, localVars, pc))
      case _ => throw new RuntimeException
    }
  }
  
  val depends = (1, 1, new EmptySet[Int], new EmptySet[Int])
}

case class Write(field : String) extends Instruction {
   def execute(node : Node, stack : Stack, localVars : LocalVars, pc : Int) : (Stack, LocalVars, Int) = {
    val (p1, p2, ns) = stack.pop2
    p1 match {
      case or : ObjectRef if or.location == node => {
        node.store(or.ref, field) = p2
        (ns, localVars, pc+1)
      }
      case or : ObjectRef if or.location != node => throw new RemoteReferenceException(or.location, (stack, localVars, pc))
      case o => throw new RuntimeException("Expected ObjectRef, got an "+o)
    }
   }
   
   val depends = (2, 0, new EmptySet[Int], new EmptySet[Int])
}

case class GenRandom() extends Instruction {
  def execute(node : Node, stack : Stack, localVars : LocalVars, pc : Int) : (Stack, LocalVars, Int) = {
    (stack.push(Util.random.nextInt(100)), localVars, pc+1)
  }
   
   val depends = (0, 1, new EmptySet[Int], new EmptySet[Int])
}

case class RemoteReferenceException(location : Node, state : (Stack, LocalVars, Int)) extends Exception
