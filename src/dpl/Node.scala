package dpl
import scala.collection.immutable.IntMap
import scala.actors.Actor
import scala.actors.Actor._

class Node(c : Cluster, storeSize : Int, program : IntMap[Instruction]) extends Actor {
  val id = Node.newId
  val interpreter : Interpreter = new Interpreter(this)
  val store : Store = new Store(storeSize)
  val cluster = c
  def act() = {
    loop {
      react {
        case () => this ! (new Stack, new LocalVars, 0)
        case pc : Int => this ! (new Stack, new LocalVars, pc)
        case state : (Stack, LocalVars, Int) => {
          try {
            interpreter.interpret(program, state)
          } catch {
            case RemoteReferenceException(remoteNode, continuation) => {
              Stats.moves += 1
//              println("Moving to "+remoteNode)
              remoteNode ! continuation
            }
          }
        }
      }
    }
  }
  
  def equals(eq : Node) = {
    eq.id == id
  }
  
  override def toString() = "Node("+id+")"
}

object Node {
  var nextId = 0;
  
  def newId : Int = {
    val id = nextId;
    nextId += 1;
    id
  }
}

object Stats {
  var instructionsExecuted = 0
  var moves = 0
  
  def clear = {
    instructionsExecuted = 0
    moves = 0
  }
}