package dpl
import scala.collection.mutable._
import Util._

class Store(maxSize : Int) {
  def this() = this(10)
  
  val map : Map[Int, Map[String,Any]] = new HashMap()
    
  def apply(objectPtr : Int, f : String) : Any = {
    map.get(objectPtr) match {
      case Some(o) => {
        o.get(f) match {
          case Some(vl) => vl
        }
      }
      case None => None
    }
  }

  def update(objectPtr : Int, f : String, v : Any) : Boolean = {
    map.get(objectPtr) match {
      case Some(obj) => {
        obj(f) = v
        true
      }
      case None => false
    }
  }
  
  def create() : Option[Int] = {
    if (map.size <= maxSize) {
      var nref : Int = Store.getNextId
      map(nref) = Map()
      return Some(nref)
    } else {
      return None
    }
  }
  
  def delete(objectPtr : Int) : Unit = {
    map-objectPtr
  }
  
  def delete(objectPtr : Int, f : String) : Boolean = {
    map.get(objectPtr) match {
      case Some(o) => {
        o-f
        return true
      }
      case None => false
    }
  }
  
  def size = map.size
}

object Store {
  var nextId = 0;
  
  def getNextId : Int = {
    val ni = nextId
    nextId += 1
    ni
  }
}