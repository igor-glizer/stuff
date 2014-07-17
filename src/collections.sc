//traits ex, collections and iterators

trait Iterator[T] {
  def next: Option[T]
}

trait Iterable[T] {
  def iterator: Iterator[T]

  def headOption: Option[T] = iterator.next

  def contains(value: T): Boolean = {
    val it = iterator
    var item = it.next
    while (!item.isEmpty) {
      if (item.get.equals(value))
        return true
      else {
        item = it.next
      }
    }
    false
  }

  def size: Int = {
    val it = iterator
    var c = 0
    while (!it.next.isEmpty) {
      c += 1
    }
    c
  }


}


class ArrayList[T](array : Array[T]) extends Iterable[T] {


  def iterator = new Iterator[T] {
    var i = -1

    def next: Option[T] = {
      i += 1
      if (i < array.length)
        Some(array(i))
      else
        None
    }
  }


}


class LinkedList[T] extends Iterable[T] {

  trait Node

  case class Link(value: T, next: Node) extends Node

  case object Tail extends Node

  var head : Node = Tail

  def add(item: T) = {
    head = Link(item, head)
  }

  def iterator = new Iterator[T] {
    private var curr: Node = head

    def next: Option[T] = {
      curr match {
        case Link(value, next) => {
          curr = next; Some(value)
        }
        case Tail => None
      }
    }
  }

}

var l = new LinkedList[Int]
l.add(1)
l.add(2)
l.add(3)

var a =  Array(1,2,3)

val aa = new ArrayList[Int](a)


println("l = " + l.iterator.next.get)
println("l = " + aa.iterator.next.get)