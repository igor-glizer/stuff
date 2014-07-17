import scala.collection.mutable

//BFS and DFS

case class Node (
                  value : Int,
                  left : Option[Node],
                  right : Option[Node]
                  )

def printDFS(node : Node) : Unit =
{
  println(node.value)
  if (!node.left.isEmpty)
    printDFS(node.left.get)

  node.left.foreach { l => printDFS(l) }

  if (!node.right.isEmpty)
    printDFS(node.right.get)
}

def printBFS(root : Node) : Unit =
{
  val queue : mutable.Queue[Node] = new mutable.Queue[Node]()

  queue.enqueue(root)

  while (!queue.isEmpty)
  {
    val node = queue.dequeue
    println(node.value)
    if (!node.left.isEmpty)
      queue.enqueue(node.left.get)
    if (!node.right.isEmpty)
      queue.enqueue(node.right.get)
  }
}
val ll = Node(3, None, None)
val lr = Node(4, None, None)
val l = Node(2, Some(ll), Some(lr))
val rr = Node(6, None, None)
val r = Node(5, None, Some(rr))
val root = Node(1, Some(l), Some(r))
printDFS(root)
printBFS(root)