import scala.collection.immutable.Queue

//BFS and DFS

case class Node (
                  value : Int,
                  left : Option[Node],
                  right : Option[Node]
                  )

def traverseDFS(node : Node, action : Node => Unit) : Unit =
{
  action(node)

  node.left.foreach { l => traverseDFS(l, action) }

  node.right.foreach { l => traverseDFS(l, action) }
}

def traverseBFS(root : Node, action : Node => Unit) : Unit =
{
  traverseBFSAux(Seq(root), action)
}

def traverseBFSAux(nodesToVisit : Seq[Node], action : Node => Unit) : Unit =
{
  if (!nodesToVisit.isEmpty)
  {
    val node = nodesToVisit.head
    action(node)

    val nextNodes = nodesToVisit.tail ++
      node.left.map(n => n).seq ++
      node.right.map(n => n).seq
    traverseBFSAux(nextNodes, action)
  }
}

val ll = Node(3, None, None)
val lr = Node(4, None, None)
val l = Node(2, Some(ll), Some(lr))
val rr = Node(6, None, None)
val r = Node(5, None, Some(rr))
val root = Node(1, Some(l), Some(r))
traverseDFS(root, n => println(n.value))
traverseBFS(root, n => println(n.value))