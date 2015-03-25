import scalaz.Memo

object trees {

  case class TreeNode[T](value: T,
                         left: Option[TreeNode[T]] = None,
                         right: Option[TreeNode[T]] = None)
                        (implicit comp: T => Ordered[T]) {

    private def seek[V](data: T)(f: (Option[TreeNode[T]], Boolean) => V): V = data compare value match {
      case 0 => f(None, false)
      case -1 => f(left, true)
      case 1 => f(right, false)
    }

    def insert(data: T): TreeNode[T] = seek(data) { (n, isLeft) =>
      lazy val node = Option(if (n.isEmpty) TreeNode(data) else n.get.insert(data))
      if (isLeft) copy(left = node) else copy(right = node)
    }

    def contains(term: T): Boolean = find(term).isDefined

    def find(term: T): Option[TreeNode[T]] = seek(term)((n, _) =>
      n.map(nn => if (term == nn.value) n else nn.find(term))).getOrElse(None)

    def min: TreeNode[T] = left.map(_.min).getOrElse(this)
    def max: TreeNode[T] = right.map(_.max).getOrElse(this)
  }

  implicit class TreePrinter[T](tree: TreeNode[T]) {
    private val spaces: Int => String = Memo.mutableHashMapMemo[Int, String] { num =>
      (for (_ <- 1 to num) yield " ").mkString
    }
    def print() = {
      def printNode(n: Option[TreeNode[T]], in: Int): Unit = n.foreach { node =>
        println(s"${spaces(in)} ${node.value}")
        printNode(node.left, in + 2)
        printNode(node.right, in + 2)
      }
      printNode(Some(tree), 0)
    }
  }

}
