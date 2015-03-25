import org.scalatest.FunSuite

class RBTreeTest extends FunSuite {
  val rbt = new RedBlackTree[Int]

  import rbt._

  test("print tree") {
    //val tree = Seq(11,2,14,1,7,5,8,15,4).foldLeft(E: Tree)(_ insert _)
    val tree = (1 to 20).foldLeft(E: Tree)(_ insert _)

    tree.print()
    //tree match { case t: T => t.display()}
  }
}

class RedBlackTree[A](implicit ord: Ordering[A]) {
  sealed abstract class Color
  case object R extends Color
  case object B extends Color

  sealed abstract class Tree {
    def print(): Unit

    def insert(x: A): Tree = ins(x) match {
      case T(_, a, y, b) => T(B, a, y, b)
      case E => E
    }
    def ins(x: A): Tree
  }

  case object E extends Tree {
    override def ins(x: A): Tree = T(R, E, x, E)
    override def print(): Unit = println("<empty>")
  }

  case class T(color: Color, left: Tree, value: A, right: Tree) extends Tree {
    private def balance: Tree = {
      (color, left, value, right) match {
        case (B, T(R, T(R, a, x, b), y, c), z, d) => T(R, T(B, a, x, b), y, T(B, c, z, d))
        case (B, T(R, a, x, T(R, b, y, c)), z, d) => T(R, T(B, a, x, b), y, T(B, c, z, d))
        case (B, a, x, T(R, T(R, b, y, c), z, d)) => T(R, T(B, a, x, b), y, T(B, c, z, d))
        case (B, a, x, T(R, b, y, T(R, c, z, d))) =>T(R, T(B, a, x, b), y, T(B, c, z, d))
        case _ => this
      }
    }

    override def ins(data: A): Tree = ord.compare(data, value) match {
      case -1 => T(color, left ins data, value, right).balance
      case 1 => T(color, left, value, right ins data).balance
      case 0 => this
    }

    def size(node: Tree = this): Int = node match {
      case E => 0
      case t: T => size(t.left) + size(t.right) + 1
    }

    def depth(node: Tree = this): Int = node match {
      case E => 0
      case t: T => math.max(depth(t.left), depth(t.right)) + 1
    }

    def spaces(num: Int) = (for (_ <- 1 to num) yield " ").mkString

    def print(): Unit = {
      def printNode(n: Tree, branch: String = "", in: Int = 0): Unit = {
        n match {
          case E =>
          case node: T =>
            val color = node match {
              case t: T => t.color
              case _ => "?"
            }
            println(s"${spaces(in)}$branch$color(${node.value})")
            printNode(node.left, "+l: ", in + 2)
            printNode(node.right, "+r: ", in + 2)
        }
      }
      printNode(this)
    }

    def display() {
      val height = depth()
      val width = size()
      val len = width * height * 2 + 2
      val sb = new StringBuilder(len)
      (1 until len) foreach { i =>
        sb.append(if(i < len - 2 && i % width == 0) "\n" else ' ')
      }
      displayR(sb, width / 2, 1, width / 4, width, this, " ")
      println(sb)
    }
    def displayR(sb: StringBuilder, c: Int, r: Int, d: Int, w: Int, n: Tree, edge: String): Unit = n match {
      case t: T =>
        displayR(sb, c - d, r + 2, d / 2, w, t.left, " /")
        val s = t.value.toString
        val idx1 = r * w + c - (s.length + 1) / 2
        val idx2 = idx1 + s.length
        val idx3 = idx1 - w
        if (idx2 < sb.length) sb.replace(idx1, idx2, s).replace(idx3, idx3 + 2, edge)
        displayR(sb, c + d, r + 2, d / 2, w, t.right, "\\ ")
      case _ =>
    }
  }

}
