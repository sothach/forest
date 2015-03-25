import oracle.net.aso.n
import org.scalatest.FunSuite

import scalaz.Alpha.{T, B, E}
import scalaz.Memo

class TreeTime extends FunSuite {

  test("RBN") {
    val tree = (1 to 20).foldLeft(Black[Int](1).asInstanceOf[RBN[Int]])(_+_)
    tree.print()
    assert(tree.size() === 20)
    assert(tree.depth() === 6)
  }

  test("word tree") {
    val tree = Black("Dublin") + "Cork" + "Limerick" + "Sligo" + "Belfast" +
      "Waterford" + "Wexford" + "Athlone" + "Edenderry" + "Finglas" + "Galway"
    tree.print()
  }
}

/*
    1. A node is either red or black.
    2. The root is black. (This rule is sometimes omitted. Since the root can always be changed
       from red to black, but not necessarily vice versa, this rule has little effect on analysis)
    3. All leaves (NIL) are black. (All leaves are same color as the root.)
    4. Every red node must have two black child nodes.
    5. Every path from a given node to any of its descendant NIL nodes contains the same number
       of black nodes.

  Pattern-match implementation, based on:
    https://wiki.rice.edu/confluence/download/attachments/2761212/Okasaki-Red-Black.pdf

  B:z <R:y <R:x(a,b)> <c> |         <d>          => R:y <B:x(a,b)> <B:z(c,d>
  B:z <R:x <a> <R:y(b,c)> |         <d>
  B:x         <a>         | <R:z <R:y(b,c)> <d>
  B:x         <a>         | <R:y <b> <R:z(c,d)>
*/
protected trait RBTree
object RBTree {
  //def apply[T](v: T): RBN[T] = Black[T](v)
  //def create[T](v: T) = Black[T](v).asInstanceOf[RBN[T]]
}
abstract class RBN[A](implicit ord: A => Ordered[A]) extends RBTree {
  def value: A
  def left: RBTree
  def right: RBTree
  protected def withLeft(node: RBN[A]): RBN[A]
  protected def withRight(node: RBN[A]): RBN[A]

  def +(data: A): RBN[A] = ins(data) match {
    case n: RBN[A] => Black (n.value, n.left, n.right)
  }

  def ins(data: A): RBN[A] = {
    def childWith(parent: RBTree, v: A): RBN[A] = parent match {
      case Empty => Red(v)
      case node: RBN[A] => node ins v
    }
    data compare value match {
      case d if d < 0 => withLeft ( childWith(left, data)).balance
      case d if d > 0 => withRight ( childWith(right, data)).balance
      case  0 => this
    }
  }

  def balance: RBN[A] = {
    def rotate(a: RBTree, b: RBTree, c: RBTree, d: RBTree, x: A, y: A, z: A) =
      Red[A](y, Black[A](x, a, b), Black[A](z, c, d))
    this match {
        case Black(z, Red(y: A, Red(x: A, a, b), c), d) => rotate(a, b, c, d, x, y, z)
        case Black(z, Red(x: A, a, Red(y: A, b, c)), d) => rotate(a, b, c, d, x, y, z)
        case Black(x, a, Red(z: A, Red(y: A, b, c), d)) => rotate(a, b, c, d, x, y, z)
        case Black(x, a, Red(y: A, b, Red(z: A, c, d))) => rotate(a, b, c, d, x, y, z)
        case _ => this
    }
  }

  def size(node: RBTree = this): Int = node match {
    case Empty => 0
    case t: RBN[A] => size(t.left) + size(t.right) + 1
  }

  def depth(node: RBTree = this): Int = node match {
    case Empty => 0
    case t: RBN[A] => math.max(depth(t.left), depth(t.right)) + 1
  }

  def print(): Unit = {
    val spaces: Int => String = Memo.mutableHashMapMemo[Int, String] { num =>
      (for (_ <- 1 to num) yield " ").mkString
    }
    def printNode(n: RBTree, branch: String="", in: Int = 0): Unit = n match {
      case node: RBN[A] =>
        val color = if (node.isInstanceOf[Black[A]]) 'B' else 'R'
        println(s"${spaces(in)}$branch$color(${node.value})")
        printNode(node.left, "+l: ", in + 2)
        printNode(node.right, "+r: ", in + 2)
      case _ =>
    }
    printNode(this)
  }
}

private object Empty extends RBTree
sealed private case class Black[A](
  value: A, left: RBTree=Empty, right:  RBTree=Empty)(implicit ord: A => Ordered[A]) extends RBN[A] {
  def withLeft(node: RBN[A]) = copy(left=node)
  def withRight(node: RBN[A]) = copy(right=node)
}
sealed private case class Red[A](
  value: A, left: RBTree=Empty, right:  RBTree=Empty)(implicit ord: A => Ordered[A]) extends RBN[A]{
  def withLeft(node: RBN[A]) = copy(left=node)
  def withRight(node: RBN[A]) = copy(right=node)
}
