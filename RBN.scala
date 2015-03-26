package org.nulleins.trees

import java.io.PrintStream

import oracle.net.aso.{r, l}

import scala.collection.immutable.Stream._
import scalaz.Memo

/* Red/Black Balanced Binary Tree implementation
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
protected trait RBNode
object RedBlackTree {
  def apply[T](values: T*)(implicit ord: Ordering[T]): RedBlackTree[T] = Black[T](values.head) ++ values.tail
  def unapply[T](node: RedBlackTree[T]) = Some(node.value, node.left, node.right)

  implicit class treePrinter[_](val tree: RedBlackTree[_]) {
    def show(output: PrintStream=Console.out): Unit = {
      def showNode(node: RBNode, branch: String = "", in: Int = 0): Unit = node match {
        case RedBlackTree(v,l,r) =>
          lazy val spaces: Int => String = Memo.mutableHashMapMemo[Int, String] { n => (1 to n).map(i => " ").mkString }
          val color = if (node.isInstanceOf[Black[_]]) 'B' else 'R'
          output.println(s"${spaces(in)}$branch$color($v)")
          showNode(l, "+l: ", in + 2)
          showNode(r, "+r: ", in + 2)
        case Empty =>
      }
      showNode(tree)
    }
  }
}
sealed abstract class RedBlackTree[A](implicit ord: A => Ordered[A]) extends RBNode {
  def value: A
  def left: RBNode
  def right: RBNode
  protected[RedBlackTree] def withLeft(node: RBNode): RedBlackTree[A]
  protected[RedBlackTree] def withRight(node: RBNode): RedBlackTree[A]

  def +(data: A): RedBlackTree[A] = insert(data) match {
    case RedBlackTree(v,l,r) => Black(v,l,r)
  }
  def ++(values: Iterable[A]) = values.foldLeft(this)(_+_)
  def ::(other: RedBlackTree[A]) = this ++ other.iterate().toList

  private def insert(data: A): RedBlackTree[A] = {
    def childWith(parent: RBNode, v: A): RedBlackTree[A] = parent match {
      case Empty => Red(v)
      case node: RedBlackTree[A] => node insert v
    }
    data compare value match {
      case d if d < 0 => withLeft ( childWith(left, data)).balance
      case d if d > 0 => withRight ( childWith(right, data)).balance
      case  0 => this
    }
  }

  private def balance: RedBlackTree[A] = {
    def rotate(a: RBNode, b: RBNode, c: RBNode, d: RBNode, x: A, y: A, z: A) =
      Red(y, Black(x, a, b), Black(z, c, d))
    this match {
      case Black(z, Red(y: A, Red(x: A, a, b), c), d) => rotate(a, b, c, d, x, y, z)
      case Black(z, Red(x: A, a, Red(y: A, b, c)), d) => rotate(a, b, c, d, x, y, z)
      case Black(x, a, Red(z: A, Red(y: A, b, c), d)) => rotate(a, b, c, d, x, y, z)
      case Black(x, a, Red(y: A, b, Red(z: A, c, d))) => rotate(a, b, c, d, x, y, z)
      case _ => this
    }
  }

  private def visit[T](node: RBNode, default: T)(f: (RedBlackTree[A]) => T): T = node match {
    case t: RedBlackTree[A] => f(t)
    case Empty => default
  }

  def iterate(node: RBNode=this): Stream[A] = visit[Stream[A]](node,empty)(t =>
    iterate(t.left) #::: t.value #:: iterate(t.right))
  def to(node: RBNode=this): Stream[A] = visit[Stream[A]](node,empty)(t => iterate(t.left) #::: t.value #:: empty)
  def from(node: RBNode=this): Stream[A] = visit[Stream[A]](node,empty)(t => t.value #:: iterate(t.right))
  def size(node: RBNode=this): Int = visit[Int](node,0)(t => size(t.left) + size(t.right) + 1)
  def depth(node: RBNode=this): Int = visit[Int](node,0)(t => math.max(depth(t.left), depth(t.right)) + 1)
  def find(term: A, node: RBNode=this): Option[RedBlackTree[A]] = visit[Option[RedBlackTree[A]]](node,None) { t =>
    term compare t.value match {
      case 0 => Some(t)
      case d if d < 0 => find(term, t.left)
      case d if d > 0 => find(term, t.right)
    }
  }
}

case object Empty extends RBNode {
  def ::[T](v: T*)(implicit ord: Ordering[T]) = RedBlackTree(v.head) ++ v.tail
}
sealed private case class Black[A](value: A, left: RBNode=Empty, right:  RBNode=Empty)
                                  (implicit ord: A => Ordered[A]) extends RedBlackTree[A] {
  def withLeft(node: RBNode) = copy(left=node)
  def withRight(node: RBNode) = copy(right=node)
}
sealed private case class Red[A](value: A, left: RBNode=Empty, right:  RBNode=Empty)
                                (implicit ord: A => Ordered[A]) extends RedBlackTree[A]{
  def withLeft(node: RBNode) = copy(left=node)
  def withRight(node: RBNode) = copy(right=node)
}
