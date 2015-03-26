import scala.collection.immutable.Stream._
import scalaz.Memo


/* Red/Black Balanced Binary Tree implemenation
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
  def apply[T](value: T)(implicit ord: Ordering[T]): RBN[T] = Black[T](value)
  def apply[T](values: Seq[T])(implicit ord: Ordering[T]): RBN[T] = Black[T](values.head) ++ values.tail
}
abstract class RBN[A](implicit ord: A => Ordered[A]) extends RBTree {
  def value: A
  def left: RBTree
  def right: RBTree
  protected def withLeft(node: RBN[A]): RBN[A]
  protected def withRight(node: RBN[A]): RBN[A]

  def +(data: A): RBN[A] = ins(data).asBlack
  def ++(values: Iterable[A]) = values.foldLeft(this)(_+_)
  def asBlack: RBN[A] = Black(value,left,right)

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

  private def visit[T](node: RBTree, default: T)(f: (RBN[A]) => T): T = node match {
    case t: RBN[A] => f(t)
    case Empty => default
  }

  def iterate(node: RBTree=this): Stream[A] = visit[Stream[A]](node,empty)(t =>
    iterate(t.left) #::: t.value #:: iterate(t.right))
  def before(node: RBTree=this): Stream[A] = visit[Stream[A]](node,empty)(t => iterate(t.left) #::: t.value #:: empty)
  def after(node: RBTree=this): Stream[A] = visit[Stream[A]](node,empty)(t => t.value #:: iterate(t.right))
  def size(node: RBTree=this): Int = visit[Int](node,0)(t => size(t.left) + size(t.right) + 1)
  def depth(node: RBTree=this): Int = visit[Int](node,0)(t => math.max(depth(t.left), depth(t.right)) + 1)
  def find(term: A, node: RBTree=this): Option[RBN[A]] = visit[Option[RBN[A]]](node,None) { t =>
    term compare t.value match {
      case 0 => Some(t)
      case d if d < 0 => find(term, t.left)
      case d if d > 0 => find(term, t.right)
    }
  }

  def show(node: RBTree=this, branch: String="", in: Int = 0): Unit = visit[Unit](node,Int) {
    case nn: RBN[A] =>
      lazy val spaces: Int => String = Memo.mutableHashMapMemo[Int, String]{n=>(1 to n).map(i=>" ").mkString}
      val color = if (nn.isInstanceOf[Black[A]]) 'B' else 'R'
      println(s"${spaces(in)}$branch$color(${nn.value})")
      show(nn.left, "+l: ", in + 2)
      show(nn.right, "+r: ", in + 2)
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
