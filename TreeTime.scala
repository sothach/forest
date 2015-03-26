import org.nulleins.trees.{RedBlackTree, RBNode}
import org.scalatest.FunSuite

class TreeTime extends FunSuite {
  test("red-black tree") {
    val tree = (1 to 20).foldLeft(RedBlackTree(1))(_+_)
    tree.show()
    assert(tree.size() === 20)
    assert(tree.depth() === 6)
  }

  test("word tree") {
    val tree = RedBlackTree("Dublin", "Cork", "Limerick", "Sligo", "Belfast",
                "Waterford", "Wexford", "Athlone", "Edenderry", "Finglas", "Galway")
    tree.show(Console.err)
    tree.iterate() foreach println
    assert(tree.size() === 11)

    assert(tree.find("New Ross").isEmpty)
    assert(tree.find("Wexford").isDefined)

    val list = tree.find("Edenderry") match {
      case None => ""
      case Some(root) => root.before().toList map identity
    }
    println(list)
  }

  test("building") {
    val item1 = RedBlackTree("aaa", "bcd")
    val tree = item1 + "efg" + "hij"
    val tree2 = tree :: RedBlackTree("klm", "nop", "qrs", "tuv", "wxy", "zzz")
    tree2.show()
  }

  /*  test("O(logn)?") {
      def log2(n: Int) = {
        require(n > 0)
        math.log(n) / math.log(2)
      }
      val n = 20
      println(s"n=$n logn=${math.log(n)}")
      println(s"2^h (h=$n) = ${math.pow(2,n)}")
      println(s"log2(n+1)) = ${log2(n+1)}")
    }*/
}
