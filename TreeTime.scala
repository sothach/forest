import oracle.net.aso.{n, h}
import org.nulleins.trees.{Empty, RedBlackTree}
import org.scalatest._

import scala.math._

class TreeTime extends FlatSpec with Matchers {

  "A RB Tree" should "have the correct size" in {
    val tree = (1 to 20).foldLeft(RedBlackTree(1))(_ + _)
    tree.show()
    tree.size() shouldBe 20
    tree.depth() shouldBe 6
  }

  "A word tree" should "be sorted correctly" in {
    val tree = RedBlackTree("Dublin", "Cork", "Limerick", "Sligo", "Belfast",
      "Waterford", "Wexford", "Athlone", "Edenderry", "Finglas", "Galway")
    tree.iterator.toStream shouldBe sorted
    tree.toSet should contain ("Dublin")
    tree.show()
    tree.size() shouldBe 11

    tree.find("New Ross") shouldBe empty
    tree.find("Wexford") should not be empty

    val list = tree.find("Edenderry") match {
      case None => Nil
      case Some(root) => root.to.toList map identity
    }
    list.size shouldBe 5
  }

  "Tree builders" should "correctly concatenate trees" in {
    val tree = "aaa" :: "efg" :: "hij" :: "bcd" :: Empty
    val tree2 = tree :: RedBlackTree("klm", "nop", "qrs", "tuv", "wxy", "zzz")
    tree2.toSet should contain ("bcd")
    tree2.toSet should contain ("klm")
  }

  implicit def setConvert[A](tree: RedBlackTree[A]): Set[A] = tree.iterator.toSet

  def height(elements: Int): Int = {
    def modp2(size: Int): Int = {
        val h = List(1, 2, 4, 8, 16).foldLeft(size - 1)((v, e) => v | (v >> e)) + 1
        if (h == size) modp2(h + 1) else h
    }
    if(elements <= 0) 0
    else if(elements == 1) 1
    else ceil(log(modp2(elements)) / log(2)).toInt
  }

  /*
  1:                         1
  2:                       2   3
  3:                    4  5   6  7
  4:               8 9 10 11  12 13 14 15
  5: 16 17 18 19 20 21 22 23  24 25 26 27 28 29 30 31
                              8
                      4                  12
                  2      6         10          16
                1  3   5   7     9   11    14       18
                                         13  15   17  19
                                                         20
  */
  "Tree metrics" should "be calculated (e.g, O(logn)?" in {
    val res = (1 to 33) map ( n => (n,height(n)))
    println(res)
  }

}

