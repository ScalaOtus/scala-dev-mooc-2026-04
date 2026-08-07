package ru.otus.module2.catsHomework

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TreeSpec extends AnyFlatSpec with Matchers {

  import ru.otus.module2.catsHomework.{Branch, Empty, Leaf, Tree}

  private val emptyTree: Tree[Int] = Tree.empty
  private val singleLeaf: Tree[Int] = Tree.leaf(1)
  private val simpleTree: Tree[Int] = Tree(List(1, 2, 3, 4))

  "Empty tree" should "be empty" in {
    Tree.empty[Int] shouldBe an[Empty.type]
    Tree.empty[Int].isEmpty shouldBe true
  }

  "Leaf node" should "contain single value" in {
    val leaf = Tree.leaf(1)
    leaf shouldBe a[Leaf[_]]
    leaf.isEmpty shouldBe false
  }

  "Branch node" should "contain value and children" in {
    val branch = Tree.branch(1, Tree.leaf(2), Tree.leaf(3))
    branch match {
      case b: Branch[Int] =>
        b.value shouldBe 1
        b.left shouldBe a[Leaf[_]]
        b.right shouldBe a[Leaf[_]]
      case _ => fail("Expected a Branch")
    }
  }

  "Tree construction" should "handle single element" in {
    Tree(1) shouldBe Tree.leaf(1)
  }

  // Tree(List(1, 2, 3, 4)) is
  //      Branch(1)
  // Leaf(2)    Branch(3)
  //          Empty     Leaf(4)
  it should "handle list of elements correctly" in {
    simpleTree match {
      case Branch(1, Leaf(2), Branch(3, Empty, Leaf(4))) =>
      case _ => fail("Root structure is incorrect")
    }
  }

  "map function" should "transform values correctly" in {
    val tree = Tree(List(1, 2, 3))
    val mapped = tree.map(_ * 2)
    mapped shouldBe Tree(List(2, 4, 6))
  }

  "Empty checks" should "work correctly" in {
    Tree.empty[Int].isEmpty shouldBe true
    Tree.leaf(1).isEmpty shouldBe false
    Tree.branch(1, Tree.leaf(2), Tree.leaf(3)).isEmpty shouldBe false
  }

}
