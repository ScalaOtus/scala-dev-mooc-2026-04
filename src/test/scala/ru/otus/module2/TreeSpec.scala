package ru.otus.module2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.otus.module2.catsHomework.{Tree, Branch, Leaf}

class TreeSpec extends AnyFlatSpec with Matchers {

  private val emptyTree: Tree[Int] = Tree.empty
  private val singleLeaf: Tree[Int] = Tree.leaf(1)
  private val simpleTree: Tree[Int] =
    Branch(
      Leaf(1),
      Branch(
        Leaf(2),
        Leaf(3)
      )
    )

  "Empty Tree" should "be empty (emptyTree == true)" in {
    emptyTree.isEmpty shouldBe true
  }

  "Single leaf tree" should "not be empty (emptyTree == false)" in {
    singleLeaf.isEmpty shouldBe false
  }

  "map function" should "correctly transform values" in {
    val mappedTree = simpleTree.map(_ * 2)
    mappedTree shouldBe
      Branch(
        Leaf(2),
        Branch(
          Leaf(4),
          Leaf(6)
        )
      )
  }

  it should "work with identity function" in {
    val result = simpleTree.map(identity)
    result shouldBe simpleTree
  }

  "map on empty Tree" should "return empty Tree" in {
    val result = emptyTree.map(_ * 2)
    result shouldBe emptyTree
  }

  "map on single Leaf" should "transform single value" in {
    val result = singleLeaf.map(_ * 2)
    result shouldBe Tree.leaf(2)
  }

  it should "correctly handle different types" in {
    val stringTree = simpleTree.map(_.toString)
    stringTree shouldBe
      Branch(
        Leaf("1"),
        Branch(
          Leaf("2"),
          Leaf("3")
        )
      )
  }

  it should "compose functions correctly" in {
    val result = simpleTree.map(_ * 2).map(_ + 1)
    result shouldBe
      Branch(
        Leaf(3),
        Branch(
          Leaf(5),
          Leaf(7)
        )
      )
  }

  "Branch Tree" should "correctly represent structure" in {
    simpleTree shouldBe
      Branch(
        Leaf(1),
        Branch(
          Leaf(2),
          Leaf(3)
        )
      )
  }

  "Tree equality" should "work correctly for different structures" in {
    simpleTree should !==(singleLeaf)
    simpleTree should !==(emptyTree)
  }

}
