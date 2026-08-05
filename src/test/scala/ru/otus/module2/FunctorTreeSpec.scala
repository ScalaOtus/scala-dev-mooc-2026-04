package ru.otus.module2

import cats.Functor
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.otus.module2.catsHomework.{Branch, Leaf, Tree}

class FunctorTreeSpec(using functor: Functor[Tree]) extends AnyFlatSpec with Matchers with OptionValues {

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

  "Functor[Tree] instance" should "be the same instance as TreeFunctor" in {
    functor shouldBe ru.otus.module2.catsHomework.TreeFunctor
  }

  "Functor[Tree] instance" should "exist and work correctly" in {
    functor shouldNot be(null)
  }

  "Functor[Tree] map function" should "work correctly with empty tree" in {
    val result = functor.map(emptyTree)(_ * 2)
    result shouldBe emptyTree
  }

  it should "correctly transform single leaf" in {
    val result = functor.map(singleLeaf)(_ * 2)
    result shouldBe Tree.leaf(2)
  }

  it should "correctly transform complex tree" in {
    val result = functor.map(simpleTree)(_ * 2)
    result shouldBe
      Branch(
        Leaf(2),
        Branch(
          Leaf(4),
          Leaf(6)
        )
      )
  }

}
