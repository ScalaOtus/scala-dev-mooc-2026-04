package ru.otus.module2

import cats.Functor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import ru.otus.module2.catsHomework.{Branch, Leaf, Tree}
import ru.otus.module2.catsHomework.given

class FunctorTreeSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val functor = summon[Functor[Tree]]
  private val emptyTree: Tree[Int] = Tree.empty
  private val singleLeaf: Tree[Int] = Tree.leaf(1)
//  private val simpleTree: Tree[Int] =
//    Branch(
//      Leaf(1),
//      Branch(
//        Leaf(2),
//        Leaf(3)
//      )
//    )

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

/*
  it should "correctly transform tree" in {
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

  "map function" should "handle String type" in {
    val stringTree = Branch(
      Leaf("a"),
      Branch(
        Leaf("b"),
        Leaf("c")
      )
    )


    val result = functor.map(stringTree)(_.toUpperCase)
    result shouldBe
      Branch(
        Leaf("A"),
        Branch(
          Leaf("B"),
          Leaf("C")
        )
      )
  }

  it should "correctly handle complex transformations" in {
    val result = functor.map(simpleTree)(x => (x * 2).toString + "x")
    result shouldBe
      Branch(
        Leaf("2x"),
        Branch(
          Leaf("4x"),
          Leaf("6x")
        )
      )
  }

  // Тесты законов функтора

  "Functor laws" should "satisfy identity law" in {
    // Закон идентичности: fu.map(fa)(identity) == fa
    functor.map(simpleTree)(identity) shouldBe simpleTree
    functor.map(singleLeaf)(identity) shouldBe singleLeaf
    functor.map(emptyTree)(identity) shouldBe emptyTree
  }

  it should "satisfy composition law" in {
    // Закон композиции: fu.map(fu.map(fa)(f))(g) == fu.map(fa)(f andThen g)
    val f = (_: Int) * 2
    val g = (_: Int) + 1

    val leftSide = functor.map(functor.map(simpleTree)(f))(g)
    val rightSide = functor.map(simpleTree)(f andThen g)

    leftSide shouldBe rightSide
  }
*/

}
