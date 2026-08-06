package ru.otus.module2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import ru.otus.module2.catsHomework.{Branch, Empty, Leaf, Tree, Functor2, given}

class FunctorTreeSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val functor = summon[Functor2[Tree]]
  private val emptyTree: Tree[Int] = Tree.empty
  private val singleLeaf: Tree[Int] = Tree.leaf(1)
  private val simpleTree: Tree[Int] = Tree(List(1, 2, 3, 4))
  //      Branch(1)
  // Leaf(2)    Branch(3)
  //          Empty     Leaf(4)

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

  it should "correctly transform tree" in {
    val result = functor.map(simpleTree)(_ * 2)
    result shouldBe Branch(2, Leaf(4), Branch(6, Empty, Leaf(8)))
  }

  it should "handle String type" in {
    val stringTree =
      Branch("a",
        Leaf("b"), Leaf("c"))
    val result = functor.map(stringTree)(_.toUpperCase)
    result shouldBe
      Branch("A",
        Leaf("B"), Leaf("C"))
  }

  it should "correctly handle complex transformations" in {
    val result = functor.map(simpleTree)(x => (x * 2).toString + "x")
    result shouldBe Branch("2x", Leaf("4x"), Branch("6x", Empty, Leaf("8x")))
  }

  // Тесты законов функтора

  "Functor laws on Functor[Tree]" should "satisfy identity law" in {
    // Закон идентичности: fa.map(a => a) <=> fa
    functor.map(simpleTree)(identity) shouldBe simpleTree
    functor.map(singleLeaf)(identity) shouldBe singleLeaf
    functor.map(emptyTree)(identity) shouldBe emptyTree
  }

  it should "satisfy composition law" in {
    // Закон композиции: fa.map(g(f(_))) <=> fa.map(f).map(g)
    //           fu.map(fa)(f andThen g) <=> fu.map(fu.map(fa)(f))(g)
    val f = (x: Int) => x * 2
    val g = (y: Int) => y + 1

    val leftSide = functor.map(simpleTree)(f andThen g)
    val mapF = functor.map(simpleTree)(f)
    val rightSide = functor.map(mapF)(g)

    leftSide shouldBe rightSide
  }

  it should "satisfy composition law (better syntax)" in {
    // Закон композиции: fa.map(g(f(_))) <=> fa.map(f).map(g)
    val f = (x: Int) => x * 2
    val g = (y: Int) => y + 1

    val leftSide = simpleTree.map(g compose f)
    val rightSide = simpleTree.map(f).map(g)

    leftSide shouldBe rightSide
  }

}
