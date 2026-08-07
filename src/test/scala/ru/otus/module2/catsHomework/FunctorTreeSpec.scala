package ru.otus.module2.catsHomework

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FunctorTreeSpec extends AnyFlatSpec with Matchers {

  import ru.otus.module2.catsHomework.{Branch, Empty, Leaf, Tree, given}

  private val emptyTree: Tree[Int] = Tree.empty
  private val singleLeaf: Tree[Int] = Tree.leaf(1)
  private val simpleTree: Tree[Int] = Tree(List(1, 2, 3, 4))
  //      Branch(1)
  // Leaf(2)    Branch(3)
  //          Empty     Leaf(4)

  "Functor[Tree] instance" should "exist and work correctly" in {
    treeFunctor shouldNot be(null)
  }

  "Functor[Tree] map function" should "work correctly with empty tree" in {
    val result = treeFunctor.map(emptyTree)(_ * 2)
    result shouldBe emptyTree
  }

  it should "correctly transform single leaf" in {
    val result = treeFunctor.map(singleLeaf)(_ * 2)
    result shouldBe Tree.leaf(2)
  }

  it should "correctly transform tree" in {
    val result = treeFunctor.map(simpleTree)(_ * 2)
    result shouldBe Branch(2, Leaf(4), Branch(6, Empty, Leaf(8)))
  }

  it should "handle String type" in {
    val stringTree =
      Branch("a",
        Leaf("b"), Leaf("c"))
    val result = treeFunctor.map(stringTree)(_.toUpperCase)
    result shouldBe
      Branch("A",
        Leaf("B"), Leaf("C"))
  }

  it should "correctly handle complex transformations" in {
    val result = treeFunctor.map(simpleTree)(x => (x * 2).toString + "x")
    result shouldBe Branch("2x", Leaf("4x"), Branch("6x", Empty, Leaf("8x")))
  }

  // Тесты законов функтора

  "Functor laws on Functor[Tree]" should "satisfy identity law" in {
    // Закон идентичности: fa.map(a => a) <=> fa
    treeFunctor.map(simpleTree)(identity) shouldBe simpleTree
    treeFunctor.map(singleLeaf)(identity) shouldBe singleLeaf
    treeFunctor.map(emptyTree)(identity) shouldBe emptyTree
  }

  it should "satisfy composition law" in {
    // Закон композиции: fa.map(g(f(_))) <=> fa.map(f).map(g)
    //           fu.map(fa)(f andThen g) <=> fu.map(fu.map(fa)(f))(g)
    val f = (x: Int) => x * 2
    val g = (y: Int) => y + 1

    val leftSide = treeFunctor.map(simpleTree)(f andThen g)
    val mapF = treeFunctor.map(simpleTree)(f)
    val rightSide = treeFunctor.map(mapF)(g)

    leftSide shouldBe rightSide
  }

  it should "satisfy composition law (better syntax)" in {
    // Закон композиции: fa.map(g(f(_))) <=> fa.map(f).map(g)
    val f = (x: Int) => x * 2
    val g = (y: Int) => y + 1

    simpleTree.map(g compose f) shouldBe simpleTree.map(f).map(g)
  }

}
