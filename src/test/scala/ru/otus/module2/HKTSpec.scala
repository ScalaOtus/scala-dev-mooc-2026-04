package ru.otus.module2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.otus.module2.higher_kinded_types.{tupleF, given}

class HKTSpec extends AnyFlatSpec with Matchers {

  val optA: Option[Int] = Some(10)
  val optB: Option[Int] = Some(20)
  val optNone: Option[Int] = None
  val listA = List(1, 2)
  val listB = List("A", "B")
  val listExpected = List(
    (1, "A"), (1, "B"),
    (2, "A"), (2, "B")
  )
  "tupleF with Option" should "combine two Some values into a Some pair" in {
    tupleF(optA, optB) shouldBe Some((10, 20))
  }

  it should "not be equal to a different pair" in {
    tupleF(optA, optB) should not equal Some((10, 30))
  }

  it should "return None if the second argument is None" in {
    tupleF(optA, optNone) shouldBe None
  }

  it should "return None if the first argument is None" in {
    tupleF(optNone, optB) shouldBe None
  }

  "tupleF with List" should "produce all pairs" in {
    
    tupleF(listA, listB) shouldBe listExpected
  }

  it should "return an empty list if the first list is empty" in {
    tupleF(List.empty[Int], listB) shouldBe List.empty
  }

  it should "return an empty list if the second list is empty" in {
    tupleF(listA, List.empty[String]) shouldBe List.empty
  }

  it should "return an empty list if both lists are empty" in {
    tupleF(List.empty[Int], List.empty[String]) shouldBe List.empty
  }
}