package ru.otus.module2.catsHomework

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EitherMonadErrorSpec extends AnyFlatSpec with Matchers {

  import ru.otus.module2.catsHomework.{EitherMonadError, EitherStr}

  "pure" should "return Right with the given value" in {
    EitherMonadError.pure(42) shouldBe Right(42)
  }

  "raiseError" should "return Left with the error message" in {
    EitherMonadError.raiseError("Error") shouldBe Left("Error")
  }

  "flatMap" should "execute the function for Right value" in {
    val start: EitherStr[Int] = Right(10)
    val res = EitherMonadError.flatMap(start)(x => Right(x * 2))
    res shouldBe Right(20)
  }

  it should "not execute the function for Left value" in {
    val start: EitherStr[Int] = Left("bad")
    val res = EitherMonadError.flatMap(start)(_ => Right(100))
    res shouldBe Left("bad")
  }

  "handleErrorWith" should "apply the recovery function when given a Left value" in {
    val start = Left("initial error")
    val res = EitherMonadError.handleErrorWith(start) { err =>
      if (err == "initial error") Right(42) else Left("other")
    }
    res shouldBe Right(42)
  }

  "handleError" should "convert the error into a value" in {
    val start = Left("parse error")
    val res = EitherMonadError.handleError(start)(_ => 0)
    res shouldBe Right(0)
  }

  "ensure" should "keep the Right value if the condition holds" in {
    val start = Right(10)
    val res = EitherMonadError.ensure(start)("fail")(_ > 5)
    res shouldBe Right(10)
  }

  it should "raise an error if the condition does not hold" in {
    val start = Right(3)
    val res = EitherMonadError.ensure(start)("too small")(_ > 5)
    res shouldBe Left("too small")
  }
}