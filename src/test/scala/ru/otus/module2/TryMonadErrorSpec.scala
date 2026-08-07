package ru.otus.module2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class TryMonadErrorSpec extends AnyFlatSpec with Matchers {

  import scala.util.{Try, Success, Failure}
  import ru.otus.module2.catsHomework.{TryMonadError, TryStr}

  "pure" should "wrap a value in Success" in {
    val result = TryMonadError.pure(42)
    result shouldBe Success(42)
  }

  "flatMap" should "chain successful computations" in {
    //                               (fa: TryStr[A]) (f: A => TryStr[B])
    val result = TryMonadError.flatMap(Success(10)) { x => Success(x * 2) }
    result shouldBe Success(20)
  }

  it should "propagate failure from the first step" in {
    val error = new RuntimeException("Error")
    val result = TryMonadError.flatMap(Failure(error)) { _ => Success(0) }
    result shouldBe Failure(error)
  }

  it should "propagate failure from the function" in {
    val error = new RuntimeException("inner exception")
    val result = TryMonadError.flatMap(Success(5)) { _ => Failure(error) }
    result shouldBe Failure(error)
  }

  "raiseError" should "create a Failure with given Throwable" in {
    val err = new IllegalArgumentException("bad value")
    val result = TryMonadError.raiseError[Int](err)
    result shouldBe Failure(err)
  }

  // способ поймать и обработать ошибку, где обработчик тоже может упасть (здесь падает)
  "handleErrorWith" should "recover from Failure using provided function" in {
    val originalErr = new RuntimeException("error")
    val recoveryErr = new RuntimeException("recovered")

    val recovered = TryMonadError.handleErrorWith(Failure(originalErr)) { _ =>
      Failure(recoveryErr)
    }
    recovered shouldBe Failure(recoveryErr)
  }

  it should "leave Success unchanged" in {
    val ok = Success(123)
    val f: Throwable => TryStr[Int] = _ => Failure(new Exception("should not be used"))
    val result = TryMonadError.handleErrorWith(ok)(f)
    result shouldBe ok
  }

  "handleError" should "convert error to a value and wrap it in Success" in {
    val err = new NumberFormatException("NaN")
    val recoveredValue = 0
    val result = TryMonadError.handleError(Failure(err)) { _ => recoveredValue }
    result shouldBe Success(recoveredValue)
  }

  it should "leave Success unchanged" in {
    val ok = Success(999)
    val f: Throwable => Int = _ => 0
    val result = TryMonadError.handleError(ok)(f)
    result shouldBe ok
  }

  "ensure" should "return Success if predicate holds" in {
    val value = 10
    val predicate: Int => Boolean = _ > 0
    val error = new Exception("not positive")

    val result = TryMonadError.ensure(Success(value))(error)(predicate)
    result shouldBe Success(value)
  }

  it should "raise error if predicate fails" in {
    val value = -5
    val predicate: Int => Boolean = _ > 0
    val expectedError = new Exception("not positive")

    val result = TryMonadError.ensure(Success(value))(expectedError)(predicate)
    result.isFailure shouldBe true
    // проверка, что внутри Failure лежит наша ошибка
    result.failed.get shouldBe expectedError
  }

  // важное свойство ensure: если в контексте уже есть ошибка, предикат не должен выполняться вообще
  it should "not run predicate if initial value is Failure" in {
    val originalError = new Exception("already failed")   // есть ошибка до вызова ensure
    var predicateCalled = false
    // предикат, которые всегда возвращает true и сообщает, был ли он вызван
    val predicate: Int => Boolean = { _ => predicateCalled = true; true }
    val ensureError = new Exception("from ensure")

    val result = TryMonadError.ensure(Failure(originalError))(ensureError)(predicate)

    result.isFailure shouldBe true
    result.failed.get shouldBe originalError  // внутри именно исходная ошибка (до вызова)
    predicateCalled shouldBe false    // предикат НЕ должен сработать
  }
}
