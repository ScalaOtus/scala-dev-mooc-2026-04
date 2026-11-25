package ru.otus.module1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.otus.module1.OptionT.*


class OptionTSpec extends AnyFlatSpec with Matchers {

  "Some" should "correctly handle value presence" in {
    val someValue = OptionT(42)
    someValue.isEmpty should be(false)
    someValue.get should be(42)
  }

  it should "correctly apply map function" in {
    val someValue = OptionT(42)
    someValue.map(_ * 2) should be(Some(84))
  }

  it should "correctly apply flatMap function" in {
    val someValue = OptionT(42)
    someValue.flatMap(x => Some(x * 2)) should be(Some(84))
  }

  it should "correctly filter values" in {
    val someValue = OptionT(42)
    someValue.filter(_ > 40) should be(Some(42))
    someValue.filter(_ < 40) should be(OptionT.None)
  }

  it should "correctly zip with another Some" in {
    val some1 = OptionT(42)
    val some2 = Some("test")
    some1.zip(some2) should be(Some((42, "test")))
  }

  "OptionT.None" should "correctly handle absence of value" in {
    OptionT.None.isEmpty should be(true)
  }

  it should "throw exception on get" in {
    an[NoSuchElementException] should be thrownBy {
      OptionT.None.get
    }
  }

  it should "return None on map" in {
    OptionT.empty[Int].map(_ * 2) should be(OptionT.empty[Int])
  }

  "None" should "return None on flatMap" in {
    OptionT.empty[Int].flatMap(x => Some(x * 2)) should be(OptionT.empty[Int])
  }

  "Some" should "apply flatMap function" in {
    OptionT(42).flatMap(x => Some(x * 2)) should be(Some(84))
  }

  "flatMap with None result" should "return None" in {
    OptionT(42).flatMap(_ => OptionT.None) should be(OptionT.None)
  }

  "flatMap with Some result" should "apply function" in {
    OptionT(42).flatMap(x => Some(x * 2)) should be(Some(84))
  }

  it should "return None on zip with None" in {
    OptionT.None.zip(Some(42)) should be(OptionT.None)
    OptionT(42).zip(OptionT.None) should be(OptionT.None)
  }

  "getOrElse method" should "return default value for None" in {
    OptionT.None.getOrElse(42) should be(42)
  }

  it should "return value for Some" in {
    OptionT(42).getOrElse(0) should be(42)
  }

  it should "do nothing for None" in {
    OptionT.None.printIfAny() // Ничего не должно произойти
  }

  "Conversion from Some to OptionT" should "work correctly" in {
    val scalaSome: Some[String] = Some("test")
    val converted: OptionT[String] = scalaSome

    converted shouldEqual Some("test")
    converted.get === "test"
    converted.isEmpty === false
  }

  it should "work correctly for integers" in {
    val scalaSome: Some[Int] = Some(42)
    val converted: OptionT[Int] = scalaSome

    converted === Some(42)
    converted.get shouldEqual 42
    converted.isEmpty === false
  }

}
