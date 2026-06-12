package ru.otus.module1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.otus.module1.optionImpl.{Option, Some, None}


class OptionSpec extends AnyFlatSpec with Matchers:

  "getOrElse" should "вернуть значение из Some" in {
    Some(10).getOrElse(0) === 10
  }

  it should "вернуть дефолт для None" in {
    None.getOrElse(-1) === -1
  }

  it should "не вычислять default при наличии значения (call-by-name)" in {
    var called: Boolean = false
    (Some(5): Option[Int]).getOrElse {
      called = true; 99
    }
    called === false
  }

  it should "вычислять default только для None" in {
    var called = false
    None.getOrElse { called = true; 42 }
    called === true
  }

  "map" should "сохранить None после map" in {
    val noneInt: Option[Int] = None
    noneInt.map[Int](_ * 3) === None
  }

  "flatMap" should "вызвать lookup для Some и вернуть его Option" in {
    def lookupUser(id: Int): Option[String] =
      if id > 0 then Some(s"User_$id") else None

    (Some(5): Option[Int]).flatMap(lookupUser) === Some("User_5")
  }

  it should "превратить Some в None, если lookup вернул None" in {
    def lookupBad(_id: Int): Option[String] = None
    (Some(99): Option[Int]).flatMap(lookupBad) === None
  }

  it should "не вызывать lookup для None (экономия вызовов)" in {
    var lookupCalled = false
    def lookupTracked(_id: Int): Option[String] = {
      lookupCalled = true
      Some("User_X")
    }
    None.flatMap(lookupTracked)
    lookupCalled === false
  }

  "zip" should "создать пару, если оба Some" in {
    Some(1).zip(Some("a")) === Some((1, "a"))
  }

  it should "вернуть None, если хотя бы один None" in {
    None.zip(Some("a")) === None
    Some(1).zip(None) === None
  }

  "filter" should "оставить Some при true-предикате" in {
    Some(3).filter(_ > 0) === Some(3)
  }

  it should "обнулить Some при false-предикате" in {
    Some(-3).filter(_ > 0) === None
  }

  it should "оставить None без изменений" in {
    val noneInt: Option[Int] = None
    noneInt.filter(_ > 0) === None
  }

  "None.get" should "бросить NoSuchElementException" in {
    a[NoSuchElementException] should be thrownBy {
      None.get
    }
  }

  "Option factory" should "корректно создавать Some и empty" in {
    Option(42) === Some(42)
    Option.empty[Int] === None
  }

end OptionSpec
