package ru.otus.module1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.otus.module1.LinkedList.*


class LinkedListSpec extends AnyFlatSpec with Matchers {

  private val emptyList: LinkedList[Int] = Nil
  private val oneElementList: LinkedList[Int] = Cons(1, emptyList)
  private val twoElementsList: LinkedList[Int] = Cons(2, oneElementList)
  private val threeElementsList: LinkedList[Int] = Cons(3, twoElementsList)

  "Empty List" should "have correct properties" in {
    emptyList.isEmpty should be(true)
    emptyList.length should be(0)
    emptyList.headOption === None
    emptyList.tailOption === None
  }

  "One Element List" should "have correct properties" in {
    oneElementList.isEmpty should be(false)
    oneElementList.length should be(1)
    oneElementList.headOption === Some(1)
    oneElementList.tailOption === Some(emptyList)
  }

  "Basic operations" should "work correctly" in {
    threeElementsList.head should be(3)
    threeElementsList.tail should be(twoElementsList)
    threeElementsList.length should be(3)
  }

  "Map function" should "transform elements" in {
    oneElementList.map(_ * 2) should be(LinkedList(2))
    threeElementsList.map(_ * 2) should be(LinkedList(6, 4, 2))
  }

  "FlatMap function" should "flatten lists" in {
    oneElementList.flatMap(x => LinkedList(x, x * 10)) should be(LinkedList(1, 10))
    threeElementsList.flatMap(x => LinkedList(x, x * 10)) should be(LinkedList(3, 30, 2, 20, 1, 10))
  }

  "Filter function" should "filter elements" in {
    threeElementsList.filter(_ % 2 == 0) should be(LinkedList(2))
    threeElementsList.filter(_ > 3) should be(emptyList)
  }

  "Lists concatenation" should "work correctly" in {
    emptyList ++ emptyList should be(emptyList)
    emptyList ++ oneElementList should be(oneElementList)
    oneElementList ++ emptyList should be(oneElementList)
    oneElementList ++ oneElementList === LinkedList(1, 1)
  }

  "reverse function" should "reverse lists" in {
    emptyList.reverse should be(emptyList)
    oneElementList.reverse should be(oneElementList)
    threeElementsList.reverse should be(LinkedList(1, 2, 3))
  }

  "mkString function" should "join strings" in {
    emptyList.mkString(",") should be("")
    oneElementList.mkString(",") should be("1")
    threeElementsList.mkString(",") should be("3,2,1")
  }

  "shoutString" should "work as expected" in {
    oneElementList.incList should be(LinkedList(2))
    LinkedList("hi", "hello").shoutString should be(LinkedList("!hi", "!hello"))
  }

}
