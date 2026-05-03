package ru.otus.module1

object functions {

  /**
   * Реализовать метод sum, которая будет суммировать 2 целых числа и выдавать результат
   */

  // SAM

  trait Printer {
    def print(s: String): Unit
  }

  val p: Printer = s => println(s)
  p.print("hello")

  def sum(x: Int, y: Int): Int = x + y


  sum(2, 3) // 5

  val sum2: (Int, Int) => Int = (x, y) => x + y

  sum2(3, 2) // 5
  sum(3, 2) // 5

  val sum3: Function2[Int, Int, Int] = sum2

  sum3(3, 2) // 5

  val sum4 = sum

  sum4(3, 2) // 3

  // Currying
  val sumCurried: Int => Int => Int = sum2.curried

  val r1 = sumCurried(2)(3) // 5
  val r2: Int => Int = sumCurried(2)

  r2(2) //4
  r2(5) // 7

  // Partial function

  val divide: PartialFunction[(Int, Int), Int] =
    case x if x._2 != 0 => x._1 / x._2

  val divide2: PartialFunction[(Int, Int), Int] = new PartialFunction[(Int, Int), Int] {
    override def isDefinedAt(x: (Int, Int)): Boolean = x._2 != 0

    override def apply(v1: (Int, Int)): Int = v1._1 / v1._2
  }

  val r3 = if divide.isDefinedAt(10, 2)  then divide(10, 2) else 0

  val r4 = List((10, 2), (5, 0), (4, 1)).collect(divide) // List(5, 4)

  /**
   *  Задание 1. Написать ф-цию метод isEven, которая будет вычислять является ли число четным
   */

  val isEven: Int => Boolean = x => x % 2 == 0


  /**
   * Задание 2. Написать ф-цию метод isOdd, которая будет вычислять является ли число нечетным
   */
  val isOdd: Int => Boolean = x => x % 2 != 0

  /**
   * Задание 3. Написать ф-цию метод filterEven, которая получает на вход массив чисел и возвращает массив
   * только четных
   */
  val filterEven: Array[Int] => Array[Int] = array => array.filter(x => x % 2 == 0)

  /**
   * Задание 4. Написать ф-цию метод filterOdd, которая получает на вход массив чисел и возвращает массив
   * только нечетных
   */

  val filterOdd: Array[Int] => Array[Int] = array => array.filter(x => x % 2 != 0)


}