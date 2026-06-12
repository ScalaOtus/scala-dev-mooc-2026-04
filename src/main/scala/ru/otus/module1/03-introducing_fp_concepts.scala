package ru.otus.module1
import scala.language.postfixOps
import scala.math.Fractional.Implicits.infixFractionalOps
import scala.math.Numeric.Implicits.infixNumericOps
import scala.annotation.tailrec

//import ru.otus.module1.variance.{Animal, Cat}


/**
 * referential transparency
 */

// recursion

object recursion {
  def main(args: Array[String]): Unit = {
    /**
     * Реализовать метод вычисления n!
     * n! = 1 * 2 * ... n
     */

    def fact(n: Int): Int = {
      var _n = 1
      var i = 2
      while (i <= n) {
        _n *= i
        i += 1
      }
      _n
    }


    def factRec(n: Int): Int =
      if (n <= 0) 1 else n * factRec(n - 1)


    def factTailRec(n: Int): Int = {
      @tailrec
      def loop(n: Int, accum: Int): Int =
        if (n <= 0) accum
        else loop(n - 1, n * accum)

      loop(n, 1)
    }

    println(factTailRec(4))

    /**
     * Реализовать вычисление N числа Фибоначчи
     * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
     */

  }
}



object hof{
  def main(args: Array[String]): Unit = {

    def dumb(string: String): Unit = {
      Thread.sleep(1000)
      println(string)
    }

    // обертки

    def logRunningTime[A, B](f: A => B): A => B = a =>
      val start = System.currentTimeMillis()
      val result = f(a)
      val end = System.currentTimeMillis()
      println(end - start)
      result


    // изменение поведения ф-ции

    def isOdd(i: Int): Boolean = i % 2 > 0

    lazy val isEven: Int => Boolean = not(isOdd)

    def not[A](f: A => Boolean): A => Boolean = a => !f(a)


    // изменение самой функции

    def sum(x: Int, y: Int): Int = x + y
    // Автоматическое каррирование
    val sum2 = (x: Int, y: Int) => x + y
    val curriedSum = sum2.curried  // стандартный метод Scala

    // Преобразование обычной функции в каррированную
    def curried[A, B, C](f: (A, B) => C): A => (B => C) =
      a => b => f(a, b)

    // curried((a, b) => a + b)

    // Эквивалентные записи
    def add(x: Int)(y: Int) = x + y

    def add2 = (x: Int) => (y: Int) => x + y

    // Обратное преобразование
    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
      (a, b) => f(a)(b)

    curried(sum) // Int => Int => Int

    def partial2[A, B, C](a: A, f: (A, B) => C): B => C = curried(f)(a)

    val r: Int => Int = partial2(2, sum)
    r(3) // 5
  }
}


object variance {
  // Invariance Вне зависимости от отношений между типами A и B, Box[A] и Box[B] два разных типа
  // + Covariance Если А является подтипом В, то Box[A] является подтипом Box[B]
  // - Contravariance Если А является подтипом В, то Box[A] является супер типом Box[B]

  class Box[+T](val item: T)

  class Feeder[-T] {
    def feed(v: T): Unit = println(s"Feeding $v")
  }

  sealed trait Animal:
    val name: String

  case class Cat(name: String) extends Animal:
    override def toString = s"$name (cat)"

  case class Dog(name: String) extends Animal:
    override def toString = s"$name (dog)"

  val animalFeeder: Feeder[Animal] = Feeder[Animal]()

  def feed(a: Animal): Unit =
    animalFeeder.feed(a)

  val catFeeder: Feeder[Cat] = animalFeeder
  val dogFeeder: Feeder[Dog] = animalFeeder

  def main(args: Array[String]): Unit = {

    catFeeder.feed(Cat("Syoma"))
    dogFeeder.feed(Dog("Ray"))

    feed(Cat("Nora"))
    feed(Dog("Balu"))

    //    trait Function1[-R, +T] = R => T

    //    val f1: Animal => Dog = ???
    //    val f2: Dog => Animal = f1
  }

}

/**
 *  Реализуем тип Option
 */

object opt {

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутствие результата
   */

  sealed trait Option[+T] {
    def isEmpty: Boolean = if(this.isInstanceOf[None.type]) true else false

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] =
      if (isEmpty) None
      else f(this.asInstanceOf[Some[T]].v)
  }

  case class Some[T](v: T) extends Option[T]
  case object None extends Option[Nothing]

  object Option {
    def apply[T](v: T): Option[T] = Some(v)
  }

  //  var animalOpt: Option[Animal] = None
  //  var intOpt: Option[Int] = ???

  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */


  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */


  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */

 }


object optionImpl {

  /**
   *
   * Структура данных Option, которая указывает на присутствие либо отсутствие результата
   */

  sealed trait Option[+T] {
    // Каждый подтип обязан сказать, пуст ли он
    def isEmpty: Boolean

    // Получить значение; на None вызовет ошибку
    def get: T

    /**
     * getOrElse: если пусто — верни дефолт, иначе верни значение.
     * (default: => B -> тут передача "по имени")
     */
    def getOrElse[B >: T](default: => B): B =
      if (isEmpty) default else get

    /**
     * map: если есть значение — примени функцию, иначе оставь None
     */
    def map[B](f: T => B): Option[B] =
      if (isEmpty) None else Some(f(get))

    /**
     * flatMap: если есть значение — примени функцию, которая сама может вернуть Option
     */
    def flatMap[B](f: T => Option[B]): Option[B] =
      if (isEmpty) None else f(get)

    /**
     * Метод printIfAny: печатать значение, если оно есть
     */
    def printIfAny(): Unit =
      map(v => println(v))  // для None map ничего не сделает, для Some вызовет println

//    def printIfAny(): Unit =
//      if (!isEmpty) println(get)

    /**
     * Метод zip: создавать Option от пары значений из 2-х Option (если они есть, иначе None)
     */
    def zip[B](that: Option[B]): Option[(T, B)] =
      if (this.isEmpty || that.isEmpty) None
      else Some((this.get, that.get))

    /**
     * Метод filter: будет возвращать Some ("непустой Option"), если есть значение и оно проходит предикат,
     * иначе None
     */
    def filter(p: T => Boolean): Option[T] =
      if (isEmpty) None
      else if p(get) then Some(get) else None

    /**
     * Свёртка: мощный способ заменить if (opt.isDefined) ... else ...
     * Аналог when(col.isNotNull, f(col), elseValue) в Spark
     */
    // def fold[B](ifEmpty: => B)(f: T => B): B =
    //  if (isEmpty) ifEmpty else f(get)
  }

  /**
   * Реализация для Some: у нас есть значение
   */
  case class Some[T](v: T) extends Option[T] {
    override def isEmpty: Boolean = false

    override def get: T = v
  }

  /**
   * Реализация для None: значения нет
   */
  case object None extends Option[Nothing] {
    override def isEmpty: Boolean = true

    // get на None — это ошибка. В пайплайнах мы стараемся никогда не вызывать get без isEmpty/fold
    override def get: Nothing = throw new NoSuchElementException("None.get")
  }

  /**
   * Объект-фабрика (чтобы писать Option(x) вместо Some(x))
   */
  object Option {
    def apply[T](v: T): Option[T] = Some(v)

    def empty[T]: Option[T] = None
  }

  //  метод printIfAny, который будет печатать значение, если оно есть
}


object list {
   /**
    *
    * Реализовать одно связанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
    */

    sealed trait List[+T]{
      def ::[TT >: T](elem: TT): List[TT] = ???
    }


    case class ::[A](head: A, tail: List[A]) extends List[A]
    case object Nil extends List[Nothing]

    object List {
      def apply[A](v: A*): List[A] =
        if(v.isEmpty) Nil else ::(v.head, apply(v.tail:_*))
    }






    /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      * 
      * Например, вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */

    /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */

    /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */


    /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */

    /**
      *
      * Написать функцию incList которая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */


    /**
      *
      * Написать функцию shoutString которая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */

 }