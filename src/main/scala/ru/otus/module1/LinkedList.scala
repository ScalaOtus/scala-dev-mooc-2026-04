package ru.otus.module1

import ru.otus.module1.LinkedList.{Cons, Nil}
import ru.otus.module1.OptionT.*

/**
 * Реализовать одно связанный иммутабельный список List
 * Список имеет два случая:
 * Nil - пустой список
 * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
 */

object LinkedList {

  sealed trait LinkedList[+T] {
    def isEmpty: Boolean
    def length: Int
    def headOption: OptionT[T]
    def tailOption: OptionT[LinkedList[T]]
    def head: T
    def tail: LinkedList[T]

    /**
     * Метод ++ соединяет списки
     */
    def ++[TT >: T](that: LinkedList[TT]): LinkedList[TT] =
      if this.isEmpty then that
      else Cons(this.head, this.tail ++ that)

    /**
     * Реализовать метод map для списка, который будет применять некую ф-цию к элементам данного списка
     */
    def map[B](f: T => B): LinkedList[B] =
      if this.isEmpty then Nil
      else Cons(f(head), tail.map(f))

    def flatMap[B](f: T => LinkedList[B]): LinkedList[B] =
      if this.isEmpty then Nil
      else f(head) ++ tail.flatMap(f)

    /**
     * Реализовать метод filter для списка, который будет фильтровать список по некому условию
     */
    def filter(p: T => Boolean): LinkedList[T] =
      if this.isEmpty then Nil
      else if p(head) then Cons(head, tail.filter(p))
      else tail.filter(p)

    /**
     * Написать функцию incList которая будет принимать список Int и возвращать список,
     * где каждый элемент будет увеличен на 1
     * <:< — это встроенный в Scala тип-класс для подтипизации. "Этот метод имеет смысл, только если T является подтипом Int"
     */
    def incList(using ev: T <:< Int): LinkedList[Int] =
      this.map(x => ev(x) + 1)

    /**
     * Написать функцию shoutString, которая будет принимать список String и возвращать список,
     * где к каждому элементу будет добавлен префикс в виде '!'
     */
    def shoutString(using ev: T <:< String): LinkedList[String] =
      this.map(x => "!" + ev(x))

    /**
     * Реализовать метод mkString
     */
    def mkString(sep: String): String =
      if this.isEmpty then ""
      else this match
        case Cons(h, t) =>
          val headStr = h.toString
          if t.isEmpty then headStr
          else headStr + sep + t.mkString(sep)

    /**
     * Реализовать метод reverse, который позволит заменить порядок элементов в списке на противоположный
     */
    def reverse: LinkedList[T] =
      @annotation.tailrec
      def go(acc: LinkedList[T], rest: LinkedList[T]): LinkedList[T] =
        if rest.isEmpty then acc
        else go(Cons(rest.head, acc), rest.tail)

      go(Nil, this)

  }

  case object Nil extends LinkedList[Nothing]:
    override def isEmpty: Boolean = true
    override def length: Int = 0
    override def headOption: OptionT[Nothing] = OptionT.None
    override def tailOption: OptionT[LinkedList[Nothing]] = OptionT.None
    override def head: Nothing = throw new NoSuchElementException("head of empty list")
    override def tail: LinkedList[Nothing] = throw new NoSuchElementException("tail of empty list")

  // класс-конструктор для непустого списка
  case class Cons[+T](head: T, tail: LinkedList[T]) extends LinkedList[T]:
    override def isEmpty: Boolean = false
    override def length: Int = 1 + tail.length
    override def headOption: OptionT[T] = Some(head)
    override def tailOption: OptionT[LinkedList[T]] = Some(tail)

  /**
   * Фабрика для создания всего списка сразу из произвольного числа элементов (из varargs)
   * foldLeft в Scala обычно итеративен (не ест стек, в отличие от рекурсии) - этот способ безопаснее для больших varargs
   */
  def apply[T](as: T*): LinkedList[T] =
    as.foldLeft(Nil: LinkedList[T])((acc, x) => Cons(x, acc)).reverse

/*
  def apply[T](s: T*): LinkedList[T] =
    if s.isEmpty then Nil
    else Cons(s.head, apply(s.tail: _*)) // здесь методы head/tail из Seq[T]; конструкция : _* превратит Seq обратно в varargs
*/

  def empty[T]: LinkedList[T] = Nil

  /**
   * Инфиксный оператор (работают справа налево) :: , чтобы работало x Cons list или 1 Cons Nil
   * Это фабричный метод в компаньоне, и его задача - удобно создавать узлы, не вызывая new Cons(...) напрямую.
   * Превращает запись List(1, 2, 3) в цепочку узлов: Cons(1, Cons(2, Cons(3, Nil))).
   */
  // TODO: доделать инфиксный оператор ::
  // infix def ::[B](head: B, list: LinkedList[B]): LinkedList[B] = Cons(head, list)

}
