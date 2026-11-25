package ru.otus.module1

/**
 *
 * Структура данных Option, которая указывает на присутствие либо отсутствие результата
 */
object OptionT {

  sealed trait OptionT[+T] {
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
    def map[B](f: T => B): OptionT[B] =
      if (isEmpty) None else Some(f(get))

    /**
     * flatMap: если есть значение — примени функцию, которая сама может вернуть Option
     */
    def flatMap[B](f: T => OptionT[B]): OptionT[B] =
      if (isEmpty) None else f(get)

    /**
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Unit =
      map(v => println(v)) // для None map ничего не сделает, для Some вызовет println

    /**
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[B](that: OptionT[B]): OptionT[(T, B)] =
      if (this.isEmpty || that.isEmpty) None
      else Some((this.get, that.get))

    /**
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true (иначе None)
     */
    def filter(p: T => Boolean): OptionT[T] =
      if (isEmpty) None
      else if p(get) then Some(get) else None
  }

  /**
   * Реализация для Some: у нас есть значение
   */
  case class Some[T](v: T) extends OptionT[T] {
    override def isEmpty: Boolean = false

    override def get: T = v
  }

  /**
   * Реализация для None: значения нет
   */
  case object None extends OptionT[Nothing] {
    override def isEmpty: Boolean = true

    // Вызов метода get на None — это ошибка
    override def get: Nothing = throw new NoSuchElementException("None.get")
  }

  def apply[T](v: T): OptionT[T] = Some(v)

  def empty[T]: OptionT[T] = None

  given someToOptionT[T]: Conversion[Some[T], OptionT[T]] =
    some => OptionT.Some(some.get)
}