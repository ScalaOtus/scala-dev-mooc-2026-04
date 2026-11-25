package ru.otus.module2

object higher_kinded_types{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap{ a => b.map((a, _))}



  // def tupleF[F[_], A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???

  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] =
    fa.flatMap{ a => fb.map((a, _))}


  def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
    override def map[B](f: A => B): Option[B] = opt.map(f)

    override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
  }

  def listBindable[A](opt: List[A]): Bindable[List, A] = new Bindable[List, A] {
    override def map[B](f: A => B): List[B] = opt.map(f)

    override def flatMap[B](f: A => List[B]): List[B] = opt.flatMap(f)
  }



  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val r1 = println(tupleBindable(optBindable(optA), optBindable(optB)))
  val r2 = println(tupleBindable(listBindable(list1), listBindable(list2)))

  // Задача: Использовать на практике концепции занятия - HKT и Implicits и реализовать общий метод tupleF, который
  // будет способен превратить два любых контейнера с типом F[A] и F[B] в один контейнер с типом F[(A, B)].

  // абстракция над HKT F[_] = "для любого F я умею делать map и flatMap"
  trait FBindable[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  // Реализуем tupleF через FBindable и заданной где-то реализации через given для конкретного типа (должен работать с ops)
  def tupleF[F[_], A, B](fa: F[A], fb: F[B])(using ops: FBindable[F]): F[(A, B)] =
    ops.flatMap(fa) { a =>
      ops.map(fb)((a, _))
    }

  given optionFBindable: FBindable[Option] with {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  given listFBindable: FBindable[List] with {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
}