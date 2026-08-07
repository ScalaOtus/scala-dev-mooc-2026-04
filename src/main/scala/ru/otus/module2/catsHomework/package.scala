package ru.otus.module2

package object catsHomework {

  /**
   * Простое бинарное дерево
   * @tparam A
   */
  sealed trait Tree[+A] {
    def isEmpty: Boolean
    def map[B](f: A => B): Tree[B]
  }

  final case class Leaf[A](value: A) extends Tree[A] {
    override def isEmpty: Boolean = false

    override def map[B](f: A => B): Tree[B] = Leaf(f(value))
  }

  final case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A] {
    override def isEmpty: Boolean = false

    override def map[B](f: A => B): Tree[B] =
      Branch(f(value), left.map(f), right.map(f))
  }

  case object Empty extends Tree[Nothing] {
    override def isEmpty: Boolean = true

    override def map[B](f: Nothing => B): Tree[B] = Empty
  }

  object Tree {
    def empty[A]: Tree[A] = Empty

    def leaf[A](value: A): Tree[A] = Leaf(value)

    def branch[A](value: A, left: Tree[A], right: Tree[A]): Tree[A] = Branch(value, left, right)

    def apply[A](a: A): Tree[A] = leaf(a)

    // создание дерева из списка
    def apply[A](values: List[A]): Tree[A] = {
      def buildTree(list: List[A]): Tree[A] = list match {
        case Nil => Empty
        case x :: Nil => Leaf(x)
        case x :: tail =>
          val (leftHalf, rightHalf) = tail.splitAt(tail.length / 2)
          Branch(
            value = x,
            left = if (leftHalf.nonEmpty) buildTree(leftHalf) else Empty,
            right = if (rightHalf.nonEmpty) buildTree(rightHalf) else Empty
          )
      }
      buildTree(values)
    }


  }


  /**
   * 1. Напишите instance Functor для объявленного выше бинарного дерева.
   * Проверьте, что код работает корректно для Branch и Leaf
   */

  trait Functor2[F[_]]:
    def map[A, B](fa: F[A])(f: A => B): F[B]

  given func[R]: Functor2[[A] =>> (R) => A] with
    def map[A, B](fa: R => A)(f: A => B): R => B = fa andThen f

  // Functor для Tree
  given treeFunctor: Functor2[Tree] with {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Empty => Empty
      case Leaf(value) => Leaf(f(value))
      case Branch(value, left, right) =>
        Branch(f(value), left.map(f), right.map(f))
    }
  }

  object TreeSyntax {
    extension [A](t: Tree[A])(using F: Functor2[Tree]) {
      def map[B](f: A => B): Tree[B] = F.map(t)(f)
    }
  }

  // Запуск тестов по решению задачи 1:
  // sbt clean compile
  // sbt Test/clean "testOnly ru.otus.module2.FunctorTreeSpec"


  /**
   * Monad абстракция для последовательной
   * комбинации вычислений в контексте F
   * @tparam F
   */
  trait Monad[F[_]]{
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](v: A): F[A]
  }

  object Monad {
    extension [F[_], A, B](fa: F[A])(using M: Monad[F]) {
      def map(f: A => B): F[B] = M.flatMap(fa)(a => M.pure(f(a)))
    }
  }

  /**
   * MonadError расширяет возможность Monad
   * кроме последовательного применения функций, позволяет обрабатывать ошибки
   * @tparam F
   * @tparam E
   */
  trait MonadError[F[_], E] extends Monad[F]{
    // Поднимаем ошибку в контекст `F`:
    def raiseError[A](e: E): F[A]

    // Обработка ошибки, потенциальное восстановление:
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

    // Обработка ошибок, восстановление от них:
    def handleError[A](fa: F[A])(f: E => A): F[A]

    // Test an instance of `F`, failing if the predicate is not satisfied:
    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  }

  /**
   * 2.1 Напишите instance MonadError для Try
   */

  import scala.util.{Try, Success, Failure}

  type TryStr[A] = Try[A]   // ошибка в Throwable

  object TryMonadError extends MonadError[TryStr, Throwable] {
    // поднимает чистое значение A в контекст Try как успешный результат
    override def pure[A](a: A): TryStr[A] = Success(a)

    // строит последовательность шагов, где каждый шаг может упасть;
    // если fa - Success, применяет f; если fa - Failure, сразу возвращает ошибку
    override def flatMap[A, B](fa: TryStr[A])(f: A => TryStr[B]): TryStr[B] =
      fa.flatMap(f)

    // способ сообщить об ошибке внутри монадической цепочки
    override def raiseError[A](e: Throwable): TryStr[A] = Failure(e)

    // способ поймать и обработать ошибку, где обработчик тоже может упасть
    override def handleErrorWith[A](fa: TryStr[A])(f: Throwable => TryStr[A]): TryStr[A] =
      fa match {
        case Success(_) => fa
        case Failure(err) => f(err)
      }

    // восстанавливает ошибку до значения (A), предполагает, что восстановление всегда успешно
    override def handleError[A](fa: TryStr[A])(f: Throwable => A): TryStr[A] =
      handleErrorWith(fa)(err => pure(f(err)))

    // монадическая валидация: допустимо ли это значение в данном контексте (проверяет значение предикатом p)
    override def ensure[A](fa: TryStr[A])(e: Throwable)(p: A => Boolean): TryStr[A] =
      flatMap(fa) { a =>
        if (p(a)) pure(a) else raiseError(e)
      }
  }

  // Запуск тестов по решению задачи 2.1:
  // sbt clean compile
  // sbt Test / clean "testOnly ru.otus.module2.TryMonadErrorSpec"


  /**
   * 2.2 Напишите instance MonadError для Either,
   * где в качестве типа ошибки будет String
   */

  type EitherStr[A] = Either[String, A]

  object EitherMonadError extends MonadError[EitherStr, String] {
    override def pure[A](a: A): EitherStr[A] = Right(a)

    override def flatMap[A, B](fa: EitherStr[A])(f: A => EitherStr[B]): EitherStr[B] =
      fa.flatMap(f)

    override def raiseError[A](e: String): EitherStr[A] = Left(e)

    override def handleErrorWith[A](fa: EitherStr[A])(f: String => EitherStr[A]): EitherStr[A] =
      fa match {
        case Right(_) => fa
        case Left(err) => f(err)
      }

    override def handleError[A](fa: EitherStr[A])(f: String => A): EitherStr[A] =
      handleErrorWith(fa)(err => pure(f(err)))

    override def ensure[A](fa: EitherStr[A])(e: String)(p: A => Boolean): EitherStr[A] =
      flatMap(fa) { a =>
        if (p(a)) pure(a) else raiseError(e)
      }
  }

  // Запуск тестов по всем решениям:
  //   sbt clean compile
  //   sbt Test / clean "testOnly ru.otus.module2.*"


}
