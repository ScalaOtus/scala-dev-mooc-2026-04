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

/*    // Определяем тип для given как функцию
    type TreeShow[A] = Tree[A] => String

    // Добавляем given как функцию
    given treeShow[A]: TreeShow[A] = (tree: Tree[A]) => {
      def printTree(t: Tree[A], level: Int = 0, indent: String = "    "): String = t match {
        case Empty => "Empty"
        case Leaf(v) => List.fill(level)(indent).mkString + s"Leaf($v)"
        case Branch(v, l, r) =>
          val indentation: String = List.fill(level)(indent).mkString
          val leftStr: String = printTree(l, level + 1, indent)
          val rightStr: String = printTree(r, level + 1, indent)

          s"${indentation}Branch($v)\n" +
            leftStr.replaceAll("\n", s"\n$indent") + "   " +
            rightStr.replaceAll("\n", s"\n$indent$indent")
      }

      printTree(tree)
    }

    // Добавляем extension-метод для удобства
    extension [A](tree: Tree[A]) {
      def show: String = summon[TreeShow[A]](tree)
    }
*/
  }


  /**
   * Напишите instance Functor для объявленного выше бинарного дерева.
   * Проверьте, что код работает корректно для Branch и Leaf
   */

  trait Functor2[F[_]]:
    def map[A, B](fa: F[A])(f: A => B): F[B]

  given func[R]: Functor2[[A] =>> (R) => A] with
    def map[A, B](fa: R => A)(f: A => B): R => B = fa andThen f

  object TreeFunctor extends Functor2[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Empty => Empty
      case Leaf(value) => Leaf(f(value))
      case Branch(value, left, right) => Branch(f(value), left.map(f), right.map(f))
    }
  }

  given Functor2[Tree] = TreeFunctor

  object TreeSyntax {
    extension [A](t: Tree[A])(using F: Functor2[Tree]) {
      def map[B](f: A => B): Tree[B] = F.map(t)(f)
    }
  }

//  Запуск тестов решения по задаче 1:
//    sbt clean compile
//    sbt "testOnly ru.otus.module2.FunctorTreeSpec"


  /**
   * Monad абстракция для последовательной
   * комбинации вычислений в контексте F
   * @tparam F
   */
  trait Monad[F[_]]{
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](v: A): F[A]
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

    // Test an instance of `F`,
    // failing if the predicate is not satisfied:
    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  }

  /**
   * Напишите instance MonadError для Try
   */



  /**
   * Напишите instance MonadError для Either,
   * где в качестве типа ошибки будет String
   */



}
