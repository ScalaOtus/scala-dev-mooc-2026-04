package ru.otus.module2

import cats.Functor
//import cats.syntax.functor._
import catsHomework.{Branch, Empty, Leaf, Tree}

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

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    override def isEmpty: Boolean = false

    override def map[B](f: A => B): Tree[B] =
      Branch(left.map(f), right.map(f))
  }

  case object Empty extends Tree[Nothing] {
    override def isEmpty: Boolean = true

    override def map[B](f: Nothing => B): Tree[B] = Empty
  }

  object Tree {
    def empty[A]: Tree[A] = Empty

    def leaf[A](value: A): Tree[A] = Leaf(value)

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  }

  /**
   * Напишите instance Functor для объявленного выше бинарного дерева.
   * Проверьте, что код работает корректно для Branch и Leaf
   */

  object TreeFunctor extends Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Empty => Empty
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(left.map(f), right.map(f))
    }
  }

  given Functor[Tree] = TreeFunctor


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
