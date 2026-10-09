package okay.scala2

import cats.{Functor, StackSafeMonad}
import okay.core._
import okay.std._
import tofu.{Errors, WithLocal}

/** TOFU'S TYPECLASSES OVER `Program[R, *]`, WRITTEN IN SCALA 2 against tofu's 2.13 artifacts: what a Scala 2 user
 * of okay-core with tofu writes once. Effects of two parameters as Scala 2's type lambdas */
object OnProgram {
  type Of[R <: Row] = { type L[A] = Program[R, A] }
  type ReaderOf[C] = { type L[X] = Reader[C, X] }
  type ThrowsOf[E] = { type L[X] = Throws[E, X] }

  implicit def monad[R <: Row]: StackSafeMonad[Of[R]#L] = new StackSafeMonad[Of[R]#L] {
    def pure[A](a: A): Program[R, A] = Program.pure(a)
    def flatMap[A, B](fa: Program[R, A])(f: A => Program[R, B]): Program[R, B] = fa.flatMap(f)
  }

  implicit def withLocal[R <: Row, C](implicit in: In[ReaderOf[C]#L, R]): WithLocal[Of[R]#L, C] = new WithLocal[Of[R]#L, C] {
    def functor: Functor[Of[R]#L] = monad[R]
    def context: Program[R, C] = Programs.ask[R, C]
    def local[A](fa: Program[R, A])(project: C => C): Program[R, A] = Programs.local[R, C, A](project)(fa)
  }

  implicit def errors[R <: Row, E](implicit in: In[ThrowsOf[E]#L, R]): Errors[Of[R]#L, E] = new Errors[Of[R]#L, E] {
    def raise[A](err: E): Program[R, A] = Programs.raise[R, E, A](err)
    def tryHandleWith[A](fa: Program[R, A])(f: E => Option[Program[R, A]]): Program[R, A] =
      Programs.handleWith[R, E, A](fa)(e => f(e).getOrElse(Programs.raise[R, E, A](e)))
    def restore[A](fa: Program[R, A]): Program[R, Option[A]] =
      Programs.handleWith[R, E, Option[A]](fa.map(a => Option(a)))(_ => Program.pure[R, Option[A]](None))
    def lift[A](fa: Program[R, A]): Program[R, A] = fa
  }
}
