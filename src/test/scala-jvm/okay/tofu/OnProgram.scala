package okay.tofu

import cats.{Functor, StackSafeMonad}
import okay.core.*
import okay.std.*
import tofu.{Errors, WithLocal}

/** TOFU'S TYPECLASSES OVER `Program[R, *]`: code written over `F[_]` runs on okay's effects, on any monad. The
 * context is a `Reader` in the row, `local` its interceptor; the errors a `Throws`, `handleWith` its interceptor —
 * each `fa` built again under it, which a `Program` is for. The monad stack safe as okay's monads are */
object OnProgram:
  given monad[R <: Row]: StackSafeMonad[Program.Of[R]] = new StackSafeMonad[Program.Of[R]]:
    def pure[A](a: A): Program[R, A] = Program.pure(a)
    def flatMap[A, B](fa: Program[R, A])(f: A => Program[R, B]): Program[R, B] = fa.flatMap(f)

  given withLocal[R <: Row, C](using in: Reader % C :? R): WithLocal[Program.Of[R], C] = new WithLocal[Program.Of[R], C]:
    def functor: Functor[Program.Of[R]] = monad[R]
    def context: Program[R, C] = Programs.ask[R, C]
    def local[A](fa: Program[R, A])(project: C => C): Program[R, A] = Programs.local(project)(fa)

  given errors[R <: Row, E](using in: Throws % E :? R): Errors[Program.Of[R], E] = new Errors[Program.Of[R], E]:
    def raise[A](err: E): Program[R, A] = Programs.raise(err)
    def tryHandleWith[A](fa: Program[R, A])(f: E => Option[Program[R, A]]): Program[R, A] =
      Programs.handleWith[R, E, A](fa)(e => f(e).getOrElse(Programs.raise(e)))
    def restore[A](fa: Program[R, A]): Program[R, Option[A]] =
      Programs.handleWith[R, E, Option[A]](fa.map(Some(_)))(_ => Program.pure(None))
    def lift[A](fa: Program[R, A]): Program[R, A] = fa
