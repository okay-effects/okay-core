package okay.interop

import cats.Functor
import okay.core.*
import okay.std.*
import tofu.{Errors, WithLocal}

/** TOFU'S TYPECLASSES OVER `Program[R, *]`: code written over `F[_]` with tofu runs on okay's effects, on any monad.
 * The context a `Reader` in the row, `local` its interceptor; the errors a `Throws`, `handleWith` its interceptor —
 * each `fa` built again under it, which a `Program` is for. With `CatsProgram`'s monad:
 * `import okay.interop.CatsProgram.given, okay.interop.TofuProgram.given` */
object TofuProgram:
  given withLocal[R <: Row, C](using in: Reader % C :? R): WithLocal[Program.Of[R], C] = new WithLocal[Program.Of[R], C]:
    def functor: Functor[Program.Of[R]] = CatsProgram.monad[R]
    def context: Program[R, C] = Programs.ask[R, C]
    def local[A](fa: Program[R, A])(project: C => C): Program[R, A] = Programs.local(project)(fa)

  given errors[R <: Row, E](using in: Throws % E :? R): Errors[Program.Of[R], E] = new Errors[Program.Of[R], E]:
    def raise[A](err: E): Program[R, A] = Programs.raise(err)
    def tryHandleWith[A](fa: Program[R, A])(f: E => Option[Program[R, A]]): Program[R, A] =
      Programs.handleWith[R, E, A](fa)(e => f(e).getOrElse(Programs.raise(e)))
    def restore[A](fa: Program[R, A]): Program[R, Option[A]] =
      Programs.handleWith[R, E, Option[A]](fa.map(Some(_)))(_ => Program.pure(None))
    def lift[A](fa: Program[R, A]): Program[R, A] = fa
