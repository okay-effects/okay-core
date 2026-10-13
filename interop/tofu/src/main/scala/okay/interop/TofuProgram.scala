package okay.interop

import cats.Functor
import okay.core.*
import okay.std.*
import tofu.{Errors, WithLocal}

/** TOFU'S TYPECLASSES OVER `Eff[R, *]`: code written over `F[_]` with tofu runs on okay's effects. The context a
 * `Reader` anywhere in the row, `local` its interceptor; the errors a `Throws`, `handleWith` its interceptor — each by
 * `Intercept.at`, the effect handled again where it is in the row, the row kept. With `CatsProgram`'s monad:
 * `import okay.interop.CatsProgram.given, okay.interop.TofuProgram.given` */
object TofuProgram:
  import CatsProgram.Of

  given withLocal[R <: Row, C](using p: Reader % C :? R): WithLocal[Of[R], C] = new WithLocal[Of[R], C]:
    def functor: Functor[Of[R]] = CatsProgram.monad[R]
    def context: Eff[R, C] = okay.std.ask[C].at[R]
    /** the value outside asked once, every `ask` of `fa` answered `project` of it */
    def local[A](fa: Eff[R, A])(project: C => C): Eff[R, A] =
      okay.std.ask[C].at[R].flatMap(c => Intercept.at(p.in)(fa)([X] => (op: Reader[C, X], k: X => Eff[R, A]) => op match
        case Reader.Ask() => k(project(c))))

  given errors[R <: Row, E](using p: Throws % E :? R): Errors[Of[R], E] = new Errors[Of[R], E]:
    def raise[A](err: E): Eff[R, A] = okay.std.raise(err).at[R]
    /** a `raise` of `fa` answered `f` of the error, the rest dropped; `f`'s program outside the interceptor */
    def tryHandleWith[A](fa: Eff[R, A])(f: E => Option[Eff[R, A]]): Eff[R, A] =
      Intercept.at(p.in)(fa)([X] => (op: Throws[E, X], k: X => Eff[R, A]) => op match
        case Throws.Raise(e) => f(e).getOrElse(raise(e)))
    def restore[A](fa: Eff[R, A]): Eff[R, Option[A]] =
      Intercept.at(p.in)(fa.map(Some(_)))([X] => (op: Throws[E, X], k: X => Eff[R, Option[A]]) => op match
        case Throws.Raise(_) => pure(None))
    def lift[A](fa: Eff[R, A]): Eff[R, A] = fa
