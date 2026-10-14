package okay.core.std

import okay.core.*
import okay.one.handler.Aborting

/** THROWS, an abort: `raise` is an operation whose rest is dropped — the handler answers `Left(e)` in place of the
 * body, no `k` made */
export okay.one.std.Throws

/** the failure: a program of no value, which no continuation ever gets */
def raise[E](e: E): Op[Throws % E, Nothing] = perform(Throws.Raise(e))
/** `throws(body)`: the value as `Right`, or the first `raise` as `Left` */
def throws[E, A, T <: Row](body: Under[Throws % E, T, A]): Eff[T, Either[E, A]] = handleIn(Throws.either[E, T, A])(body)
/** `handleWith(body)(f)`: a `raise` of the body answered `f` of the error, the rest of the body dropped — the throws
 * intercepted: `f` runs outside, so an error it raises goes on outside */
def handleWith[E, A, T <: Row](body: Under[Throws % E, T, A])(f: E => Eff[T, A])(using Path[Throws % E, T]): Eff[T, A] =
  intercept(new Aborting[Throws % E, T, A, A]:
    def ret(a: A): Eff[T, A] = pure(a)
    def abort[X](op: Throws[E, X]): Eff[T, A] = op match
      case Throws.Raise(e) => f(e))(body)
