package okay.zero.std

import okay.zero.*

/** THROWS, an abort: `raise` is an operation whose clause never resumes — the handler answers `Left(e)` in place of
 * the body */
enum Throws[E, +A]:
  case Raise[E](e: E) extends Throws[E, Nothing]

/** the failure: a program of any value, which no continuation ever gets */
def raise[E, A](e: E)(using c: Ctx, has: Throws % E :? c.R): c.P[A] = perform[Throws % E, A](Throws.Raise(e))
/** `throws[E, A](body)`: the value as `Right`, or the first `raise` as `Left` */
def throws[E, A](using c: Ctx)(body: c.Body[Throws % E, Either[E, A], A]): c.P[Either[E, A]] =
  handle(new Handler[c.P, Throws % E, A, Either[E, A]]:
    def ret(a: A): Either[E, A] = Right(a)
    def apply[X](op: Throws[E, X], k: X => c.P[Either[E, A]]): c.P[Either[E, A]] = op match
      case Throws.Raise(e) => pure(Left(e)))(body)
