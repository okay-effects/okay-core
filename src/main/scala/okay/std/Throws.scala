package okay.std

import okay.core.*

/** THROWS, an abort: `raise` is an operation whose clause never resumes — the delimiter answers `Left(e)` in
 * place of the body */
enum Throws[E, +A]:
  case Raise[E](e: E) extends Throws[E, Nothing]

/** the failure: a program of any value, which no continuation ever gets — `Raise` is a `Throws[E, A]` for any
 * `A`, `Throws` covariant, so no map from `Nothing` */
def raise[E, A](e: E)(using c: Effects, has: Throws % E :? c.Effs): c.Prog[A] =
  perform[Throws % E, A](Throws.Raise(e))
/** `throws[E, A](body)`: the value as `Right`, or the first `raise` as `Left` */
def throws[E, A](using c: Effects)(body: c.Body[Throws % E, Either[E, A], A])
    : c.Prog[Either[E, A]] =
  type Ans = Either[E, A]
  Effects.handle(new Handler[c.Prog, Throws % E, A, Ans]:
    def ret(a: A): Ans = Right(a)
    def apply[X](op: Throws[E, X], k: X => c.Prog[Ans]): c.Prog[Ans] = op match
      case Throws.Raise(e) => c.pure(Left(e)))(body)
/** `handleWith[E, A](body)(f)`: a `raise` of the body answered `f` of the error, the rest of the body dropped — the
 * throws intercepted, so an error `f` raises goes on outside */
def handleWith[E, A](using c: Effects, in: Throws % E :? c.Effs)(body: c.Intercepted[A, A])(f: E => c.Prog[A]): c.Prog[A] =
  Effects.intercept(new Handler[c.Prog, Throws % E, A, A]:
    def ret(a: A): A = a
    def apply[X](op: Throws[E, X], k: X => c.Prog[A]): c.Prog[A] = op match
      case Throws.Raise(e) => f(e))(body)
