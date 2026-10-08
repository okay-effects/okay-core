package okay.zero

import Cont0.*

/** WHAT A DELIMITER'S BODY ANSWERS IT: its value, or an operation of its row — at its path — with the rest, `k`, a
 * program of the delimiter's own level. Made and read in this package only */
sealed trait Res[R <: Row, J, A]
object Res:
  private[zero] final case class Done[R <: Row, J, A](a: A) extends Res[R, J, A]
  private[zero] final case class Req[R <: Row, J, A, E[_], X](op: E[X], in: In[E, R], k: X => Cont0[J, J, Res[R, J, A]])
    extends Res[R, J, A]

/** A LEVEL: the answer at a point of a body over `R`, its delimiter's outside at `J` — a program there, pure in its
 * answer, yielding the delimiter the body's answer `T`, or an operation of the body's */
opaque type Level[R <: Row, J, T] = Cont0[J, J, Res[R, J, T]]

/** A PROGRAM over the row `R`, its nearest delimiter's outside at `J`: of value `A`, which, with a continuation
 * whose answer is `I`, answers `O`. The one type of both modules — Control's shift and reset, Effects' operations
 * and handlers — over the kernel, which nothing outside this package reaches */
opaque type Eff[R <: Row, J, I, O, A] = Cont0[Level[R, J, I], Level[R, J, O], A]

/** THE CONTEXT a program is written in: its row `R`, its nearest delimiter's outside `J`. A type, nothing at run
 * time: made by `run`, a handler and a reset only — its constructor this package's, so no subclass elsewhere */
class Ctx[R <: Row, J] private[zero] ()

extension [R <: Row, J, I, O, A](p: Eff[R, J, I, O, A])
  def flatMap[I2, B](f: A => Eff[R, J, I2, I, B]): Eff[R, J, I2, O, B] = Bind(p, f)
  def map[B](f: A => B): Eff[R, J, I, O, B] = Bind(p, a => Return(f(a)))

def pure[R <: Row, J, T, A](a: A): Eff[R, J, T, T, A] = Return(a)

/** `run(body)`: the body at the top under a delimiter of its own — nothing to ask over `Empty` — its answer its
 * value; the top outside it `identity $`, as the kernel's */
def run[A](body: Ctx[Empty, A] ?=> Eff[Empty, A, A, A, A]): A =
  Bind(Dollar(body(using Ctx()), (a: A) => Return(Res.Done[Empty, A, A](a))), (res: Res[Empty, A, A]) => res match
    case Res.Done(a) => Return[A, A](a)).value

// THE DELIMITERS' PROTOCOL — what Control and Effects are written on, each a delimiter of its own: a body under one,
// its answers read, an operation's way out, a capture. In this package only

/** `Done $ body`: the body, over `RB`, under a delimiter whose outside is over `R` at the answer `T` — the program
 * outside, yielding the body's answers to it */
private[zero] def delimit[RB <: Row, R <: Row, J, T, A, O](body: Eff[RB, Level[R, J, T], A, O, A]): Eff[R, J, T, T, Res[RB, Level[R, J, T], O]] =
  Dollar(body, (a: A) => Return(Res.Done[RB, Level[R, J, T], A](a)))

/** what a delimiter does with its body's answers */
private[zero] trait OnAnswers[RB <: Row, R <: Row, J, T, A, B]:
  def done(a: A): Eff[R, J, T, T, B]
  def request[E[_], X](op: E[X], in: In[E, RB], k: X => Eff[R, J, T, T, Res[RB, Level[R, J, T], A]]): Eff[R, J, T, T, B]

private[zero] def onAnswers[RB <: Row, R <: Row, J, T, A, B](out: Eff[R, J, T, T, Res[RB, Level[R, J, T], A]],
    on: OnAnswers[RB, R, J, T, A, B]): Eff[R, J, T, T, B] =
  Bind(out, (res: Res[RB, Level[R, J, T], A]) => res match
    case Res.Done(a) => on.done(a)
    case r: Res.Req[rb, j, a, e, x] => on.request(r.op, r.in, r.k))

/** an operation of `E` at its path: a `shift0` to the nearest delimiter, its body the request */
private[zero] def request[E[_], R <: Row, J, T, X](op: E[X], in: In[E, R]): Eff[R, J, T, T, X] =
  Shift0[J, J, Res[R, J, T], Level[R, J, T], X](k => Return(Res.Req[R, J, T, E, X](op, in, k)))

/** a capture up to the nearest delimiter, its outside over `R` at `T`: `k` the rest under it again, yielding its
 * answers; the body in the delimiter's place, its value the delimiter's answers */
private[zero] def capture[R <: Row, J, T, A, I, O](f: (A => Eff[R, J, T, T, Res[R, Level[R, J, T], I]]) => Eff[R, J, T, T, Res[R, Level[R, J, T], O]])
    : Eff[R, Level[R, J, T], I, O, A] =
  Shift0[Level[R, J, T], Level[R, J, T], Res[R, Level[R, J, T], I], Level[R, Level[R, J, T], O], A](f)
