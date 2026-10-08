package okay.zero

import Cont0.*

/** WHAT A DELIMITER'S BODY ANSWERS IT, of the answer `V`: its answer; an operation of its row — at its path — with the
 * rest, `k`; or a capture up to it, a `shift`, with its body and the rest. A program of the delimiter's own level each
 * rest. Made and read in this package only */
sealed trait Res[R <: Row, J, V]
object Res:
  private[zero] final case class Done[R <: Row, J, V](v: V) extends Res[R, J, V]
  private[zero] final case class Req[R <: Row, J, V, E[_], X](op: E[X], in: In[E, R], k: X => Cont0[J, J, Res[R, J, V]])
    extends Res[R, J, V]
  private[zero] final case class Ctl[R <: Row, J, V, A, I](f: (A => Eff[R, J, V, V, I]) => Eff[R, J, V, V, V],
                                                           k: A => Cont0[J, J, Res[R, J, I]]) extends Res[R, J, V]

/** A LEVEL: the answer at a point of a body over `R`, its delimiter's outside at `J` — a program there, pure in its
 * answer, yielding the delimiter the body's answer `V`, or a request */
opaque type Level[R <: Row, J, V] = Cont0[J, J, Res[R, J, V]]

/** A PROGRAM over the row `R`, its nearest delimiter's outside at `J`: of value `A`, which, with a continuation
 * whose answer is `I`, answers `O`. The one type of both modules — Control's shift and reset, Effects' operations
 * and handlers — over the kernel, which nothing outside this package reaches */
opaque type Eff[R <: Row, J, I, O, A] = Cont0[Level[R, J, I], Level[R, J, O], A]

/** THE CONTEXT a program is written in, as master's: its row `R`, its nearest delimiter's outside `J`, the answer at
 * this point `T` — what an operation, a handler, a reset and a shift take their types from, by its members. The
 * nearest wins: a delimiter gives its body one of its own. A type, nothing at run time; made here only */
abstract class Ctx private[zero] ():
  type R <: Row
  type J
  type T
  /** a program here, pure in the answer */
  type P[A] = Eff[R, J, T, T, A]
  /** the level a delimiter here gives its body */
  type Inner = Level[R, J, T]
  /** the context under a handler of `E` here, at its answer `Ans` */
  type Handled[E[_], Ans] = At[E +: R, Inner, Ans]
  /** a handler's body here: in its context, of value `A`, at its answer `Ans` */
  type Body[E[_], Ans, A] = Handled[E, Ans] ?=> Eff[E +: R, Inner, Ans, Ans, A]
  /** the context in a reset here, its body written at `S` */
  type Reset[S] = InReset[R, J, T, S]
  /** this context at the answer `O`: a shift's body */
  type Answer[O] = At[R, J, O]

/** contexts made in this package: a library's own runner (`Async.runAsync`) re-entering at the top */
private[zero] object Ctxs:
  def at[R0 <: Row, J0, T0]: At[R0, J0, T0] = At()

/** a context at `R0`, `J0`, `T0` */
class At[R0 <: Row, J0, T0] private[zero] () extends Ctx:
  type R = R0
  type J = J0
  type T = T0

extension [R <: Row, J, I, O, A](p: Eff[R, J, I, O, A])
  def flatMap[I2, B](f: A => Eff[R, J, I2, I, B]): Eff[R, J, I2, O, B] = Bind(p, f)
  def map[B](f: A => B): Eff[R, J, I, O, B] = Bind(p, a => Return(f(a)))

def pure[R <: Row, J, T, A](a: A): Eff[R, J, T, T, A] = Return(a)

/** `delay(p)`: the program `p`, built when the loop gets to it — a tail call */
def delay[R <: Row, J, I, O, A](p: => Eff[R, J, I, O, A]): Eff[R, J, I, O, A] = Bind(Return[Level[R, J, O], Unit](()), _ => p)

/** `run(body)`: the body at the top, under a delimiter of its own — the top's: nothing to ask over `Empty`, a `shift`
 * up to it the body's — its answer its value */
def run[A](body: At[Empty, A, A] ?=> Eff[Empty, A, A, A, A]): A = top(Dollar(body(using At()), (a: A) => Return(Res.Done[Empty, A, A](a)))).value

private def top[A](out: Cont0[A, A, Res[Empty, A, A]]): Cont0[A, A, A] =
  Bind(out, (res: Res[Empty, A, A]) => res match
    case Res.Done(a) => Return[A, A](a)
    case c: Res.Ctl[r, j, v, a, i] => top(again(c)))

// THE DELIMITERS' PROTOCOL — what Control and Effects are written on, each a delimiter of its own. In this package only

/** `ret $ body`: the body, over `RB`, under a delimiter whose outside is over `R` at the answer `T` — its value
 * through `ret` to its answer `I` — the program outside, yielding the body's answers to it */
private[zero] def delimit[RB <: Row, R <: Row, J, T, A, I, O](body: Eff[RB, Level[R, J, T], I, O, A], ret: A => I)
    : Eff[R, J, T, T, Res[RB, Level[R, J, T], O]] =
  Dollar(body, (a: A) => Return(Res.Done[RB, Level[R, J, T], I](ret(a))))

/** what a delimiter does with an operation reaching it: `again` reads the rest's answers as it reads these */
private[zero] trait Requests[RB <: Row, R <: Row, J, T, V]:
  def apply[E[_], X](op: E[X], in: In[E, RB], k: X => Eff[R, J, T, T, Res[RB, Level[R, J, T], V]],
                     again: Eff[R, J, T, T, Res[RB, Level[R, J, T], V]] => Eff[R, J, T, T, V]): Eff[R, J, T, T, V]

/** A DELIMITER'S READING of its body's answers: its answer, its own; an operation, by `on`; a `shift` up to it —
 * Danvy–Filinski's, the same for every delimiter: the shift's body under this delimiter again, `k` the rest
 * resumed inside it */
private[zero] def read[RB <: Row, R <: Row, J, T, V](out: Eff[R, J, T, T, Res[RB, Level[R, J, T], V]], on: Requests[RB, R, J, T, V])
    : Eff[R, J, T, T, V] =
  Bind(out, (res: Res[RB, Level[R, J, T], V]) => res match
    case Res.Done(v) => Return(v)
    case r: Res.Req[rb, j, v, e, x] => on(r.op, r.in, r.k, read(_, on))
    case c: Res.Ctl[rb, j, v, a, i] => read(again(c), on))

/** a `shift`'s body under its delimiter again, its value the delimiter's answer, `k` the rest resumed inside */
private def again[RB <: Row, J, V, A, I](c: Res.Ctl[RB, J, V, A, I]): Cont0[J, J, Res[RB, J, V]] =
  Dollar(c.f(a => inside(c.k(a))), (v: V) => Return(Res.Done[RB, J, V](v)))

/** a rest, a program outside, resumed from inside the delimiter: run there, each operation it asks made again
 * inside — its path the same, the row is — and a `shift` up to the rest's own delimiter taken as the delimiter's */
private def inside[RB <: Row, J, V, I](s: Cont0[J, J, Res[RB, J, I]]): Eff[RB, J, V, V, I] =
  Bind(Shift0[J, J, Res[RB, J, V], Level[RB, J, V], Res[RB, J, I]](kk => Bind(s, kk)), (res: Res[RB, J, I]) => res match
    case Res.Done(i) => Return[Level[RB, J, V], I](i)
    case r: Res.Req[rb, j, i, e, x] => Bind(request[e, RB, J, V, x](r.op, r.in), (x: x) => inside[RB, J, V, I](r.k(x)))
    case c: Res.Ctl[rb, j, i, a, i2] => inside[RB, J, V, I](again(c)))

/** an operation of `E` at its path: a `shift0` to the nearest delimiter, its body the request */
private[zero] def request[E[_], R <: Row, J, T, X](op: E[X], in: In[E, R]): Eff[R, J, T, T, X] =
  Shift0[J, J, Res[R, J, T], Level[R, J, T], X](k => Return(Res.Req[R, J, T, E, X](op, in, k)))

/** a `shift` to the nearest delimiter, its body and the rest the request */
private[zero] def control[R <: Row, J, A, I, O](f: (A => Eff[R, J, O, O, I]) => Eff[R, J, O, O, O]): Eff[R, J, I, O, A] =
  Shift0[J, J, Res[R, J, I], Level[R, J, O], A](k => Return(Res.Ctl[R, J, O, A, I](f, k)))

/** a capture up to the nearest delimiter, its outside over `R` at `T`: `k` the rest under it again, yielding its
 * answers; the body in the delimiter's place — shift0's */
private[zero] def capture[R <: Row, J, T, A, I, O](f: (A => Eff[R, J, T, T, Res[R, Level[R, J, T], I]]) => Eff[R, J, T, T, Res[R, Level[R, J, T], O]])
    : Eff[R, Level[R, J, T], I, O, A] =
  Shift0[Level[R, J, T], Level[R, J, T], Res[R, Level[R, J, T], I], Level[R, Level[R, J, T], O], A](f)
