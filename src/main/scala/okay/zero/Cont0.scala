package okay.zero

import scala.annotation.tailrec

/**
 * THE MONAD OF shift0 AND $, answers modified — Materzok–Biernacki's. `Cont0[I, O, A]`: a program of value `A`
 * which, with a continuation answering `I`, answers `O`, `(A => I) => O`. Under a delimiter the answers are programs
 * of the level outside — the levels nested in the answer types.
 *
 *  - `ret $ body` (`Dollar`): the body's value through `ret` to its answer `I`, a program outside; the body's final
 *    answer the delimiter's.
 *  - `shift0(k => e)`: `k` the rest up to the nearest delimiter and the delimiter — `λx. ret $ K[x]`; `e` runs in
 *    the delimiter's place, outside it.
 *
 * Four nodes, open: a monad of its own, raw shift0 and `$`, the levels named in the types. The rest are this
 * package's, each a shift0 program in one node, for speed — a map, a tail call, an operation (a shift0 whose body
 * is its request), a `shift` (the same, a control request), a handler and a reset (a `$` whose answers are read
 * there), a resumption — and run by the machine (Eff.scala), each delimiter in a loop of its own. `step` runs any
 * of them to its head, the machine's by their shift0 programs, in constant stack.
 */
sealed abstract class Cont0[I, O, A]:
  final def flatMap[I2, B](f: A => Cont0[I2, I, B]): Cont0[I2, O, B] = Cont0.bind(this, f)
  final def map[B](f: A => B): Cont0[I, O, B] = Cont0.Map(this, f)

/** the answer at a point of a body over `R`, its delimiter's outside at `J`: a program there, yielding the body's
 * answers — its value or a request, `Res` */
private[zero] type Lv[R <: Row, J, T] = Cont0[J, J, Res[R, J, T]]

object Cont0:
  final case class Return[S, A](a: A) extends Cont0[S, S, A]
  final case class Bind[I, T, O, A, B](m: Cont0[T, O, A], f: A => Cont0[I, T, B]) extends Cont0[I, O, B]
  final case class Shift0[I1, O1, X1, O, A](f: (A => Cont0[I1, O1, X1]) => O) extends Cont0[Cont0[I1, O1, X1], O, A]
  final case class Dollar[I, I2, O2, B, A](body: Cont0[I, Cont0[I2, O2, B], A], ret: A => I) extends Cont0[I2, O2, B]

  /** `m.map(f)`: `Bind(m, a => Return(f(a)))` */
  private[zero] final case class Map[I, O, A, B](m: Cont0[I, O, A], f: A => B) extends Cont0[I, O, B]
  /** a program built when the loop gets to it */
  private[zero] final case class Delay[I, O, A](c: () => Cont0[I, O, A]) extends Cont0[I, O, A]
  /** an operation of `E` at its path, at the answer `T`: `shift0(k => Return(Req(op, in, k)))` */
  private[zero] final case class Op[R <: Row, J, T, E[_], X](op: E[X], in: In[E, R]) extends Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], X]
  /** an operation with the rest bound to its value: `Bind(Op(op, in), k)` */
  private[zero] final case class OpK[R <: Row, J, T, E[_], X, I, B](op: E[X], in: In[E, R], k: X => Cont0[I, Cont0[J, J, Res[R, J, T]], B])
    extends Cont0[I, Cont0[J, J, Res[R, J, T]], B]
  /** a `shift` to the nearest delimiter: `shift0(k => Return(Ctl(f, k)))` */
  private[zero] final case class Ctl[R <: Row, J, A, I, O](f: (A => Cont0[Cont0[J, J, Res[R, J, O]], Cont0[J, J, Res[R, J, O]], I]) => Cont0[Cont0[J, J, Res[R, J, O]], Cont0[J, J, Res[R, J, O]], O])
    extends Cont0[Cont0[J, J, Res[R, J, I]], Cont0[J, J, Res[R, J, O]], A]
  /** a handler of `E`: `Done∘h.ret $ body`, its answers read by `h` — the delimiter its loop answers to, itself */
  private[zero] final case class Handle[E[_], R <: Row, J, T, A, Ans](body: Cont0[Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[E +: R, Cont0[J, J, Res[R, J, T]], Ans]], Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[E +: R, Cont0[J, J, Res[R, J, T]], Ans]], A],
      h: Handler[[X] =>> Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], X], E, A, Ans]) extends Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Ans]
      with Delim[E +: R, Cont0[J, J, Res[R, J, T]], Ans, Ans, A]
  /** a reset: `Done $ body`, every operation in it passed on */
  private[zero] final case class Reset[R <: Row, J, T, S, O](body: Cont0[Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], S]], Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], O]], S])
    extends Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], O]
    with Delim[R, Cont0[J, J, Res[R, J, T]], S, O, S]
  /** a captured rest `k` applied to `x`: the rest under its delimiter `d` again, the handlers in force those where the
   * node runs — `ev`, those of when it was captured, kept if the ones outside are the same */
  private[zero] final case class Resume[RB <: Row, L, I0, V, A, X](x: X, k: X => Cont0[Cont0[L, L, Res[RB, L, I0]], Cont0[L, L, Res[RB, L, V]], A],
      d: Delim[RB, L, I0, V, A], ev: Ev[RB]) extends Cont0[L, L, V]

  /** `c.flatMap(f)`: an operation bound one node, `OpK` */
  private[zero] def bind[I, O, A, I2, B](c: Cont0[I, O, A], f: A => Cont0[I2, I, B]): Cont0[I2, O, B] = c match
    case o: Op[r, j, t, e, x] => OpK[r, j, t, e, x, I2, B](o.op, o.in, f)
    case _ => Bind(c, f)

  def pure[S, A](a: A): Cont0[S, S, A] = Return(a)
  def shift0[I1, O1, X1, O, A](f: (A => Cont0[I1, O1, X1]) => O): Cont0[Cont0[I1, O1, X1], O, A] = Shift0(f)
  def dollar[I, I2, O2, B, A](ret: A => I)(body: Cont0[I, Cont0[I2, O2, B], A]): Cont0[I2, O2, B] = Dollar(body, ret)

  extension [J, I2, O2, B, A](body: Cont0[Cont0[J, J, A], Cont0[I2, O2, B], A])
    /** `pure $ body` */
    def reset0: Cont0[I2, O2, B] = Dollar(body, Return(_))

  /** the program to its head: binds followed, a delimiter entered — the machine's nodes by their shift0 programs,
   * a handler, a reset and a resumption by the machine, nothing known of the handlers around */
  @tailrec private[zero] def step[I, T, O, X, A](c: Cont0[T, O, X], k: Frames[X, I, T, A]): Head[I, O, A] = c match
    case b: Bind[i, t, o, a, bb] => b.m match
      case r: Return[s, aa] => step(b.f(r.a), k)
      case _ => step(b.m, Frames.Then(b.f, k))
    case Return(a) => k match
      case Frames.End() => Head.Done(a)
      case Frames.Then(f, next) => step(f(a), next)
      case Frames.Mapped(f, next) => step(Return(f(a)), next)
    case Map(m, f) => step(m, Frames.Mapped(f, k))
    case Delay(t) => step(t(), k)
    case o: Op[r, j, t, e, x] => Head.Cut[I, j, j, Res[r, j, t], Cont0[j, j, Res[r, j, t]], x, A](kk => Return(Res.Req(o.op, o.in, kk)), k)
    case o: OpK[r, j, t, e, x, i, b] => Head.Cut[I, j, j, Res[r, j, t], Cont0[j, j, Res[r, j, t]], x, A](kk => Return(Res.Req(o.op, o.in, kk)), Frames.Then(o.k, k))
    case c: Ctl[r, j, a, i, o] => Head.Cut[I, j, j, Res[r, j, i], Cont0[j, j, Res[r, j, o]], a, A](kk => Return(Res.Ctl(c.f, kk)), k)
    case h: Handle[e, r, j, t, a, ans] => step(enterHandle(h, Ev.Unknown), k)
    case s: Reset[r, j, t, sv, o] => step(enterReset(s, Ev.Unknown), k)
    case r: Resume[rb, l, i0, v, a, x] => step(enterUnknown(r), k)
    case Shift0(f) => Head.Cut(f, k)
    case Dollar(body, ret) => step(delimit(body, ret), k)

  /** `ret $ body` to the program outside: a value through `ret`; a capture's body in the delimiter's place, `k` the
   * rest under the delimiter again */
  private def delimit[I, I2, O2, B, A](body: Cont0[I, Cont0[I2, O2, B], A], ret: A => I): Cont0[I2, O2, B] =
    step(body, Frames.End()) match
      case Head.Done(a) => ret(a)
      case Head.Cut(f, k) => f(x => Dollar(resumed(x, k), ret))

  /** `x` into the rest `k` — LAZILY: a bind the loop takes when it gets to it, so applying `k` runs nothing of the
   * rest; an empty rest, the value itself */
  private[zero] def resumed[I, T, X, A](x: X, k: Frames[X, I, T, A]): Cont0[I, T, A] = k match
    case Frames.End() => Return(x)
    case _ => Bind(Return(x), k)

  extension [A](c: Cont0[A, A, A])
    /** a program at the top, its value — the top `identity $` */
    def value: A = step(c, Frames.End()) match
      case Head.Done(a) => a
      case Head.Cut(f, k) => f(x => Dollar(resumed(x, k), identity))

/** A DELIMITER the machine runs, its own node: a handler of `E` (its body over `E +: R`, the outside over `R` at `T`,
 * at the answer `Ans`), or a reset (its body over `R`, written at `S`) */
private[zero] sealed trait Delim[RB <: Row, L, I0, V, A]

/** what a value goes through, innermost first; as a function, the rest */
private[zero] enum Frames[A, I, T, B] extends (A => Cont0[I, T, B]):
  case End[S, A]() extends Frames[A, S, S, A]
  case Then[A, X, I, T1, T, B](f: A => Cont0[T1, T, X], next: Frames[X, I, T1, B]) extends Frames[A, I, T, B]
  case Mapped[A, X, I, T, B](f: A => X, next: Frames[X, I, T, B]) extends Frames[A, I, T, B]

  def apply(a: A): Cont0[I, T, B] = this match
    case End() => Cont0.Return(a)
    // the rest bound after the first frame — none, the first frame's program itself: an empty stack bound as a
    // frame would be one more each resumption, nested
    case Then(f, next) => next match
      case End() => f(a)
      case _ => Cont0.Bind(f(a), next)
    case Mapped(f, next) => next match
      case End() => Cont0.Return(f(a))
      case _ => Cont0.Bind(Cont0.Return(f(a)), next)

/** a program's head: its value, or a capture with the rest */
private[zero] enum Head[I, O, A]:
  case Done[S, A](a: A) extends Head[S, S, A]
  case Cut[I, I1, O1, X1, O, X, A](f: (X => Cont0[I1, O1, X1]) => O, k: Frames[X, I, Cont0[I1, O1, X1], A]) extends Head[I, O, A]
