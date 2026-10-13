package okay.one

import okay.one.handler.*
import okay.one.machine.Machine
import okay.one.row.*

/** DELIMITED CONTROL over the row `R`: shift0 and `$`, and the effects of `R` — a program of effects over `R` put in
 * it by `cont`, its operations going out past the `$`s to their handlers, around the `reset` that makes the program of
 * control one of effects again. Not a row of effects: a program over one never meets a shift0 or a `$` */
sealed trait Control[R <: Row]

/** a program of delimited control with no effect, its answer unmoved */
type Cont[L, A] = Cont0[Control[Pure], L, L, A]

/** a PROGRAM OF EFFECTS over the row `R`: no answers — its handlers' `k` its rest, data */
infix type ![+A, R <: Row] = Cont0[R, Nothing, Nothing, A]

/** A PROGRAM OF `A` over the row `R`, data: `I` the answer of its rest, `O` what it answers. Its nodes:
 *
 *  - the monad's, over any row: `Return`, `Bind`, `Map`, `Defer`;
 *  - effects', over a row of them: an operation (`Perform`, `PerformHere`; with its rest, `Inject`, `InjectHere`), a
 *    handler around a program (`Handle`, `HandleState`, at any place in its row by `View`), a `k` resumed (`Resume`)
 *    — the answers through them unmoved, none of the machine's;
 *  - delimited control's, over `Control[R]`: `Shift0`, `Dollar` — the answer type modified; `Lift`, a program of
 *    effects over `R` in it; `Continue`, a shift0's `k` resumed; and back, over `R`: `Lower`, a `reset` — a closed
 *    program of control a program of effects.
 *
 * A handler's frames `k` and a `k` as data the machine's: where it has run a program, it is put back */
enum Cont0[R, I, +O, +A]:
  case Return[R, S, A](a: A) extends Cont0[R, S, S, A]
  case Bind[R, I, T, O, X, A](m: Cont0[R, T, O, X], f: X => Cont0[R, I, T, A]) extends Cont0[R, I, O, A]
  case Map[R, I, O, X, A](m: Cont0[R, I, O, X], f: X => A) extends Cont0[R, I, O, A]
  /** `c` made when the run gets to it */
  case Defer[R, I, O, A](c: () => Cont0[R, I, O, A]) extends Cont0[R, I, O, A]
  /** AN ERROR RAISED, what user code threw: the frames of its level dropped, the levels unwound to the top — a node,
   * never an exception through the machine */
  case Raise[R, I, O, A] private[one] (e: Throwable) extends Cont0[R, I, O, A]

  /** an operation of `E`, `in` its place in `R` — not the head */
  case Perform[R <: Row, E[_], S, X] private[one] (op: E[X], in: In[E, R]) extends Cont0[R, S, S, X]
  /** an operation of the row's head: no place carried */
  case PerformHere[E[_], T <: Row, S, X] private[one] (op: E[X]) extends Cont0[E +: T, S, S, X]
  /** an operation of `E`, `in` its place — not the head — `g` its rest */
  case Inject[R <: Row, E[_], Y, I, O, A] private[one] (op: E[Y], in: In[E, R], g: Y => Cont0[R, I, O, A])
      extends Cont0[R, I, O, A]
  /** an operation of the row's head, `g` its rest */
  case InjectHere[E[_], T <: Row, Y, I, O, A] private[one] (op: E[Y], g: Y => Cont0[E +: T, I, O, A])
      extends Cont0[E +: T, I, O, A]
  /** `body`, then its frames `k`, with the row's head `E` handled by `h` */
  case Handle[E[_], T <: Row, S, X, A, B] private[one] (h: Stateless[E, T, A, B], k: Machine.Frame[E +: T, X, A],
      body: Cont0[E +: T, ?, ?, X]) extends Cont0[T, S, S, B]
  /** `body`, then its frames `k`, with the row's head `E` handled by `h` from the state `s` */
  case HandleState[E[_], T <: Row, St, S, X, A, B] private[one] (h: Stateful[E, T, St, A, B], s: St,
      k: Machine.Frame[E +: T, X, A], body: Cont0[E +: T, ?, ?, X]) extends Cont0[T, S, S, B]
  /** `body`, over `R`, then its frames `k`, SEEN over `E +: T` — `E` taken out of `R` by `rm`, put first: for a handler
   * of `E` at any place in the row, the handler at its head */
  case View[E[_], R <: Row, T <: Row, S, X, A] private[one] (rm: Removed[E, R, T], k: Machine.Frame[R, X, A],
      body: Cont0[R, ?, ?, X]) extends Cont0[E +: T, S, S, A]
  /** `body`, then its frames `k`, under the guard `g`: a level of no effect, the row the same in it */
  case Guarded[T <: Row, S, X, A, B] private[one] (g: Guard[T, A, B], k: Machine.Frame[T, X, A],
      body: Cont0[T, ?, ?, X]) extends Cont0[T, S, S, B]
  /** a `k` resumed with `y`: its levels put back, outermost first, then its rest with `y` under them */
  case Resume[Y, T <: Row, S, B] private[one] (c: Machine.Cont[Y, T, B], y: Y) extends Cont0[T, S, S, B]
  /** a `k` failed with `e`: its levels put back, then `e` raised inside them */
  case Fail[T <: Row, S, B] private[one] (c: Machine.Cont[Nothing, T, B], e: Throwable) extends Cont0[T, S, S, B]

  case Shift0[R <: Row, I1, O1, X1, O, A](f: (A => Cont0[Control[R], I1, O1, X1]) => O)
      extends Cont0[Control[R], Cont0[Control[R], I1, O1, X1], O, A]
  case Dollar[R <: Row, I, I2, O2, B, A](body: Cont0[Control[R], I, Cont0[Control[R], I2, O2, B], A], ret: A => I)
      extends Cont0[Control[R], I2, O2, B]
  /** `p`, a program of effects over `R`, in control: its operations answered in place, by the handlers around */
  case Lift[R <: Row, I0, O0, S, A] private[one] (p: Cont0[R, I0, O0, A]) extends Cont0[Control[R], S, S, A]
  /** a shift0's `k` resumed with `a`: its frames a level of their own under the place it is resumed at */
  case Continue[R <: Row, A, I, O, B] private[one] (k: Machine.CFrame[R, A, Cont0[Control[R], I, O, B]], a: A)
      extends Cont0[Control[R], I, O, B]
  /** a handler's `k` resumed in control: its levels with control outside them put back, then its rest */
  case ResumeC[Y, R <: Row, I, O, A] private[one] (c: Machine.ContC[Y, R, I, O, A], y: Y)
      extends Cont0[Control[R], I, O, A]
  /** a handler's `k` failed in control: its levels put back, then the error raised inside them */
  case FailC[R <: Row, I, O, A] private[one] (c: Machine.ContC[Nothing, R, I, O, A], e: Throwable)
      extends Cont0[Control[R], I, O, A]
  /** `reset(c)`: `c`, a closed program of control — its value its initial answer — under a `$` of its own, the rest
   * the identity: a program of effects over `R`, its answer `O` the value */
  case Lower[R <: Row, S, O, S2] private[one] (c: Cont0[Control[R], S, O, S]) extends Cont0[R, S2, S2, O]

  def flatMap[I2, B](f: A => Cont0[R, I2, I, B]): Cont0[R, I2, O, B] = this match
    case p: Perform[r, e, s, x] => Inject[r, e, x, I2, s, B](p.op, p.in, f)
    case p: PerformHere[e, t, s, x] => InjectHere[e, t, x, I2, s, B](p.op, f)
    case _ => Bind(this, f)
  def map[B](f: A => B): Cont0[R, I, O, B] = this match
    case p: Perform[r, e, s, x] => Inject[r, e, x, I, O, B](p.op, p.in, (y: x) => Return[r, s, B](f(y)))
    case p: PerformHere[e, t, s, x] => InjectHere[e, t, x, I, O, B](p.op, (y: x) => Return[R, s, B](f(y)))
    case _ => Map(this, f)

object Cont0:
  /** `a`, a program of effects over any row */
  def pure[R <: Row, A](a: A): A ! R = Return(a)

  /** `p`, made when the run gets to it */
  def defer[R <: Row, A](p: => A ! R): A ! R = Defer(() => p)

  /** `op`, an operation of `E` in the row `R` */
  inline def perform[E[_], R <: Row, X](op: E[X])(using inline in: In[E, R]): X ! R = operation(op)

  /** of the row's head, no place carried: known where it is written, or asked of the place */
  private inline def operation[E[_], R <: Row, X](op: E[X])(using inline in: In[E, R]): X ! R =
    inline in match
      case _: In.Here[E, t] => PerformHere[E, t, Nothing, X](op)
      case _ => placed(op, in)

  private def placed[E[_], R <: Row, X](op: E[X], in: In[E, R]): X ! R = in match
    case _: In.Here[e, t] => PerformHere[e, t, Nothing, X](op)
    case _ => Perform(op, in)

  /** the programs over the row `R`, named: `rows[R].perform(op)` */
  def rows[R <: Row]: Rows[R] = Rows()
  final class Rows[R <: Row]:
    inline def perform[E[_], X](op: E[X])(using inline in: In[E, R]): X ! R = operation(op)

  extension [R <: Row, I0, O0, A](p: Cont0[R, I0, O0, A])
    /** `p` with its effect `E` handled by `h` — any place in the row, `T` the row without it, the row `h` is over.
     * `T` with no bound here, `& Row` where it is used: so it is found by `Removed`, not fixed to its bound first */
    def handle[E[_], T, B](h: Handler[E, T & Row, A, B])(using rm: Removed[E, R, T & Row]): B ! (T & Row) =
      h match
        case h: Stateless[E, T & Row, A, B] => Handle(h, Machine.Frame.End(), view(rm, p))
        case h: Stateful[E, T & Row, s, A, B] => HandleState(h, h.initial, Machine.Frame.End(), view(rm, p))

    /** `p` under the guard `g` */
    def guard[B](g: Guard[R, A, B]): B ! R = Guarded(g, Machine.Frame.End(), p)
    /** `fin` after `p`, whatever way it is left: an end, an error, an abort, a cancel */
    def ensure(fin: Unit ! R): A ! R = guard(Guard.ensuring(fin))
    /** an error thrown in `p` given to `f` */
    def recover(f: Throwable => A ! R): A ! R = guard(Guard.recover(f))
    /** a cancel at a step of `p` kept for when it is left */
    def uncancelable: A ! R = guard(Guard.uncancelable)

    /** `p` a program of control at any answer: to put under its `$`s and `shift0`s, its effects handled around it in
     * place */
    def cont[S]: Cont0[Control[R], S, S, A] = Lift(p)

  /** `p` seen with `E` first: itself, if it is there already */
  private def view[E[_], R <: Row, T <: Row, I0, O0, A](rm: Removed[E, R, T], p: Cont0[R, I0, O0, A])
      : Cont0[E +: T, ?, ?, A] = rm match
    case _: Removed.Head[e, t] => p
    case _ => View[E, R, T, Nothing, A, A](rm, Machine.Frame.End(), p)

  /** `p`, a program over `T`, seen over `E +: T`: its operations past the head */
  def widen[E[_], T <: Row, A](p: A ! T): A ! (E +: T) = View(Removed.Absent[E, T](), Machine.Frame.End(), p)

  /** `reset(body)`: a closed program of control over `R` — its value its initial answer, the rest the identity — a
   * program of effects over `R`: its operations not answered in it go out, past its `$`, to the handlers around it;
   * a handler's `k` taken past it holds the `$`, put back when resumed */
  def reset[R <: Row, S, O](body: Cont0[Control[R], S, O, S]): O ! R = Lower(body)

  extension [I0, O0, A](p: Cont0[Pure, I0, O0, A])
    /** `p`, its effects all handled: its value */
    def run: A = Machine.run(p)

  extension [I, O, A](c: Cont0[Control[Pure], I, O, A])
    /** a program of control with no effect run, its rest `k`: its answer */
    def run(k: A => I): O = Machine.control(c, k)
