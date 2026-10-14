package okay.one

import okay.one.handler.*
import okay.one.machine.Machine
import okay.one.row.*
import scala.compiletime.{error, summonFrom}

/** the row under a `$`, `T` outside it */
type Delim[T <: Row] = Control[T] +: T

/** a program of delimited control with no effect, under one `$`, its answer unmoved */
type Cps[L, A] = Cont[Delim[Pure], L, L, A]

/** a PROGRAM OF EFFECTS over the row `R`: no answers — its handlers' `k` its rest, data */
infix type ![+A, R <: Row] = Cont[R, Nothing, Nothing, A]

/** A PROGRAM OF `A` over the row `R`, data: `I` the answer of its rest, `O` what it answers — the state of the nearest
 * `Control` in `R`, the `$` it is under; none in `R`, inert. Its nodes:
 *
 *  - the monad's: `Return`, `Bind`, `Map`, `Defer`, `Raise`;
 *  - effects': an operation (`Perform`, `PerformHere`; with its rest, `Inject`, `InjectHere`), a handler around a
 *    program (`Handle`, `HandleState`, the answers through it; `HandleK`, the rest taken — a program of effects only;
 *    at any place in its row by `View`), a guard (`Guarded`), a `k` resumed (`Resume`, `Fail`), a program of effects
 *    at any answer (`Lift`);
 *  - control's: `Shift0`, up to the nearest `$`, the answer type modified; `Dollar`, the `$`, its answer a program run
 *    outside it; `Lower`, a `reset` — its answer a value.
 *
 * A handler's frames `k` and a `k` as data the machine's: where it has run a program, it is put back */
enum Cont[R <: Row, I, +O, +A]:
  case Return[R <: Row, S, A](a: A) extends Cont[R, S, S, A]
  case Bind[R <: Row, I, T, O, X, A](m: Cont[R, T, O, X], f: X => Cont[R, I, T, A]) extends Cont[R, I, O, A]
  case Map[R <: Row, I, O, X, A](m: Cont[R, I, O, X], f: X => A) extends Cont[R, I, O, A]
  /** `c` made when the run gets to it */
  case Defer[R <: Row, I, O, A](c: () => Cont[R, I, O, A]) extends Cont[R, I, O, A]
  /** AN ERROR RAISED, what user code threw: the frames of its level dropped, the levels unwound to the top — a node,
   * never an exception through the machine */
  case Raise[R <: Row, I, O, A] private[one] (e: Throwable) extends Cont[R, I, O, A]

  /** an operation of `E`, `in` its place in `R` — not the head */
  case Perform[R <: Row, E[_], S, X] private[one] (op: E[X], in: In[E, R]) extends Cont[R, S, S, X]
  /** an operation of the row's head: no place carried */
  case PerformHere[E[_], T <: Row, S, X] private[one] (op: E[X]) extends Cont[E +: T, S, S, X]
  /** an operation of `E`, `in` its place — not the head — `g` its rest */
  case Inject[R <: Row, E[_], Y, I, O, A] private[one] (op: E[Y], in: In[E, R], g: Y => Cont[R, I, O, A])
      extends Cont[R, I, O, A]
  /** an operation of the row's head, `g` its rest */
  case InjectHere[E[_], T <: Row, Y, I, O, A] private[one] (op: E[Y], g: Y => Cont[E +: T, I, O, A])
      extends Cont[E +: T, I, O, A]
  /** `body`, then its frames `k`, with the row's head `E` handled by `h` — the rest left where it is, the answers
   * through: `J` the rest's, `O` the body's */
  case Handle[E[_], T <: Row, X, I, A, J, O, B] private[one] (h: Passing[E, T, A, B],
      k: Machine.Frame[E +: T, X, I, A, J], body: Cont[E +: T, I, O, X]) extends Cont[T, J, O, B]
  /** `body`, then its frames `k`, with the row's head `E` handled by `h` from the state `s` */
  case HandleState[E[_], T <: Row, St, X, I, A, J, O, B] private[one] (h: Stateful[E, T, St, A, B], s: St,
      k: Machine.Frame[E +: T, X, I, A, J], body: Cont[E +: T, I, O, X]) extends Cont[T, J, O, B]
  /** `body`, then its frames `k`, with the row's head `E` handled by `h` — the rest taken: a program of effects */
  case HandleK[E[_], T <: Row, X, I, A, B] private[one] (h: Taking[E, T, A, B],
      k: Machine.Frame[E +: T, X, I, A, Nothing], body: Cont[E +: T, I, Nothing, X]) extends Cont[T, Nothing, Nothing, B]
  /** `body`, over `R`, then its frames `k`, SEEN over `E +: T` — `E` taken out of `R` by `rm`, put first: for a handler
   * of `E` at any place in the row, the handler at its head */
  case View[E[_], R <: Row, T <: Row, X, I, A, J, O] private[one] (rm: Removed[E, R, T],
      k: Machine.Frame[R, X, I, A, J], body: Cont[R, I, O, X]) extends Cont[E +: T, J, O, A]
  /** `body`, then its frames `k`, under the guard `g`: a level of no effect, the row the same in it */
  case Guarded[T <: Row, X, I, A, B] private[one] (g: Guard[T, A, B], k: Machine.Frame[T, X, I, A, Nothing],
      body: Cont[T, I, Nothing, X]) extends Cont[T, Nothing, Nothing, B]
  /** a `k` resumed with `y`: its levels put back, outermost first, then its rest with `y` under them */
  case Resume[Y, T <: Row, I, O, B] private[one] (c: Machine.Rest[Y, T, I, O, B], y: Y) extends Cont[T, I, O, B]
  /** a `k` failed with `e`: its levels put back, then `e` raised inside them */
  case Fail[T <: Row, I, O, B] private[one] (c: Machine.Rest[Nothing, T, I, O, B], e: Throwable) extends Cont[T, I, O, B]
  /** `p`, a program of effects, at any answer: none of its own to move */
  case Lift[R <: Row, S, A] private[one] (p: A ! R) extends Cont[R, S, S, A]

  /** shift0: `f` given `k`, the rest up to the nearest `$` in the row — `at` its path, the handlers before the `$` in
   * `k` — `T` the row outside the `$`, `k`'s; the answer turned from the rest's to what `f` makes */
  case Shift0[R <: Row, T <: Row, I1, O1, X1, O, A](at: Nearest[T, R], f: (A => Cont[T, I1, O1, X1]) => O)
      extends Cont[R, Cont[T, I1, O1, X1], O, A]
  /** `$`: `body` under it, its value through `ret`; its answer a program over the row outside, run there */
  case Dollar[T <: Row, I, I2, O2, B, A](body: Cont[Control[T] +: T, I, Cont[T, I2, O2, B], A], ret: A => I)
      extends Cont[T, I2, O2, B]
  /** `reset(c)`: `c` closed — its value its initial answer — under a `$` of its own, the rest the identity: a program of
   * effects over `R`, its answer `O` the value */
  case Lower[R <: Row, S, O] private[one] (c: Cont[Control[R] +: R, S, O, S]) extends Cont[R, Nothing, Nothing, O]

  /** `f` bound: at the answers met (`Lifts`) — the same, a bind; a program of effects, this or `f`'s, bound to one
   * moving the answer, at the answers of the other, lifted */
  inline def flatMap[I2, T, I3, O2, B](f: A => Cont[R, I2, T, B])(using inline l: Lifts[I, O, I2, T, I3, O2])
      : Cont[R, I3, O2, B] =
    inline l match
      case _: Lifts.Down[?, ?] => summonFrom {
        case ev: (I =:= Nothing) => down(f, ev)
        case _ => Bind(this, (x: A) => Lift[R, I, B](f(x))) }
      case _: Lifts.Same[?, ?, ?] => bound(f)
      case _: Lifts.Up[?, ?] => Bind(Lift[R, T, A](this), f)

  /** a program of effects bound to one: `bound`, at the answers none — one call at the site of a `flatMap`, the
   * evidence applied here, not inlined there: user code stays small, and the JIT's room for the loop with it */
  private[one] def down[B](f: A => Cont[R, Nothing, Nothing, B], ev: I =:= Nothing): Cont[R, I, O, B] =
    ev.flip.liftCo[[x] =>> Cont[R, x, O, B]](bound(ev.flip.liftCo[[x] =>> A => Cont[R, Nothing, x, B]](f)))

  /** `f` bound at the answers the same: on an operation, its rest */
  private[one] def bound[I2, B](f: A => Cont[R, I2, I, B]): Cont[R, I2, O, B] = this match
    case p: Perform[r, e, s, x] => Inject[r, e, x, I2, s, B](p.op, p.in, f)
    case p: PerformHere[e, t, s, x] => InjectHere[e, t, x, I2, s, B](p.op, f)
    case _ => Bind(this, f)
  def map[B](f: A => B): Cont[R, I, O, B] = this match
    case p: Perform[r, e, s, x] => Inject[r, e, x, I, O, B](p.op, p.in, (y: x) => Return[r, s, B](f(y)))
    case p: PerformHere[e, t, s, x] => InjectHere[e, t, x, I, O, B](p.op, (y: x) => Return[R, s, B](f(y)))
    case _ => Map(this, f)

/** THE ANSWERS MET at a bind, a program's `I`, `O` and its continuation's `I2`, `T`; those of the bind `I3`, `O2`:
 * the continuation a program of effects, no answer of its own — lifted to the program's (`Down`, first: a program of
 * effects bound to one the same); the same (`Same`); the program one of effects, the continuation moving the answer
 * — the program lifted to its answers (`Up`). Given */
enum Lifts[I, -O, I2, T, I3, +O2]:
  case Down[I, O]() extends Lifts[I, O, Nothing, Nothing, I, O]
  case Same[I, O, I2]() extends Lifts[I, O, I2, I, I2, O]
  case Up[I2, T]() extends Lifts[Nothing, Nothing, I2, T, I2, T]

object Lifts extends LiftsSame:
  /** inline: decided where `flatMap` is written, no value made at run time */
  inline given down[I, O]: Lifts.Down[I, O] = Down()

trait LiftsSame extends LiftsUp:
  inline given same[I, O, I2]: Lifts.Same[I, O, I2] = Lifts.Same()

trait LiftsUp:
  inline given up[I2, T]: Lifts.Up[I2, T] = Lifts.Up()

object Cont:
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

  /** the programs over the row `R`, named: `rows[R].perform(op)`, `rows[R].shift0[X](k => …)` */
  def rows[R <: Row]: Rows[R] = Rows()
  final class Rows[R <: Row]:
    inline def perform[E[_], X](op: E[X])(using inline in: In[E, R]): X ! R = operation(op)
    /** shift0 up to the nearest `$` in `R`, of `A`: its rest a program of effects over `T`, the row outside the `$`,
     * of `X`; its body one too — the row found, the rest inferred: `rows[R].shift0[X, A](k => …)` */
    def shift0[X, A]: Shift0Of[R, X, A] = Shift0Of()
  final class Shift0Of[R <: Row, X, A]:
    def apply[T <: Row, O](using at: Nearest[T, R])(f: (A => X ! T) => O ! T): Cont[R, X ! T, O ! T, A] =
      Shift0(at, f)

  /** shift0 up to the nearest `$` in `R`, `T` the row outside it: `f` given `k`, the rest — the handlers between in it
   * — a program over `T`; the answer turned from the rest's to what `f` makes */
  def shift0[T <: Row, I1, O1, X1, O, A, R <: Row](f: (A => Cont[T, I1, O1, X1]) => O)(using at: Nearest[T, R])
      : Cont[R, Cont[T, I1, O1, X1], O, A] = Shift0(at, f)

  /** `$`: `body` delimited, its value through `ret`; its answer a program over `T`, run outside */
  def dollar[T <: Row, I, I2, O2, B, A](body: Cont[Control[T] +: T, I, Cont[T, I2, O2, B], A])(ret: A => I)
      : Cont[T, I2, O2, B] = Dollar(body, ret)

  /** `reset(body)`: `body` closed — its value its initial answer, the rest the identity — under a `$`: a program of
   * effects over `R`, its answer `O` the value. Its operations not answered in it go out, past the `$`, to the
   * handlers around; a handler's `k` taken past it holds the `$`, put back when resumed */
  def reset[R <: Row, S, O](body: Cont[Control[R] +: R, S, O, S]): O ! R = Lower(body)

  extension [R <: Row, I, O, A](p: Cont[R, I, O, A])
    /** `p` with its effect `E` handled by `h` — any place in the row, `T` the row without it, the row `h` is over. `T`
     * with no bound here, `& Row` where it is used: so it is found by `Removed`, not fixed to its bound first. By the
     * kind of `h` where it is written: one leaving the rest — `Passing`, `Stateful` — at any answers, a `$` around, a
     * shift0 under it in its `k`; one taking the rest — `Taking` — only on a program of effects, its answers none; a
     * kind not known there, the same, by a type test */
    inline def handle[E[_], T, B, H <: Handler[E, T & Row, A, B]](h: H)(using rm: Removed[E, R, T & Row])
        : Cont[T & Row, I, O, B] =
      summonFrom {
        case ev: (H <:< Passing[E, T & Row, A, B]) => Handle(ev(h), Machine.Frame.End(), view(rm, p))
        case ev: (H <:< Stateful[E, T & Row, ?, A, B]) => stateful(ev(h), rm, p)
        case ev: (H <:< Taking[E, T & Row, A, B]) => summonFrom {
          case ni: (I =:= Nothing) => summonFrom {
            case no: (O <:< Nothing) =>
              ni.flip.liftCo[[x] =>> Cont[T & Row, x, Nothing, B]](HandleK(ev(h), Machine.Frame.End(), view(rm, effects(p, ni, no))))
            case _ => error("a handler taking the rest: only on a program of effects, its answers none") }
          case _ => error("a handler taking the rest: only on a program of effects, its answers none") }
        case _ => summonFrom {
          case ni: (I =:= Nothing) => summonFrom {
            case no: (O <:< Nothing) => ni.flip.liftCo[[x] =>> Cont[T & Row, x, Nothing, B]](kinds(h, rm, effects(p, ni, no)))
            case _ => error("a handler of a kind not known where it is written: only on a program of effects") }
          case _ => error("a handler of a kind not known where it is written: only on a program of effects") }
      }

  /** a handler with a state around `p`, its state's type its own */
  private def stateful[E[_], R <: Row, T <: Row, I, O, A, B](h: Stateful[E, T, ?, A, B], rm: Removed[E, R, T],
      p: Cont[R, I, O, A]): Cont[T, I, O, B] = h match
    case st: Stateful[E, T, s, A, B] => HandleState(st, st.initial, Machine.Frame.End(), view(rm, p))

  /** `p` a program of effects, its answers none */
  private[one] def effects[R <: Row, I, O, A](p: Cont[R, I, O, A], ni: I =:= Nothing, no: O <:< Nothing): A ! R =
    no.liftCo[[x] =>> Cont[R, Nothing, x, A]](ni.liftCo[[x] =>> Cont[R, x, O, A]](p))

  /** a handler of a kind not known where it is written, around `p`: by a type test */
  private def kinds[E[_], R <: Row, T <: Row, A, B](h: Handler[E, T, A, B], rm: Removed[E, R, T], p: A ! R): B ! T =
    h match
      case ps: Passing[E, T, A, B] => Handle(ps, Machine.Frame.End(), view(rm, p))
      case st: Stateful[E, T, ?, A, B] => stateful(st, rm, p)
      case tk: Taking[E, T, A, B] => HandleK(tk, Machine.Frame.End(), view(rm, p))

  extension [R <: Row, A](p: A ! R)
    /** `p` under the guard `g` */
    def guard[B](g: Guard[R, A, B]): B ! R = Guarded(g, Machine.Frame.End(), p)
    /** `fin` after `p`, whatever way it is left: an end, an error, an abort, a cancel */
    def ensure(fin: Unit ! R): A ! R = guard(Guard.ensuring(fin))
    /** an error thrown in `p` given to `f` */
    def recover(f: Throwable => A ! R): A ! R = guard(Guard.recover(f))
    /** a cancel at a step of `p` kept for when it is left */
    def uncancelable: A ! R = guard(Guard.uncancelable)

    /** `p` at any answer `S`: to put under a `$`, among programs moving the answer — none of its own to move */
    def cont[S]: Cont[R, S, S, A] = Lift(p)

  /** `p` seen with `E` first: itself, if it is there already */
  private def view[E[_], R <: Row, T <: Row, I, O, A](rm: Removed[E, R, T], p: Cont[R, I, O, A])
      : Cont[E +: T, I, O, A] = rm match
    case _: Removed.Head[e, t] => p
    case _ => View(rm, Machine.Frame.End(), p)

  /** `p`, a program over `T`, seen over `E +: T`: its operations past the head */
  def widen[E[_], T <: Row, A](p: A ! T): A ! (E +: T) = View(Removed.Absent[E, T](), Machine.Frame.End(), p)

  extension [A](p: A ! Pure)
    /** `p`, its effects all handled: its value */
    def run: A = Machine.run(p)

  extension [I, O, A](c: Cont[Control[Pure] +: Pure, I, O, A])
    /** a program of control with no effect run, its rest `k`: its answer */
    def run(k: A => I): O = Machine.run(Lower(c.map(k)))
