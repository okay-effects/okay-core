package okay.core

import okay.one.{Cont, Lifts}
import okay.one.std.{Shift, Shifts}

/**
 * DELIMITED CONTROL, two faces, each where its typing is free (docs/one.md §2):
 *
 *  - AS AN EFFECT OF THE ROW, the answer fixed: `Shift % S` — `reset[S](body)` its handler, `Deep`; `shift`
 *    and `shift0` its operations, given `k`, the rest up to the reset — data, a program over the row outside the
 *    reset, `T`. Under any handlers, in any order; a shift directly under a handler holds the handler in its `k`.
 *    What okay's `shift`/`reset` are.
 *  - THE ANSWER MOVED, under a `$`: `shift0` and `$` with the answer type modified (Materzok–Biernacki, Asai–
 *    Kameyama's typing), the kernel's own nodes — `Control[T]` the effect of a `$` in the row, `Delim[T]` the row
 *    under it; a program of effects enters it by `cont`, a closed one under a `$` is one of effects again by `lower`
 *    (`Cont.reset`). For printf, `pstate`.
 */
object Control:
  /** `reset[S](body)`: the shifts in the body up to here, each with the rest as `k`; the body's value the answer,
   * `S`. `T` the row outside the reset — the clauses of its shifts programs over it */
  def reset[S, T <: Row](body: Under[Shifts[S, T], T, S]): Eff[T, S] = handleIn(Shift.reset[S, T])(body)

  /** `shift[S, T, X](k => body)`: `k` the rest up to the reset, the reset included; the body runs under the reset
   * again — a shift in it captures up to the same one (Danvy–Filinski's) */
  def shift[S, T <: Row, X](f: (X => Eff[Shifts[S, T] +: T, S]) => Eff[Shifts[S, T] +: T, S]): Op[Shifts[S, T], X] =
    Op(Shift.Up(f))

  /** `shift0[S, T, X](k => body)`: the same, the body run outside the reset, in its place */
  def shift0[S, T <: Row, X](f: (X => Eff[T, S]) => Eff[T, S]): Op[Shifts[S, T], X] = Op(Shift.Up0(f))

  /** a program of effects at any answer: under a `$`, among shift0s */
  export Cont.cont
  /** the row under a `$` over `T`: `Control[T] +: T` */
  type Delim[T <: Row] = okay.one.Delim[T]
  /** `lower(c)`: a closed program of control under a `$` — its value its initial answer, the rest the identity — a
   * program of effects over `R`: its operations go out past the `$` to the handlers around */
  def lower[R <: Row, S, O](c: Cont[Delim[R], S, O, S]): Eff[R, O] = Cont.reset(c)

/** THE ANSWER MOVED, THE ROW NOT NAMED: `Atm[A, X, Y, Es]` written as `A ! Es` is — `shift0[X, A](k => …)` to the
 * nearest `$`, `dollar(body)(ret)` the `$`, the handlers inside it by `handleAt` */
object Atm:
  /** `shift0[X, A](k => …)`: up to the nearest `$`, `k` the rest, a program of `X` over the row outside; the body
   * what the `$` answers, a program over it too */
  final class Shift0Of[X, A]:
    def apply[Y, Es[_]](using d: Delimited[Es])(f: (A => Eff[d.T, X]) => Eff[d.T, Y]): Cont[d.R, Eff[d.T, X], Eff[d.T, Y], A] =
      Cont.Shift0(d.at, f)
  def shift0[X, A]: Shift0Of[X, A] = Shift0Of()

  /** `dollar(body)(ret)`: the `$` — the body at the place under it, `T` the row outside; its value through `ret`
   * to the rest's answer; the `$`'s answer a program over `T`, the `$` one of effects there */
  def dollar[T <: Row, A, X, Y](body: Place[Control.Delim[T]] ?=> Cont[Control.Delim[T], Eff[T, X], Eff[T, Y], A])(ret: A => Eff[T, X])
      : Eff[T, Y] = Cont.dollar(body(using Place.at[Control.Delim[T]]))(ret)

  // -- THE STATE IN THE ANSWER: its type moved by the steps (the kernel's `PState`), the row never named

  /** the run's value, `F` — given to the steps by `pstate`, each takes it `using`: `def open[F](using
   * Atm.Typestate[F]): Atm[Unit, Hole[Open, F], Hole[Closed, F], Es]` — a step from the state `Closed` to `Open` */
  final class Typestate[F] private[Atm] ()

  /** `get[S]`: the state, its type `S` unchanged. A step not yet at its `$`: bound where it is used, as an `Op` is */
  final class Get[S]:
    def at[F, Es[_]](using ts: Typestate[F], d: Delimited[Es]): Cont[d.R, Eff[d.T, S => Eff[d.T, F]], Eff[d.T, S => Eff[d.T, F]], S] =
      Cont.Shift0(d.at, (k: S => Eff[d.T, S => Eff[d.T, F]]) => Cont.pure((s: S) => k(s).flatMap(f => f(s))))
    inline def flatMap[F, Es[_], I2, T, I3, O2, B](using ts: Typestate[F], d: Delimited[Es])(f: S => Cont[d.R, I2, T, B])
        (using inline l: Lifts[Eff[d.T, S => Eff[d.T, F]], Eff[d.T, S => Eff[d.T, F]], I2, T, I3, O2]): Cont[d.R, I3, O2, B] =
      at[F, Es].flatMap(f)(using l)
    def map[F, Es[_], B](using ts: Typestate[F], d: Delimited[Es])(f: S => B)
        : Cont[d.R, Eff[d.T, S => Eff[d.T, F]], Eff[d.T, S => Eff[d.T, F]], B] = at[F, Es].map(f)
  object Get:
    /** a step where a program is expected: at its `$` there */
    given toProgram[S, F, Es[_]](using ts: Typestate[F], d: Delimited[Es])
        : Conversion[Get[S], Cont[d.R, Eff[d.T, S => Eff[d.T, F]], Eff[d.T, S => Eff[d.T, F]], S]] = _.at[F, Es]
  def get[S]: Get[S] = Get()

  /** `put[S](s2)`: a state of another type, `S2`, for the rest; the state before, `S` — the answer moved */
  final class Put[S, S2](val s2: S2):
    def at[F, Es[_]](using ts: Typestate[F], d: Delimited[Es]): Cont[d.R, Eff[d.T, S2 => Eff[d.T, F]], Eff[d.T, S => Eff[d.T, F]], Unit] =
      Cont.Shift0(d.at, (k: Unit => Eff[d.T, S2 => Eff[d.T, F]]) => Cont.pure((_: S) => k(()).flatMap(f => f(s2))))
    inline def flatMap[F, Es[_], I2, T, I3, O2, B](using ts: Typestate[F], d: Delimited[Es])(f: Unit => Cont[d.R, I2, T, B])
        (using inline l: Lifts[Eff[d.T, S2 => Eff[d.T, F]], Eff[d.T, S => Eff[d.T, F]], I2, T, I3, O2]): Cont[d.R, I3, O2, B] =
      at[F, Es].flatMap(f)(using l)
    def map[F, Es[_], B](using ts: Typestate[F], d: Delimited[Es])(f: Unit => B)
        : Cont[d.R, Eff[d.T, S2 => Eff[d.T, F]], Eff[d.T, S => Eff[d.T, F]], B] = at[F, Es].map(f)
  object Put:
    given toProgram[S, S2, F, Es[_]](using ts: Typestate[F], d: Delimited[Es])
        : Conversion[Put[S, S2], Cont[d.R, Eff[d.T, S2 => Eff[d.T, F]], Eff[d.T, S => Eff[d.T, F]], Unit]] = _.at[F, Es]
  final class PutFrom[S]:
    def apply[S2](s2: S2): Put[S, S2] = Put(s2)
  def put[S]: PutFrom[S] = PutFrom()

  /** `pstate[Es](s0)(body)`: the body run from the state `s0`, of type `S0`, to one of type `S1` — the steps' answers
   * meeting, or the compiler says which do not; the last state and the value, a program needing the effects `Es`,
   * named here once (inferred from the body, scalac 3.9.0 dies in `wildApprox` on the open `Es`). The `$` is the
   * run's own: the body's steps shift0 to it, its other operations go out to the handlers around */
  final class PStateOf[Es[_]]:
    def apply[S0, S1, W](s0: S0)(body: Typestate[(S1, W)] ?=> Atm[W, Hole[S1, (S1, W)], Hole[S0, (S1, W)], Es])
        : (S1, W) ! Es = (c: Needs[Es]) ?=>
      val d = Delimited.under(c)
      Cont.dollar[c.R, Eff[c.R, S1 => Eff[c.R, (S1, W)]], Nothing, Nothing, S0 => Eff[c.R, (S1, W)], W](
        body(using Typestate[(S1, W)]())(using d))((w: W) => Cont.pure((s1: S1) => Cont.pure((s1, w)))).flatMap(f => f(s0))
  def pstate[Es[_]]: PStateOf[Es] = PStateOf()
