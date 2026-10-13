package okay.core

import okay.one.Cont0
import okay.one.std.{Shift, Shifts}

/**
 * DELIMITED CONTROL, two faces, each where its typing is free (docs/one.md §2):
 *
 *  - AS AN EFFECT OF THE ROW, the answer fixed: `Shift % S` — `reset[S](body)` its handler, `Deep`; `shift`
 *    and `shift0` its operations, given `k`, the rest up to the reset — data, a program over the row outside the
 *    reset, `T`. Under any handlers, in any order; a shift directly under a handler holds the handler in its `k`.
 *    What okay's `shift`/`reset` are.
 *  - THE ANSWER MOVED, `Control[R]`: `shift0` and `$` with the answer type modified (Materzok–Biernacki, Asai–
 *    Kameyama's typing), the kernel's own nodes — a program of effects enters it by `cont`, a closed program of
 *    control is one of effects again by `lower` (`Cont0.reset`). For printf, `pstate`.
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

  /** a program of effects in control, at any answer */
  export Cont0.cont
  /** `lower(c)`: a closed program of control — its value its initial answer, the rest the identity — a program of
   * effects over `R`: its operations go out past its `$` to the handlers around */
  def lower[R <: Row, S, O](c: Cont0[okay.one.Control[R], S, O, S]): Eff[R, O] = Cont0.reset(c)
