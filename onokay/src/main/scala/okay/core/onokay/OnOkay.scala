package okay.core.onokay

import okay.cont.{Member, Pure as OkayPure, Row as OkayRow}
import okay.core.Eff
import okay.one.Cont
import okay.one.row.In

/**
 * OKAY'S `Effects[M]` ON THE ONE MONAD, over okay's own vocabulary of rows — a MAPPING of rows, no cast: a program
 * over okay's row `R` is the kernel's program over `ToOne[R]`, the same effects in the same order as the kernel's
 * row (`ToOne`, a match type: `Pure` to `Pure`, `E +: T` to `E +: ToOne[T]`); okay's path `Member[E, R]` is the
 * kernel's `In[E, ToOne[R]]` by the same walk (`in`). okay's words — `A ! R`, `effect`, `perform`, `value` — stay
 * okay's, aliases over this instance; nothing of okay's rows is renamed.
 *
 * WHAT THE MAPPING COSTS: one trusted step (`trust`, below) — match types do not reduce under a GADT bound, so the
 * walk from `Member` to `In`, typed on each concrete shape, is given to the abstract row by hand.
 *
 * WHAT STOPS HERE (2026-10-14), to be given by okay itself: `handle`. okay's `Removed[E, R]` is a trait whose
 * `Out` is a type member and whose witness of `E`'s place is `route`, a function on an operation with its path —
 * nothing a type can read; the kernel's handler at a place wants `Removed[E, ToOne[R], ToOne[Out]]`, a value by the
 * place (`Head`, `Next`), which cannot be built from `route` without a cast. And a `Handler`'s clauses are
 * programs of okay-cont's own `Cont` (`k: X => Cont[o.Here, o.Here, Ans]`): a machine of the one monad cannot run
 * them; only an `Answering` (`value(op)`, no continuation) is expressible. So: okay's `Removed` to carry the place
 * (`Member[E, R]` with `Out` by it — the kernel's `Removed` enum, in okay's vocabulary), and okay's `Handler` to
 * write its clauses over `Effects[M]`'s `M` — the two changes in okay that make this instance whole.
 */
object OnOkay:
  /** okay's row as the kernel's: the same list */
  type ToOne[R <: OkayRow] <: okay.one.row.Row = R match
    case OkayPure => okay.one.row.Pure
    case okay.cont.+:[e, t] => okay.one.row.+:[e, ToOne[t]]

  /** a program over okay's row `R`: the kernel's over `ToOne[R]` */
  type P[R <: OkayRow, A] = Eff[ToOne[R], A]

  /** THE ONE TRUSTED STEP of the mapping: `ToOne[E +: T]` is `E +: ToOne[T]` by the match type's own second case,
   * but the compiler does not reduce `ToOne[R]` where `R` is known to be `E +: T` only by a match on a value
   * (a GADT bound: match types do not reduce under it) — so the walk below, typed case by case on the concrete
   * shape, is given to the abstract `R` here, and nowhere else. As okay-kernel's bridge has `trust` for the
   * protocol's values, this has one for the rows */
  private def trust[T](x: Any): T = x.asInstanceOf[T]

  /** okay's path, the kernel's: the same walk — `Head` to `Here`, `Tail` to `There` */
  def in[E[+_], R <: OkayRow](m: Member[E, R]): In[E, ToOne[R]] = m match
    case _: Member.Head[e, t] =>
      val here: In[e, ToOne[okay.cont.+:[e, t]]] = In.Here[e, ToOne[t]]()
      trust[In[E, ToOne[R]]](here)
    case tl: Member.Tail[e, e2, t] =>
      val there: In[e, ToOne[okay.cont.+:[e2, t]]] = In.There[e, e2, ToOne[t]](in(tl.m))
      trust[In[E, ToOne[R]]](there)

  given effects: okay.Effects[P] with
    def pure[R <: OkayRow, A](a: A): P[R, A] = Cont.pure(a)
    def perform[E[+_], R <: OkayRow, X](op: E[X])(using m: Member[E, R]): P[R, X] =
      Cont.perform[E, ToOne[R], X](op)(using in(m))
    def defer[R <: OkayRow, A, B](thunk: () => P[R, A])(f: A => P[R, B]): P[R, B] = Cont.defer(thunk()).flatMap(f)
    extension [R <: OkayRow, A](m: P[R, A])
      def flatMap[B](f: A => P[R, B]): P[R, B] = m.flatMap(f)
    def handle[E[+_], A, Ans, R <: OkayRow](h: okay.cont.Handler[E, A, Ans])(m: P[R, A])(using rm: okay.cont.Removed[E, R])
        : P[rm.Out, Ans] =
      throw UnsupportedOperationException(
        "okay's handle on the one monad: okay's Removed names no place and a Handler's clauses are okay-cont's Cont — see OnOkay")
    def run[A](m: P[OkayPure, A]): A = m.run
