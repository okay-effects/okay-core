package okay.core

/** EFFECTS over the monad: an operation at its path in the row, and a handler — a delimiter that answers the
 * operations of its effect; a reset is one that answers none. So effects are control with handlers: every context
 * with effects has `shift` and `reset`, and its delimiters' contexts are so too */
abstract class Effects extends Control:
  /** the monad of the body of a handler of `E` here: over the row `E +: Effs`, one level in */
  type Handling[E[_], I, O, A]
  /** the context under a handler of `E` here, at its answer `S2`: the row `E +: R`, the monad `Handling` */
  type Handled[E[_], S2] <: Effects { type M[I, O, A] = Effects.this.Handling[E, I, O, A]; type Effs = E +: Effects.this.Effs; type S = S2 }
  type Reset[S2] <: Effects { type M[I, O, A] = Effects.this.Within[I, O, A]; type Effs = Effects.this.Effs; type S = S2 }
  type Answer[S2] <: Effects { type M[I, O, A] = Effects.this.M[I, O, A]; type Effs = Effects.this.Effs; type S = S2 }
  /** a handler of `E`'s body here, of value `A`, at the handler's answer `Ans`: in its context, one level in */
  type Body[E[_], Ans, A] = Handled[E, Ans] ?=> Handling[E, Ans, Ans, A]
  def perform[E[_], S2, X](op: E[X], in: In[E, Effs]): M[S2, S2, X]
  /** a handler of `E` here: the body in its context, over `E +: R` at the answer `Ans`; its answers read by `h` */
  def handle[E[_], A, Ans](h: Handler[Prog, E, A, Ans])(body: Handled[E, Ans] ?=> Handling[E, Ans, Ans, A]): M[S, S, Ans]
  /** an INTERCEPTOR of `E`, an effect already in this row at `in`: the body in a context of its own, the row the
   * same, at the answer `Ans` — its operations at `in` to `h`'s clauses, `k` the rest under the interceptor again
   * (deep), every other operation on outside as it is; a clause runs outside it, so an operation of `E` there is the
   * handler's at `in`. A delimiter, as a reset: a `shift` in the body reaches it. What `local` and `handleWith` are
   * (tofu's `WithLocal`, `Handle`), and a handler over an effect anywhere in a row */
  def intercept[E[_], A, Ans](in: In[E, Effs], h: Handler[Prog, E, A, Ans])(body: Reset[Ans] ?=> Within[Ans, Ans, A]): M[S, S, Ans]
  /** this context with the paths of the effects a program needs: the context of an `A ! Es` */
  type Needing[Es <: AnyKind] <: Needs[Es] { type M[I, O, A] = Effects.this.M[I, O, A]; type Effs = Effects.this.Effs; type S = Effects.this.S }
  def needing[Es <: AnyKind](p: Paths[Es, Effs]): Needing[Es]
  /** this monad's top, for a library re-entering it (`Async`): a run there on the same monad */
  def top: Top

/** THE CONTEXT WITH THE EFFECTS A PROGRAM NEEDS, `Es`, found in its row: a context of the instance's own making,
 * `c.needing(paths)` — for `Cont`, an `At` itself, so an operation is the context's own call, nothing forwarded */
trait Needs[Es <: AnyKind] extends Effects:
  def find[E[_]](r: Req[E, Es]): In[E, Effs]
object Needs:
  given all[Es <: AnyKind](using c: Effects, p: Paths[Es, c.Effs])
    : (Needs[Es] { type M[I, O, A] = c.M[I, O, A]; type Effs = c.Effs; type S = c.S }) = c.needing(p)
  /** an effect needed: its path in the context's row */
  given found[E[_], Es <: AnyKind](using n: Needs[Es], r: Req[E, Es]): In[E, n.Effs] = n.find(r)

/** `A ! E1 + E2 + …`: a program of value `A`, in any context whose row has every effect it names — one,
 * `Int ! Reader % Int`, or several joined, `Int ! Reader % Int + Writer % String` — in any order, among any others;
 * written under any handlers, the row never named. At the context's answer, not `Free`'s `Unit`: under a handler the
 * answer is the handler's — `Either[E, A]` under `throws` — so a `Free` program would run only where nothing is
 * answered */
infix type ![A, F[_]] = (c: Needs[F]) ?=> c.Prog[A]

object Effects:
  /** `handle(h)(body)` in context: the body written in the handler's context, over `E +: c.Effs`, at its answer */
  def handle[E[_], A, Ans](using c: Effects)(h: Handler[c.Prog, E, A, Ans])
            (body: c.Body[E, Ans, A]): c.Prog[Ans] =
    c.handle(h)(body)
  /** `intercept(h)(body)` in context: `E`, already in the context's row, handled again for the body */
  def intercept[E[_], A, Ans](using c: Effects, in: In[E, c.Effs])(h: Handler[c.Prog, E, A, Ans])
               (body: c.Reset[Ans] ?=> c.Within[Ans, Ans, A]): c.Prog[Ans] =
    c.intercept(in, h)(body)

/** A HANDLER of the effect `E`, its clauses over the programs `P` outside it, whatever the monad: the body's value
 * `A` to the answer `Ans`; each operation with the rest of the body up to the delimiter, `k`, deep — the handler
 * stays in force through a resumption. An operation of another effect goes on outside, its path one shorter */
trait Handler[P[_], E[_], A, Ans]:
  def ret(a: A): Ans
  def apply[X](op: E[X], k: X => P[Ans]): P[Ans]

/** an operation's value, by the operation alone */
trait Value[E[_]]:
  def value[X](op: E[X]): X

/** a TAIL-RESUMPTIVE handler: each clause is `k(value(op))`, the value of the operation alone — so the operation is
 * ANSWERED IN PLACE: the run goes on with the rest of the body at once, no resumption built, nothing handed out
 * (Koka's, Effekt's optimisation; here by the handler's declaration) */
trait Answering[P[_], E[_], A, Ans] extends Handler[P, E, A, Ans] with Value[E]:
  final def apply[X](op: E[X], k: X => P[Ans]): P[Ans] = k(value(op))

/** `perform(op)`: an operation of `E`, in the context — its row has `E`, its answer is the operation's */
def perform[E[_], X](op: E[X])(using c: Effects, in: In[E, c.Effs]): c.Prog[X] = c.perform(op, in)

/** `pure(a)`: the value, a program here */
def pure[A](a: A)(using c: Context): c.Prog[A] = c.pure(a)

/** A MONAD'S TOP: its context there, over `Pure` at an answer `J`, and a run of a program of it — what a library
 * re-entering the top takes (`Async`, a run going on in a callback's thread; `stream`, a run of a generator); every
 * context has its monad's, `c.top`. `M[J, I, O, A]`: the monad at the top answering `J` */
trait Top:
  type M[J, I, O, A]
  def at[J]: Effects { type M[I, O, A] = Top.this.M[J, I, O, A]; type Effs = Pure; type S = J }
  def run[J](p: M[J, J, J, J]): J
