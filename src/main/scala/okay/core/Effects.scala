package okay.core

/** THE CONTEXT a program is written in: its monad, an indexed one — `M[R, I, O, A]` a program over the row `R` of
 * value `A` which, with a continuation answering `I`, answers `O` (Atkey's parameterised monad, the answer moved from
 * `O` to `I`) — the row `Effs` here and the answer `S` at this point: what an operation and a `shift` take their types
 * from. The context IS the instance: a program asks for one, `(using c: Effects)`, and is a value of `c.Prog`. A
 * delimiter gives its body one of its own: a handler's has the handler's effect in front of what the handler
 * leaves, `E +: c.Effs`, so the row is built by the handlers, outside in, and never named. The nearest context wins —
 * a nested scope's given is preferred. Its monad is `Inner`, the monad ONE LEVEL IN: `Cont`'s the same, `M`; a monad
 * whose answers are programs of the level outside (`okay.zero`'s) one level deeper */
abstract class Context:
  /** a program here, over the row `Effs`: of value `A`, with a continuation answering `I`, answering `O` */
  type M[I, O, A]
  type Effs <: Row
  type S
  /** the monad of the bodies of the delimiters here: a handler's, a reset's */
  type Inner[X <: Row, I, O, A]
  /** a program here, the answer kept: what most programs are */
  type Prog[A] = M[S, S, A]
  def pure[S2, A](a: A): M[S2, S2, A]
  /** `delay(m)`: the program `m`, built when it is run — a tail call */
  def delay[I, O, A](m: => M[I, O, A]): M[I, O, A]
  extension [I, O, A](m: M[I, O, A])
    def flatMap[I2, B](f: A => M[I2, I, B]): M[I2, O, B]
    def map[B](f: A => B): M[I, O, B]

/** EFFECTS over the monad: an operation at its path in the row, and a handler — a delimiter that answers the
 * operations of its effect; a reset is one that answers none. So effects are control with handlers: every context
 * with effects has `shift` and `reset`, and its delimiters' contexts are so too */
abstract class Effects extends Control:
  /** the context under a handler of `E` here, at its answer `S2`: the row `E +: R`, the monad `Inner` */
  type Handled[E[_], S2] <: Effects { type M[I, O, A] = Effects.this.Inner[E +: Effects.this.Effs, I, O, A]; type Effs = E +: Effects.this.Effs; type S = S2 }
  type Reset[S2] <: Effects { type M[I, O, A] = Effects.this.Inner[Effects.this.Effs, I, O, A]; type Effs = Effects.this.Effs; type S = S2 }
  type Answer[S2] <: Effects { type M[I, O, A] = Effects.this.M[I, O, A]; type Effs = Effects.this.Effs; type S = S2 }
  /** a handler of `E`'s body here, of value `A`, at the handler's answer `Ans`: in its context, one level in */
  type Body[E[_], Ans, A] = Handled[E, Ans] ?=> Inner[E +: Effs, Ans, Ans, A]
  def perform[E[_], S2, X](op: E[X], in: In[E, Effs]): M[S2, S2, X]
  /** a handler of `E` here: the body in its context, over `E +: R` at the answer `Ans`; its answers read by `h` */
  def handle[E[_], A, Ans](h: Handler[Prog, E, A, Ans])(body: Handled[E, Ans] ?=> Inner[E +: Effs, Ans, Ans, A]): M[S, S, Ans]
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

/** `A ! Es`: a program of value `A`, in any context whose row has every effect of `Es` — one, `Int ! Reader % Int`,
 * or a row of them, `Int ! Reader % Int +: Writer % String +: Pure`; their order the program's own, the context's
 * row any. At the context's answer, not `Free`'s `Unit`: under a handler the answer is the handler's
 * — `Either[E, A]` under `throws` — so a `Free` program would run only where nothing is answered */
infix type ![A, Es <: AnyKind] = (c: Needs[Es]) ?=> c.Prog[A]

object Effects:
  /** `handle(h)(body)` in context: the body written in the handler's context, over `E +: c.Effs`, at its answer */
  def handle[E[_], A, Ans](using c: Effects)(h: Handler[c.Prog, E, A, Ans])
            (body: c.Body[E, Ans, A]): c.Prog[Ans] =
    c.handle(h)(body)

/** a handler of the effect `E`, over the row `E +: T`, leaving `T`, written at the answer `Q` outside its
 * delimiter: the body's value `A` to the answer `Ans`; each operation with the rest of the body up to the
 * delimiter, `k`, deep — the handler stays in force through a resumption. Not a node: `handle` folds the tree
 * with it; an operation of another effect goes on outside, its path one shorter */

/** A HANDLER'S CLAUSES over the programs `P` outside it, whatever the monad: the body's value to `Ans`, each
 * operation with the rest — what a handler is to a monad that runs it. `Handler` is them over `M` at the row `T`
 * and the answer `Q`; a monad of its own (`okay.zero`'s) takes them as they are */
trait Handler[P[_], E[_], A, Ans]:
  def ret(a: A): Ans
  def apply[X](op: E[X], k: X => P[Ans]): P[Ans]

/** an operation's value, by the operation alone */
trait Value[E[_]]:
  def value[X](op: E[X]): X

/** clauses each `k(value(op))`: an operation answered in place, by its value alone */
trait Answering[P[_], E[_], A, Ans] extends Handler[P, E, A, Ans] with Value[E]:
  final def apply[X](op: E[X], k: X => P[Ans]): P[Ans] = k(value(op))

/** a TAIL-RESUMPTIVE handler: each clause is `k(value(op))`, the value of the operation alone — so the operation is
 * ANSWERED IN PLACE: the fold goes on with the rest of the body at once, in its own loop, no resumption built,
 * nothing handed out (Koka's, Effekt's optimisation; here by the handler's declaration) */

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
