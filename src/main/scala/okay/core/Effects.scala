package okay.core


/** AN INDEXED MONAD, `M[R, I, O, A]`: a program over the row `R` of value `A` which, with a continuation answering
 * `I`, answers `O` — Atkey's parameterised monad, the answer moved from `O` to `I`. What a context's programs are
 * values of; `Cont` is one, a test's CPS functions another. Final tagless: a program is written against the
 * operations, not the tree */
abstract class IxMonad:
  type M[R <: Row, I, O, A]
  def pure[R <: Row, S, A](a: A): M[R, S, S, A]
  /** `delay(m)`: the program `m`, built when it is run — a tail call */
  def delay[R <: Row, I, O, A](m: => M[R, I, O, A]): M[R, I, O, A]
  extension [R <: Row, I, O, A](m: M[R, I, O, A])
    def flatMap[I2, B](f: A => M[R, I2, I, B]): M[R, I2, O, B]
    def map[B](f: A => B): M[R, I, O, B]

/** THE CONTEXT a program is written in: the monad, the row `R` there and the answer `S` at this point — what an
 * operation and a `shift` take their types from, by its members: `c.M`, `c.Effs`, `c.S`. The context IS the
 * instance: a program asks for one, `(using c: Effects)`, and is a value of `c.M`. A delimiter gives its body
 * one of its own: a handler's has the handler's effect in front of what the handler leaves, `E +: c.Effs`, so the row
 * is built by the handlers, outside in, and never named. The nearest context wins — a nested scope's given is
 * preferred. Its monad is `Inner`, the monad ONE LEVEL IN: `Cont`'s the same, `M`; a monad whose answers are
 * programs of the level outside (`okay.zero`'s) one level deeper */
abstract class Context extends IxMonad:
  type Effs <: Row
  type S
  /** the monad of the bodies of the delimiters here: a handler's, a reset's */
  type Inner[X <: Row, I, O, A]
  /** a program here, the answer kept: what most programs are */
  type Prog[A] = M[Effs, S, S, A]

/** EFFECTS over the monad: an operation at its path in the row, and a handler — a delimiter that answers the
 * operations of its effect; a reset is one that answers none. So effects are control with handlers: every context
 * with effects has `shift` and `reset`, and its delimiters' contexts are so too */
abstract class Effects extends Control:
  /** the context under a handler of `E` here, at its answer `S2`: the row `E +: R`, the monad `Inner` */
  type Handled[E[_], S2] <: Effects { type M[X <: Row, I, O, A] = Effects.this.Inner[X, I, O, A]; type Effs = E +: Effects.this.Effs; type S = S2 }
  type Reset[S2] <: Effects { type M[X <: Row, I, O, A] = Effects.this.Inner[X, I, O, A]; type Effs = Effects.this.Effs; type S = S2 }
  type Answer[S2] <: Effects { type M[X <: Row, I, O, A] = Effects.this.M[X, I, O, A]; type Effs = Effects.this.Effs; type S = S2 }
  /** a handler of `E`'s body here, of value `A`, at the handler's answer `Ans`: in its context, one level in */
  type Body[E[_], Ans, A] = Handled[E, Ans] ?=> Inner[E +: Effs, Ans, Ans, A]
  def perform[E[_], R2 <: Row, S2, X](op: E[X], in: In[E, R2]): M[R2, S2, S2, X]
  /** a handler of `E` here: the body in its context, over `E +: R` at the answer `Ans`; its answers read by `h` */
  def handle[E[_], A, Ans](h: Handler[Prog, E, A, Ans])(body: Handled[E, Ans] ?=> Inner[E +: Effs, Ans, Ans, A]): M[Effs, S, S, Ans]
  /** this context with the paths of the effects a program needs: the context of an `A ! Es` */
  type Needing[Es <: AnyKind] <: Needs[Es] { type M[X <: Row, I, O, A] = Effects.this.M[X, I, O, A]; type Effs = Effects.this.Effs; type S = Effects.this.S }
  def needing[Es <: AnyKind](p: Paths[Es, Effs]): Needing[Es]

/** THE CONTEXT WITH THE EFFECTS A PROGRAM NEEDS, `Es`, found in its row: a context of the instance's own making,
 * `c.needing(paths)` — for `Cont`, an `At` itself, so an operation is the context's own call, nothing forwarded */
trait Needs[Es <: AnyKind] extends Effects:
  def find[E[_]](r: Req[E, Es]): In[E, Effs]
object Needs:
  given all[Es <: AnyKind](using c: Effects, p: Paths[Es, c.Effs])
    : (Needs[Es] { type M[X <: Row, I, O, A] = c.M[X, I, O, A]; type Effs = c.Effs; type S = c.S }) = c.needing(p)
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
