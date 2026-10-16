package okay.one.handler

import okay.one.{!, Cont}
import okay.one.row.*

/** A HANDLER of `E` over a program of `A`, answering `B`; `T` the row outside it — its `ret` and its clauses programs
 * over `T`, `k` too: the rest under it again, data, resumed any number of times, anywhere `T` is handled. Five forms,
 * values, by what of the machine each needs — and by what they may be put around: `Passing` and `Stateful` leave the
 * rest where it is — answered in place, or `k` resumed once — so the answers of the program pass through them,
 * whatever they are: a `$` around them, a shift0 under them in its `k`; `Taking` take the rest — dropped, or given as `k` — so only a
 * program of effects, with no answer to drop. The forms are classes, not traits: the machine tells them apart by a
 * type test, a class's cheap */
sealed abstract class Handler[E[_], T <: Row, A, B]:
  /** the place `in`, by key, found at run time — this handler's? None by default (`null`): the protocol's handlers
   * (okay-kernel's, `okay.core.kernel`) say which keys are theirs, the evidence a `Key`'s (`Key.at`). Its result
   * not written `| Null`: Scala 2 reads the handler classes through TASTy and cannot read a union in a member's
   * result (`scala2/`, a handler of its own) — `null` all the same, matched by the machine */
  def at[F[_], R <: Row](in: In.At[F, R]): In.Same[E, F] =
    if (keyRef ne null) && (in.ref eq keyRef) then In.Same.refl[E, F] else null
  /** THE KEY this handler answers, set once where it is installed (`keyed`): the root's programs perform each
   * effect at a place by key, its constructor's (okay's `KeyOf`) — `null`, none */
  private var keyRef: AnyRef = null
  /** this handler answering the places by `key` (set before it is installed; one key a handler) */
  final def keyed(key: AnyRef): this.type = { keyRef = key; this }

/** a handler with no state: its `ret` of the value only */
sealed abstract class Stateless[E[_], T <: Row, A, B] extends Handler[E, T, A, B]:
  def ret(a: A): B ! T

/** the rest left where it is: the answers through */
sealed abstract class Passing[E[_], T <: Row, A, B] extends Stateless[E, T, A, B]

/** each operation ANSWERED in place, its value at once: no `k` made */
abstract class Answering[E[_], T <: Row, A, B] extends Passing[E, T, A, B]:
  def value[X](op: E[X]): X

/** each operation TRANSLATED to a program over `T`, outside the handler: its value the operation's */
abstract class Translating[E[_], T <: Row, A, B] extends Passing[E, T, A, B]:
  def apply[X](op: E[X]): X ! T

/** a clause given `k`, the rest — ONCE, at any answers: `k` the only way to the answer `O`, so the types have it
 * resumed once, not dropped, not twice; before it and after it anything over `T` */
abstract class Linear[E[_], T <: Row, A, B] extends Passing[E, T, A, B]:
  def apply[I, O, X](op: E[X], k: X => Cont[T, I, O, B]): Cont[T, I, O, B]

/** translating, the value through it unchanged */
abstract class Translate[E[_], T <: Row, A] extends Translating[E, T, A, A]:
  def ret(a: A): A ! T = Cont.Return(a)

/** the rest TAKEN: dropped, or `k` — a program of effects only */
sealed abstract class Taking[E[_], T <: Row, A, B] extends Stateless[E, T, A, B]

/** ABORTING: the rest of the program under it dropped, `abort` in its place — no `k` made */
abstract class Aborting[E[_], T <: Row, A, B] extends Taking[E, T, A, B]:
  def abort[X](op: E[X]): B ! T

/** a clause given `k`, the rest under the handler: dropped, resumed, resumed again, kept — a program over `T` */
abstract class Deep[E[_], T <: Row, A, B] extends Taking[E, T, A, B]:
  def apply[X](op: E[X], k: X => B ! T): B ! T

/** a STATE `S`, each operation answered in place: its value and the next state from the state and the operation, `ret`
 * given the last. A register of the machine's: a `k` taken past it has the state as it was then, each time it is
 * resumed */
abstract class Stateful[E[_], T <: Row, S, A, B] extends Handler[E, T, A, B]:
  def initial: S
  def ret(s: S, a: A): B ! T
  def value[X](s: S, op: E[X]): X
  def next[X](s: S, op: E[X]): S
