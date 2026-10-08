package okay.core

import Cont.*

/** AN INDEXED MONAD, `M[R, I, O, A]`: a program over the row `R` of value `A` which, with a continuation answering
 * `I`, answers `O` — Atkey's parameterised monad, the answer moved from `O` to `I`. What a context's programs are
 * values of; `Cont` is one, a test's CPS functions another. Final tagless: a program is written against the
 * operations, not the tree */
abstract class IxMonad:
  type M[R <: Row, I, O, A] <: Ix[M, R, I, O, A]
  def pure[R <: Row, S, A](a: A): M[R, S, S, A]
  /** `delay(m)`: the program `m`, built when it is run — a tail call */
  def delay[R <: Row, I, O, A](m: => M[R, I, O, A]): M[R, I, O, A]

/** A PROGRAM of the monad `M`: its bind and its map are its own, a program's methods — not the instance's. A bind
 * is the commonest call there is; on the program it is one call to the program's class, the instance not in it
 * (as an extension of the instance it was three, through the instance and the bridges erasure makes, and the JIT
 * gave up inlining them in a tight loop: ~1.2 ns an operation, DispatchBenchmark) */
abstract class Ix[M[X <: Row, I, O, A] <: Ix[M, X, I, O, A], R <: Row, I, O, A]:
  def flatMap[I2, B](f: A => M[R, I2, I, B]): M[R, I2, O, B]
  def map[B](f: A => B): M[R, I, O, B]

/** THE CONTEXT a program is written in: the monad, the row `R` there and the answer `S` at this point — what an
 * operation and a `shift` take their types from, by its members: `c.M`, `c.R`, `c.S`. The context IS the
 * instance: a program asks for one, `(using c: Effects)`, and is a value of `c.M`. A delimiter gives its body
 * one of its own, `under`: a handler's has the handler's effect in front of what the handler leaves, `E +: c.R`,
 * so the row is built by the handlers, outside in, and never named. The nearest context wins — a nested scope's
 * given is preferred */
abstract class Context extends IxMonad:
  type R <: Row
  type S

/** EFFECTS over the monad: an operation at its path in the row, and a handler folding a body over `E +: T` to one
 * over `T` */
abstract class Effects extends Context:
  /** the context at the row `R2`, the answer `S2`, the monad this one's */
  type Under[R2 <: Row, S2] <: Effects { type M[X <: Row, I, O, A] = Effects.this.M[X, I, O, A]; type R = R2; type S = S2 }
  def under[R2 <: Row, S2]: Under[R2, S2]
  def perform[E[_], R2 <: Row, S2, X](op: E[X], in: In[E, R2]): M[R2, S2, S2, X]
  def handle[E[_], T <: Row, Q, A, Ans](body: M[E +: T, Ans, Ans, A], h: Handler[M, E, T, Q, A, Ans]): M[T, Q, Q, Ans]

/** an effect of two parameters, its first given: `Reader % Int` is the effect `[X] =>> Reader[Int, X]` — the
 * effect in a row, a path, a handler, by name */
infix type %[F[_, +_], S] = [X] =>> F[S, X]

/** THE ROW `Es` IN THE ROW `R`: a path in `R` for each path in `Es` — the effects a program needs, found in the
 * context's row. Two cases, both total: the last effect, `E0 +: Pure`, where a path can only be `Here` (one past
 * it would be into `Pure`, and there is none — the compiler sees it); and an effect in front of more */
sealed trait Sub[Es <: Row, R <: Row]:
  def apply[E[_]](in: In[E, Es]): In[E, R]
object Sub:
  given last[E0[_], R <: Row](using head: In[E0, R]): Sub[E0 +: Pure, R] = new Sub[E0 +: Pure, R]:
    def apply[E[_]](in: In[E, E0 +: Pure]): In[E, R] = in match
      case In.Here() => head
  given cons[E0[_], E1[_], T <: Row, R <: Row](using head: In[E0, R], tail: Sub[E1 +: T, R]): Sub[E0 +: E1 +: T, R] =
    new Sub[E0 +: E1 +: T, R]:
      def apply[E[_]](in: In[E, E0 +: E1 +: T]): In[E, R] = in match
        case In.Here() => head
        case In.There(out) => tail(out)

/** the effect `E` AMONG THE EFFECTS a program needs, `Es`: the one effect it is, `Only`, or its path in the row of
 * them, `Path`. Which of the two is for the compiler's search to choose — by the kind of `Es`, an effect or a row */
enum Req[E[_], Es <: AnyKind]:
  case Only[E[_]]() extends Req[E, E]
  case Path[E[_], Es <: Row](in: In[E, Es]) extends Req[E, Es]
object Req:
  given only[E[_]]: Req[E, E] = Only()
  given path[E[_], Es <: Row](using in: In[E, Es]): Req[E, Es] = Path(in)

/** the context with every effect a program needs, `Es` — one effect, or a row of them — each found in its own row:
 * the context it is made of, every operation its */
sealed trait Needs[Es <: AnyKind] extends Effects:
  def find[E[_]](r: Req[E, Es]): In[E, R]
object Needs:
  /** one effect, `E0`, at its path */
  given one[E0[_]](using c: Effects, head: In[E0, c.R])
    : (Needs[E0] { type M[X <: Row, I, O, A] = c.M[X, I, O, A]; type R = c.R; type S = c.S }) =
    new Forward[E0, c.type](c):
      // `Only`: a `Path` is into a row, `E0` is an effect — of another kind, which the checker does not tell apart
      def find[E[_]](r: Req[E, E0]): In[E, R] = (r: @unchecked) match
        case Req.Only() => head
  /** a row of them, `Pure` at the end, each at its path */
  given row[Es <: Row](using c: Effects, sub: Sub[Es, c.R])
    : (Needs[Es] { type M[X <: Row, I, O, A] = c.M[X, I, O, A]; type R = c.R; type S = c.S }) =
    new Forward[Es, c.type](c):
      // `Path`: `Only` is for one effect, `Es` is a row — of another kind, as above
      def find[E[_]](r: Req[E, Es]): In[E, R] = (r: @unchecked) match
        case Req.Path(in) => sub(in)
  /** an effect needed: its path in the context's row */
  given found[E[_], Es <: AnyKind](using n: Needs[Es], r: Req[E, Es]): In[E, n.R] = n.find(r)

  /** every operation the context's `c` */
  private abstract class Forward[Es <: AnyKind, C <: Effects & Singleton](val c: C) extends Needs[Es]:
    type M[X <: Row, I, O, A] = c.M[X, I, O, A]
    type R = c.R
    type S = c.S
    type Under[R2 <: Row, S2] = c.Under[R2, S2]
    def under[R2 <: Row, S2]: Under[R2, S2] = c.under
    def pure[X <: Row, S2, A](a: A): M[X, S2, S2, A] = c.pure(a)
    def delay[X <: Row, I, O, A](m: => M[X, I, O, A]): M[X, I, O, A] = c.delay(m)
    def perform[E[_], R2 <: Row, S2, X](op: E[X], in: In[E, R2]): M[R2, S2, S2, X] = c.perform(op, in)
    def handle[E[_], T <: Row, Q, A, Ans](body: M[E +: T, Ans, Ans, A], h: Handler[M, E, T, Q, A, Ans]): M[T, Q, Q, Ans] =
      c.handle(body, h)

/** `A ! Es`: a program of value `A`, in any context whose row has every effect of `Es` — one, `Int ! Reader % Int`,
 * or a row of them, `Int ! Reader % Int +: Writer % String +: Pure`; their order the program's own, the context's
 * row any. At the context's answer, not `Free`'s `Unit`: under a handler the answer is the handler's
 * — `Either[E, A]` under `throws` — so a `Free` program would run only where nothing is answered */
infix type ![A, Es <: AnyKind] = (c: Needs[Es]) ?=> c.M[c.R, c.S, c.S, A]

object Effects:
  /** THE INSTANCE over `Cont`, the context at the row `R0`, the answer `S0`: effects and control both */
  final class At[R0 <: Row, S0] extends Delimited:
    type M[R <: Row, I, O, A] = Cont[R, I, O, A]
    type R = R0
    type S = S0
    type Under[R2 <: Row, S2] = At[R2, S2]
    def under[R2 <: Row, S2]: At[R2, S2] = At()
    def pure[X <: Row, S2, A](a: A): Cont[X, S2, S2, A] = Return(a)
    def delay[X <: Row, I, O, A](m: => Cont[X, I, O, A]): Cont[X, I, O, A] = Delay(() => m)
    def perform[E[_], R2 <: Row, S2, X](op: E[X], in: In[E, R2]): Cont[R2, S2, S2, X] = Inject(op, in)
    def handle[E[_], T <: Row, Q, A, Ans](body: Cont[E +: T, Ans, Ans, A], h: Handler[Cont, E, T, Q, A, Ans]): Cont[T, Q, Q, Ans] =
      body.handle(h)
    def shift[X <: Row, I, O, A](f: (A => Cont[X, O, O, I]) => Cont[X, O, O, O]): Cont[X, I, O, A] = Shift(f)
    def reset[X <: Row, Q, S2, O, A](body: Cont[X, S2, O, A], ret: A => S2): Cont[X, Q, Q, O] = Reset(body, ret)

  /** `run(body)`: the body at the top over `Cont`, nothing to perform, its answer its value */
  def run[A](body: At[Pure, A] ?=> Cont[Pure, A, A, A]): A = body(using At()).value

  /** `handle(h)(body)` in context: the body written in the handler's context, over `E +: c.R`, at its answer */
  def handle[E[_], A, Ans](using c: Effects)(h: Handler[c.M, E, c.R, c.S, A, Ans])
            (body: c.Under[E +: c.R, Ans] ?=> c.M[E +: c.R, Ans, Ans, A]): c.M[c.R, c.S, c.S, Ans] =
    c.handle(body(using c.under), h)

/** a handler of the effect `E`, over the row `E +: T`, leaving `T`, written at the answer `Q` outside its
 * delimiter: the body's value `A` to the answer `Ans`; each operation with the rest of the body up to the
 * delimiter, `k`, deep — the handler stays in force through a resumption. Not a node: `handle` folds the tree
 * with it; an operation of another effect goes on outside, its path one shorter */
trait Handler[M[_ <: Row, _, _, _], E[_], T <: Row, Q, A, Ans]:
  def ret(a: A): Ans
  def apply[X](op: E[X], k: X => M[T, Q, Q, Ans]): M[T, Q, Q, Ans]

/** a TAIL-RESUMPTIVE handler: each clause is `k(value(op))`, the value of the operation alone — so the operation is
 * ANSWERED IN PLACE: the fold goes on with the rest of the body at once, in its own loop, no resumption built,
 * nothing handed out (Koka's, Effekt's optimisation; here by the handler's declaration) */
trait Answering[M[_ <: Row, _, _, _], E[_], T <: Row, Q, A, Ans] extends Handler[M, E, T, Q, A, Ans]:
  def value[X](op: E[X]): X
  final def apply[X](op: E[X], k: X => M[T, Q, Q, Ans]): M[T, Q, Q, Ans] = k(value(op))

extension [E[_], T <: Row, A, Ans](body: Cont[E +: T, Ans, Ans, A])
  /** `body.handle(h)`: the tree folded at its head — a value answered by `ret`, an operation of `E` by its
   * clause with the rest of the body folded the same way, lazily, another effect's forwarded; a capture's body
   * folded in place of the rest, the rest under a delimiter of its own as `k`. The body is read up to its first
   * operation here, the rest when resumed */
  def handle[Q](h: Handler[Cont, E, T, Q, A, Ans]): Cont[T, Q, Q, Ans] = fold0(body, h.ret, h)

/** the fold while nothing is bound under the body — a tail call's, a resumption's — the common heads taken here
 * with no stack made; `fold` from the first bind or map on, with its stack. One method each, every loop a tail
 * call: a helper (an inline one too, through its accessor) would make the answered operation's fold a call, a
 * frame each */
@scala.annotation.tailrec
private def fold0[E[_], T <: Row, Q, S, Ans, A](body: Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[Cont, E, T, Q, ?, Ans]): Cont[T, Q, Q, Ans] =
  body match
    case o: Suspend[e1, r, i, so, x, a] => o.in match
      case In.Here() => h match
        case an: Answering[Cont, E, T, Q, ?, Ans] => fold0(o.k(an.value(o.op)), ret, h)
        case _ => clause(o.op, o.k, ret, h)
      case In.There(out) => out.bind(o.op, Refold(o.k, ret, h))
    case Return(a) => Return(ret(a))
    case Bind(Return(a), g) => fold0(g(a), ret, h)
    case Delay(t) => fold0(t(), ret, h)
    case Push(m, ks) => fold(m, ks, ret, h)
    case _ => fold(body, Frames.End(), ret, h)

/** THE FOLD over the body's binds with a stack of its own, `k`, as `loop`'s: an operation of `E` answered in
 * place goes on in the same loop with the same stack — no head made, no stack rebuilt; a clause, a forwarded
 * operation, a capture get the stack as the rest */
@scala.annotation.tailrec
private def fold[E[_], T <: Row, Q, S, Ans, A, T1, X](c: Cont[E +: T, T1, Ans, X], k: Frames[E +: T, X, S, T1, A], ret: A => S,
                                                    h: Handler[Cont, E, T, Q, ?, Ans]): Cont[T, Q, Q, Ans] =
  c match
    case o: Suspend[e1, r, i, so, x, a] => o.in match
      case In.Here() => h match
        case an: Answering[Cont, E, T, Q, ?, Ans] => fold(o.k(an.value(o.op)), k, ret, h)
        case _ => k match
          case Frames.End() => clause(o.op, o.k, ret, h)
          case _ => clause(o.op, Frames.Then(o.k, k), ret, h)
      case In.There(out) => k match
        case Frames.End() => out.bind(o.op, Refold(o.k, ret, h))
        case _ => out.bind(o.op, Refold(Frames.Then(o.k, k), ret, h))
    case Return(a) => k match
      case Frames.End() => Return(ret(a))
      case Frames.Then(f, next) => fold(f(a), next, ret, h)
      case Frames.Mapped(f, next) => next match
        case Frames.End() => Return(ret(f(a)))
        case Frames.Then(g, rest) => fold(g(f(a)), rest, ret, h)
        case Frames.Mapped(g, rest) => fold(Return(g(f(a))), rest, ret, h)
    case i: Inject[e1, r, s, x] => i.in match
      case In.Here() => h match
        case an: Answering[Cont, E, T, Q, ?, Ans] => fold(Return(an.value(i.op)), k, ret, h)
        case _ => clause(i.op, k, ret, h)
      case In.There(out) => out.bind(i.op, Refold(k, ret, h))
    case Bind(m, g) => m match
      case Return(a) => fold(g(a), k, ret, h)
      case _ => fold(m, Frames.Then(g, k), ret, h)
    case Map(m, f) => fold(m, Frames.Mapped(f, k), ret, h)
    case Delay(t) => fold(t(), k, ret, h)
    case Push(m, ks) => k match
      case Frames.End() => fold(m, ks, ret, h)
      case _ => fold(m, Frames.Then(ks, k), ret, h)
    case Shift(f) => fold(f(under(k, ret)), Frames.End(), identity, h)
    case Reset(b, r) => fold(delimited(b, r), k, ret, h)

/** this handler's operation given to its clause, with the rest as `k`, resumed lazily */
private def clause[E[_], T <: Row, Q, S, Ans, A, X](op: E[X], k: X => Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[Cont, E, T, Q, ?, Ans]): Cont[T, Q, Q, Ans] =
  h(op, Resume(k, ret, h))

/** the rest of a body folded again by its handler, once the operation it waits on is answered outside: the
 * continuation a forwarded operation goes out with. One object, where a closure over a closure was two. Applied
 * by whoever answers the operation outside — while folding, or inside a resumption of its own, lazy already */
private final class Refold[E[_], T <: Row, Q, S, Ans, A, X](k: X => Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[Cont, E, T, Q, ?, Ans])
  extends (X => Cont[T, Q, Q, Ans]):
  def apply(x: X): Cont[T, Q, Q, Ans] = fold0(k(x), ret, h)

/** a clause's `k`: the rest of the body, folded again by the handler, LAZILY — a `Delay`, built when stepped, so a
 * clause that keeps `k` (a generator's next step) runs nothing by making it. One object and its delay */
private final class Resume[E[_], T <: Row, Q, S, Ans, A, X](k: X => Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[Cont, E, T, Q, ?, Ans])
  extends (X => Cont[T, Q, Q, Ans]):
  def apply(x: X): Cont[T, Q, Q, Ans] = Delay(() => fold0(k(x), ret, h))

/** `perform(op)`: an operation of `E`, in the context — its row has `E`, its answer is the operation's */
def perform[E[_], X](op: E[X])(using c: Effects, in: In[E, c.R]): c.M[c.R, c.S, c.S, X] = c.perform(op, in)
