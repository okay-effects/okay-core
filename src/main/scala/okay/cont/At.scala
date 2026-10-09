package okay.cont

import okay.core.*
import Cont.*

/** THE INSTANCE over `Cont`, the context at the row `R0`, the answer `S0`: effects and control both */
class At[R0 <: Row, S0] extends Effects with Paths.Keep[R0, At[R0, S0]]:
  type M[I, O, A] = Cont[R0, I, O, A]
  type Effs = R0
  type S = S0
  type Inner[X <: Row, I, O, A] = Cont[X, I, O, A]
  type Handled[E[_], S2] = At[E +: R0, S2]
  type Reset[S2] = At[R0, S2]
  type Answer[S2] = At[R0, S2]
  def pure[S2, A](a: A): Cont[R0, S2, S2, A] = Return(a)
  def delay[I, O, A](m: => Cont[R0, I, O, A]): Cont[R0, I, O, A] = Delay(() => m)
  extension [I, O, A](m: Cont[R0, I, O, A])
    def flatMap[I2, B](f: A => Cont[R0, I2, I, B]): Cont[R0, I2, O, B] = m.flatMap(f)
    def map[B](f: A => B): Cont[R0, I, O, B] = m.map(f)
  def perform[E[_], S2, X](op: E[X], in: In[E, R0]): Cont[R0, S2, S2, X] = Inject(op, in)
  def handle[E[_], A, Ans](h: Handler[[Z] =>> Cont[R0, S0, S0, Z], E, A, Ans])(body: At[E +: R0, Ans] ?=> Cont[E +: R0, Ans, Ans, A]): Cont[R0, S0, S0, Ans] =
    body(using At()).handle(h)
  // an `At` itself: `perform` and the rest the one implementation there is, called straight
  type Needing[Es <: AnyKind] = At[R0, S0] & Needs[Es]
  // the path kept in the context itself, no `Paths` between: one load an operation, not two
  def needing[Es <: AnyKind](p: Paths[Es, R0]): At[R0, S0] & Needs[Es] = p.kept(this)
  def one[E0[_]](head: In[E0, R0]): At[R0, S0] & Needs[E0] = NeedyOne(head)
  def row[Es <: Row](sub: Sub[Es, R0]): At[R0, S0] & Needs[Es] = NeedyRow(sub)
  def answer[S2]: At[R0, S2] = At()
  def top: Top = okay.cont.top
  def shift[I, O, A](f: (A => Cont[R0, O, O, I]) => Cont[R0, O, O, O], same: (I =:= O) | Null): Cont[R0, I, O, A] = Shift(f)
  def reset[S2, O](body: At[R0, S2] ?=> Cont[R0, S2, O, S2]): Cont[R0, S0, S0, O] = Reset(body(using At()), identity)

/** an `At` with the path of the one effect a program needs */
final class NeedyOne[R0 <: Row, S0, E0[_]](head: In[E0, R0]) extends At[R0, S0] with Needs[E0]:
  // `Only`, as in `Paths.One`
  def find[E[_]](r: Req[E, E0]): In[E, R0] = (r: @unchecked) match
    case Req.Only() => head
/** an `At` with the paths of the row of effects a program needs */
final class NeedyRow[R0 <: Row, S0, Es <: Row](sub: Sub[Es, R0]) extends At[R0, S0] with Needs[Es]:
  // `Path`, as in `Paths.Of`
  def find[E[_]](r: Req[E, Es]): In[E, R0] = (r: @unchecked) match
    case Req.Path(in) => sub(in)

/** `run(body)`: the body at the top over `Cont`, nothing to perform, its answer its value */
def run[A](body: At[Pure, A] ?=> Cont[Pure, A, A, A]): A = body(using At()).value

extension [E[_], T <: Row, A, Ans](body: Cont[E +: T, Ans, Ans, A])
  /** `body.handle(h)`: the tree folded at its head — a value answered by `ret`, an operation of `E` by its
   * clause with the rest of the body folded the same way, lazily, another effect's forwarded; a capture's body
   * folded in place of the rest, the rest under a delimiter of its own as `k`. The body is read up to its first
   * operation here, the rest when resumed */
  def handle[Q](h: Handler[[Z] =>> Cont[T, Q, Q, Z], E, A, Ans]): Cont[T, Q, Q, Ans] = fold0(body, h.ret, h)

/** the fold while nothing is bound under the body — a tail call's, a resumption's — the common heads taken here
 * with no stack made; `fold` from the first bind or map on, with its stack. One method each, every loop a tail
 * call: a helper (an inline one too, through its accessor) would make the answered operation's fold a call, a
 * frame each */
@scala.annotation.tailrec
private def fold0[E[_], T <: Row, Q, S, Ans, A](body: Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans]): Cont[T, Q, Q, Ans] =
  body match
    case o: Suspend[e1, r, i, so, x, a] => o.in match
      case In.Here() => h match
        case an: Answering[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans] => fold0(o.k(an.value(o.op)), ret, h)
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
                                                    h: Handler[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans]): Cont[T, Q, Q, Ans] =
  c match
    case o: Suspend[e1, r, i, so, x, a] => o.in match
      case In.Here() => h match
        case an: Answering[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans] => fold(o.k(an.value(o.op)), k, ret, h)
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
        case an: Answering[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans] => fold(Return(an.value(i.op)), k, ret, h)
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
private def clause[E[_], T <: Row, Q, S, Ans, A, X](op: E[X], k: X => Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans]): Cont[T, Q, Q, Ans] =
  h(op, Resume(k, ret, h))

/** the rest of a body folded again by its handler, once the operation it waits on is answered outside: the
 * continuation a forwarded operation goes out with. One object, where a closure over a closure was two. Applied
 * by whoever answers the operation outside — while folding, or inside a resumption of its own, lazy already */
private final class Refold[E[_], T <: Row, Q, S, Ans, A, X](k: X => Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans])
  extends (X => Cont[T, Q, Q, Ans]):
  def apply(x: X): Cont[T, Q, Q, Ans] = fold0(k(x), ret, h)

/** a clause's `k`: the rest of the body, folded again by the handler, LAZILY — a `Delay`, built when stepped, so a
 * clause that keeps `k` (a generator's next step) runs nothing by making it. One object and its delay */
private final class Resume[E[_], T <: Row, Q, S, Ans, A, X](k: X => Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans])
  extends (X => Cont[T, Q, Q, Ans]):
  def apply(x: X): Cont[T, Q, Q, Ans] = Delay(() => fold0(k(x), ret, h))

/** `Cont`'s top, for a library re-entering it */
object top extends Top:
  val at: At[Pure, Unit] = At()
  def run(p: at.Prog[Unit]): Unit = p.value
