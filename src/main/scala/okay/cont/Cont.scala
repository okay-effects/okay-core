package okay.cont

import scala.annotation.tailrec
import okay.core.*

/**
 * THE MONAD OF DELIMITED CONTINUATIONS over a row `R`, one level: `Cont[R, I, O, A]` is a program of value `A`
 * which, with a continuation answering `I`, answers `O` — Danvy–Filinski's `(A => I) => O`, the answer type
 * modified. An operation is a leaf, `Inject`, with the path to its effect in the row, and means nothing by
 * itself — a handler is not in the tree; it is a fold over the tree's head (`handle`, in Effects.scala). `Bind`
 * composes the answers end to end; `Return`, `Inject`, `Map` and `Delay` keep them; `Shift` and `Reset` move
 * them. Five nodes are the monad; `Suspend`, `Map`, `Delay` and `Push` are four of its programs as one node each,
 * for speed — an operation with its rest, a map, a tail call, a resumed stack, each without a bind. `step` reads
 * the nine, to the head, with a stack of frames.
 */
enum Cont[R <: Row, I, O, A]:
  case Return[R <: Row, S, A](a: A) extends Cont[R, S, S, A]
  /** an operation of `E`, at its path in the row: a leaf, to whoever folds the tree */
  case Inject[E[_], R <: Row, S, X](op: E[X], in: In[E, R]) extends Cont[R, S, S, X]
  /** an operation of `E` with the rest bound to its value: `Bind(Inject(op, in), k)` as one node — what
   * `flatMap` on an operation makes, and what an operation crossing a delimiter goes on outside as */
  case Suspend[E[_], R <: Row, I, O, X, A](op: E[X], in: In[E, R], k: X => Cont[R, I, O, A]) extends Cont[R, I, O, A]
  /** the answers composed end to end: `m` from `T` to `O`, `f`'s from `I` to `T` */
  case Bind[R <: Row, I, T, O, A, B](m: Cont[R, T, O, A], f: A => Cont[R, I, T, B]) extends Cont[R, I, O, B]
  /** `shift(k => body)`: `k` is the context up to the nearest delimiter, under a delimiter of its own, delivering
   * the answer at the hole, `I`; the body runs under the delimiter in place of the context, at its final answer
   * `O`, and its value is that answer */
  case Shift[R <: Row, I, O, A](f: (A => Cont[R, O, O, I]) => Cont[R, O, O, O]) extends Cont[R, I, O, A]
  /** `m.map(f)`: one node, where `m.flatMap(a => pure(f(a)))` is a bind, a closure and a return */
  case Map[R <: Row, I, O, A, B](m: Cont[R, I, O, A], f: A => B) extends Cont[R, I, O, B]
  /** a program built when the loop gets to it: a tail call one node, where `pure(()).flatMap(_ => c)` is two */
  case Delay[R <: Row, I, O, A](c: () => Cont[R, I, O, A]) extends Cont[R, I, O, A]
  /** the delimiter: the body's value to its initial answer `S` by `ret`, its final answer `O` the delimiter's
   * value outside, at the answer `Q` there. An operation inside goes out as it is, the rest re-delimited */
  case Reset[R <: Row, Q, S, O, A](body: Cont[R, S, O, A], ret: A => S) extends Cont[R, Q, Q, O]
  /** `m` with the rest as a stack already: what applying a captured `k` makes — `Bind(m, k)`, where the loop can
   * take `k` as its stack as it is, not as a function to push. Only `Frames` make it */
  case Push[R <: Row, I, T, O, A, B](m: Cont[R, T, O, A], k: Frames[R, A, I, T, B]) extends Cont[R, I, O, B]
  /** a HANDLER of `E` over its body, a node: the body built when the loop enters it, its value to `S` by `ret`, an
   * operation of `E` given to `h`'s clause — answered in place if it answers so — another's on outside, one effect
   * shorter: the fold the loop does there */
  case Fold[E[_], T <: Row, Q, S, Ans, A](body: () => Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans])
    extends Cont[T, Q, Q, Ans]
  /** an INTERCEPTOR of `E` at `in`, over its body, a node: the row the same, an operation at `in` given to `h`'s
   * clause, another on outside as it is */
  case Icept[E[_], R <: Row, Q, S, Ans, A](body: () => Cont[R, S, Ans, A], ret: A => S, in: In[E, R], h: Handler[[Z] =>> Cont[R, Q, Q, Z], E, ?, Ans])
    extends Cont[R, Q, Q, Ans]

  def flatMap[I2, B](f: A => Cont[R, I2, I, B]): Cont[R, I2, O, B] = this match
    case i: Inject[e, r, s, x] => Cont.bound(i, f)
    case _ => Bind(this, f)
  def map[B](f: A => B): Cont[R, I, O, B] = Map(this, f)

/** a program over the row `R` whose answer is nothing, `Unit`, before and after: the free monad of the row's
 * effects — no capture moves its answer, its value only a handler's */
type Free[R <: Row, A] = Cont[R, Unit, Unit, A]

/** WHAT IS BOUND TO A VALUE, the loop's stack: the functions a value goes through, innermost first — each
 * `Bind` the loop enters pushes its function, no tree rebuilt. As a function it is the rest itself, `k`: a head's
 * continuation is the stack as it stands. Applying it runs one function, the rest bound after it — one call,
 * whatever the depth */
enum Frames[R <: Row, A, I, T, B] extends (A => Cont[R, I, T, B]):
  /** nothing bound: the value is the program's */
  case End[R <: Row, S, A]() extends Frames[R, A, S, S, A]
  /** `f` bound to the value, the rest of the stack to its result */
  case Then[R <: Row, A, X, I, T1, T, B](f: A => Cont[R, T1, T, X], next: Frames[R, X, I, T1, B]) extends Frames[R, A, I, T, B]
  /** `f` mapped over the value, the rest of the stack to its result. Applied, it builds the next step and stops —
   * a chain of maps is not run through here, in the caller's stack */
  case Mapped[R <: Row, A, X, I, T, B](f: A => X, next: Frames[R, X, I, T, B]) extends Frames[R, A, I, T, B]

  def apply(a: A): Cont[R, I, T, B] = this match
    case End() => Cont.Return(a)
    case Then(f, next) => next match
      case End() => f(a)
      case _ => Cont.Push(f(a), next)
    case Mapped(f, next) => next match
      case End() => Cont.Return(f(a))
      case _ => Cont.Push(Cont.Return(f(a)), next)

/** a program's head, what the loop over its binds comes to: its value, an operation with the rest, or a capture
 * with the rest. An operation's row has its effect's path in it, so a program over `Pure` has no operation. The
 * rest is LAZY to resume, `Bind(Return(x), k)`: it runs when stepped, not when `k` is applied */
enum Head[R <: Row, I, O, A]:
  case Done[R <: Row, S, A](a: A) extends Head[R, S, S, A]
  case Op[E[_], E2[_], T <: Row, I, O, X, A](op: E[X], in: In[E, E2 +: T], k: X => Cont[E2 +: T, I, O, A]) extends Head[E2 +: T, I, O, A]
  case Cut[R <: Row, I, T, O, X, A](f: (X => Cont[R, O, O, T]) => Cont[R, O, O, O], k: Frames[R, X, I, T, A]) extends Head[R, I, O, A]

object Cont:
  /** an operation with the rest bound to its value, one node — out of `flatMap`, to keep that small */
  private def bound[E[_], R <: Row, S, X, I2, B](i: Inject[E, R, S, X], f: X => Cont[R, I2, S, B]): Cont[R, I2, S, B] =
    Suspend(i.op, i.in, f)
  def pure[R <: Row, S, A](a: A): Cont[R, S, S, A] = Return(a)
  def inject[R <: Row, S, E[_], X](op: E[X])(using in: In[E, R]): Cont[R, S, S, X] = Inject(op, in)
  def shift[R <: Row, I, O, A](f: (A => Cont[R, O, O, I]) => Cont[R, O, O, O]): Cont[R, I, O, A] = Shift(f)
  /** `delay(c)`: the program `c`, built when the loop gets to it — a tail call in constant stack. Inline, so the
   * thunk closes over `c`'s own variables, not over a by-name wrapper of them */
  inline def delay[R <: Row, I, O, A](inline c: Cont[R, I, O, A]): Cont[R, I, O, A] = Delay(() => c)
  extension [R <: Row, S, O](body: Cont[R, S, O, S])
    /** `body.reset`: the body's value is its initial answer, the delimiter's value its final one */
    def reset[Q]: Cont[R, Q, Q, O] = Reset(body, identity)

  extension [R <: Row, I, O, A](c: Cont[R, I, O, A])
    /** to the head: binds reassociated and followed, delimiters entered, in constant stack */
    def step: Head[R, I, O, A] = run[R, I, I, O, A, A, Head[R, I, O, A]](c, Frames.End(), Meta.Top())

  /** THE DELIMITERS THE LOOP IS INSIDE, innermost first — its METACONTINUATION: what takes the head of the innermost
   * one's body, of type `Head[R, I, O, B]`, toward the loop's result `Z`. At the top, the caller; under a reset, a
   * handler (its fold), an interceptor: its reading of the head, then on in the frames outside it, in the same loop —
   * so delimiters nested to any depth run in constant stack */
  private[cont] enum Meta[R <: Row, I, O, B, Z]:
    case Top[R <: Row, I, O, B]() extends Meta[R, I, O, B, Head[R, I, O, B]]
    case InReset[R <: Row, Q, S, O, A, I1, B1, Z](ret: A => S, k: Frames[R, O, I1, Q, B1], up: Meta[R, I1, Q, B1, Z])
      extends Meta[R, S, O, A, Z]
    case InFold[E[_], T <: Row, Q, S, Ans, A, I1, B1, Z](ret: A => S, h: Handler[[X] =>> Cont[T, Q, Q, X], E, ?, Ans],
        k: Frames[T, Ans, I1, Q, B1], up: Meta[T, I1, Q, B1, Z]) extends Meta[E +: T, S, Ans, A, Z]
    case InIcept[E[_], R <: Row, Q, S, Ans, A, I1, B1, Z](ret: A => S, in: In[E, R], h: Handler[[X] =>> Cont[R, Q, Q, X], E, ?, Ans],
        k: Frames[R, Ans, I1, Q, B1], up: Meta[R, I1, Q, B1, Z]) extends Meta[R, S, Ans, A, Z]

  /** THE LOOP: `c` with the frames `k` inside the delimiters `m` — to the head of the outermost's body, or on: a
   * delimiter entered pushed; its body's value, an operation, a capture read by it — the program leaving it run in
   * the frames outside it; an operation a handler answers in place answered where it is */
  @tailrec private[cont] def run[R <: Row, I, T, O, A, B, Z](c: Cont[R, T, O, A], k: Frames[R, A, I, T, B], m: Meta[R, I, O, B, Z]): Z =
    c match
    case Return(a) => k match
      case Frames.End() => m match
        case _: Meta.Top[r, i, o, b] => Head.Done[R, I, B](a)
        case d: Meta.InReset[r, q, s, o, a1, i1, b1, z] => run(Return[r, q, o](d.ret(a)), d.k, d.up)
        case d: Meta.InFold[e, t, q, s, ans, a1, i1, b1, z] => run(Return[t, q, ans](d.ret(a)), d.k, d.up)
        case d: Meta.InIcept[e, r, q, s, ans, a1, i1, b1, z] => run(Return[r, q, ans](d.ret(a)), d.k, d.up)
      case Frames.Then(f, next) => run(f(a), next, m)
      case Frames.Mapped(f, next) => next match
        case Frames.End() => run(Return(f(a)), Frames.End(), m)
        case Frames.Then(g, rest) => run(g(f(a)), rest, m)
        case Frames.Mapped(g, rest) => run(Return(g(f(a))), rest, m)
    case Inject(op, in) => m match
      case _: Meta.Top[r, i, o, b] => in.op(op, k)
      case d: Meta.InReset[r, q, s, o, a1, i1, b1, z] => run(in.bind(op, Redelimit[r, q, s, o, a1, A](k, d.ret)), d.k, d.up)
      case d: Meta.InFold[e, t, q, s, ans, a1, i1, b1, z] => in match
        case In.Here() => answers(d.h) match
          case null => run(d.h(op, Resume[e, t, q, s, ans, a1, A](k, d.ret, d.h)), d.k, d.up)
          case an => run(Return(an.value(op)), k, m)
        case In.There(out) => run(out.bind(op, Refold[e, t, q, s, ans, a1, A](k, d.ret, d.h)), d.k, d.up)
      case d: Meta.InIcept[e, r, q, s, ans, a1, i1, b1, z] => In.same(in, d.in) match
        case null => run(in.bind(op, Reintercept[e, r, q, s, ans, a1, A](k, d.ret, d.in, d.h)), d.k, d.up)
        case same => run(d.h(same(op), Reintercept[e, r, q, s, ans, a1, A](k, d.ret, d.in, d.h)), d.k, d.up)
    case sp: Suspend[e0, r0, i0, o0, x, a0] =>
      // the rest: the operation's own, then the frames
      val g: x => Cont[R, I, O, B] = k match
        case Frames.End() => sp.k
        case _ => Frames.Then(sp.k, k)
      m match
        case _: Meta.Top[r, i, o, b] => sp.in.op(sp.op, g)
        case d: Meta.InReset[r, q, s, o, a1, i1, b1, z] => run(sp.in.bind(sp.op, Redelimit[r, q, s, o, a1, x](g, d.ret)), d.k, d.up)
        case d: Meta.InFold[e, t, q, s, ans, a1, i1, b1, z] => sp.in match
          case In.Here() => answers(d.h) match
            case null => run(d.h(sp.op, Resume[e, t, q, s, ans, a1, x](g, d.ret, d.h)), d.k, d.up)
            case an => run(g(an.value(sp.op)), Frames.End(), m)
          case In.There(out) => run(out.bind(sp.op, Refold[e, t, q, s, ans, a1, x](g, d.ret, d.h)), d.k, d.up)
        case d: Meta.InIcept[e, r, q, s, ans, a1, i1, b1, z] => In.same(sp.in, d.in) match
          case null => run(sp.in.bind(sp.op, Reintercept[e, r, q, s, ans, a1, x](g, d.ret, d.in, d.h)), d.k, d.up)
          case same => run(d.h(same(sp.op), Reintercept[e, r, q, s, ans, a1, x](g, d.ret, d.in, d.h)), d.k, d.up)
    // a capture: up to the innermost delimiter — its body in place of the rest, under the delimiter again, the rest
    // under a delimiter of its own with `ret` as `k`
    case Shift(f) => m match
      case _: Meta.Top[r, i, o, b] => Head.Cut(f, k)
      case d: Meta.InReset[r, q, s, o, a1, i1, b1, z] =>
        run(f(under(k, d.ret)), Frames.End(), Meta.InReset[r, q, o, o, o, i1, b1, z](identity, d.k, d.up))
      case d: Meta.InFold[e, t, q, s, ans, a1, i1, b1, z] =>
        run(f(under(k, d.ret)), Frames.End(), Meta.InFold[e, t, q, ans, ans, ans, i1, b1, z](identity, d.h, d.k, d.up))
      case d: Meta.InIcept[e, r, q, s, ans, a1, i1, b1, z] =>
        run(f(under(k, d.ret)), Frames.End(), Meta.InIcept[e, r, q, ans, ans, ans, i1, b1, z](identity, d.in, d.h, d.k, d.up))
    // a delimiter: entered, this loop's frames pushed under it
    case rs: Reset[r, q, s, o, a1] => run(rs.body, Frames.End(), Meta.InReset[r, q, s, o, a1, I, B, Z](rs.ret, k, m))
    case fo: Fold[e, t, q, s, ans, a1] => run(fo.body(), Frames.End(), Meta.InFold[e, t, q, s, ans, a1, I, B, Z](fo.ret, fo.h, k, m))
    case ic: Icept[e, r, q, s, ans, a1] => run(ic.body(), Frames.End(), Meta.InIcept[e, r, q, s, ans, a1, I, B, Z](ic.ret, ic.in, ic.h, k, m))
    case Delay(t) => run(t(), k, m)
    case Map(mm, f) => run(mm, Frames.Mapped(f, k), m)
    case Push(mm, ks) => k match
      case Frames.End() => run(mm, ks, m)
      case _ => run(mm, Frames.Then(ks, k), m)
    case Bind(mm, g) => mm match
      case Return(a) => run(g(a), k, m)
      case _ => run(mm, Frames.Then(g, k), m)

  /** a handler's clauses as one that answers in place, if it is one */
  private def answers[P[_], E[_]](h: Handler[P, E, ?, ?]): Value[E] | Null = h match
    case a: Answering[P, E, ?, ?] => a
    case _ => null

  /** a captured rest as `k`: applied, the stack it holds resumed, under a delimiter of its own with `ret` */
  private[cont] def under[R <: Row, Q, S, T, X, A](k: Frames[R, X, S, T, A], ret: A => S): X => Cont[R, Q, Q, T] =
    x => Reset(Push(Return(x), k), ret)

  /** the rest of a reset's body, once the operation it waits on is answered outside: under the reset again */
  private final class Redelimit[R <: Row, Q, S, O, A, X](k: X => Cont[R, S, O, A], ret: A => S) extends (X => Cont[R, Q, Q, O]):
    def apply(x: X): Cont[R, Q, Q, O] = Reset(k(x), ret)
  /** a clause's `k`: the rest of the body under the handler again, built when stepped — a clause that keeps `k` (a
   * generator's next step) runs nothing by making it */
  private final class Resume[E[_], T <: Row, Q, S, Ans, A, X](k: X => Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans])
      extends (X => Cont[T, Q, Q, Ans]):
    def apply(x: X): Cont[T, Q, Q, Ans] = Fold(() => k(x), ret, h)
  /** the rest of a handler's body, once an operation it passed on is answered outside: under the handler again */
  private final class Refold[E[_], T <: Row, Q, S, Ans, A, X](k: X => Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans])
      extends (X => Cont[T, Q, Q, Ans]):
    def apply(x: X): Cont[T, Q, Q, Ans] = Fold(() => k(x), ret, h)
  /** the rest of an interceptor's body: under the interceptor again */
  private final class Reintercept[E[_], R <: Row, Q, S, Ans, A, X](k: X => Cont[R, S, Ans, A], ret: A => S, in: In[E, R],
      h: Handler[[Z] =>> Cont[R, Q, Q, Z], E, ?, Ans]) extends (X => Cont[R, Q, Q, Ans]):
    def apply(x: X): Cont[R, Q, Q, Ans] = Icept(() => k(x), ret, in, h)

  extension [A](c: Cont[Pure, A, A, A])
    /** a program at the top, nothing to perform, its answer its value: the value — the top is a delimiter; a
     * capture up to it, its body under the top again, in a loop */
    def value: A = valueOf(c)
  @tailrec private def valueOf[A](c: Cont[Pure, A, A, A]): A = c.step match
    case Head.Done(a) => a
    case Head.Cut(f, k) => valueOf(f(under(k, identity)))

extension [E[_], R <: Row](in: In[E, R])
  /** the head for an operation at this path: the row has the effect, the path says so */
  private[cont] def op[I, O, X, A](op: E[X], k: X => Cont[R, I, O, A]): Head[R, I, O, A] = in match
    case h: In.Here[e, t] => Head.Op[E, E, t, I, O, X, A](op, h, k)
    case h: In.There[e, e2, t] => Head.Op[E, e2, t, I, O, X, A](op, h, k)
  /** the operation out at this path, bound to `k`: an operation crossing a delimiter goes on outside so, `k` the
   * rest put back under the delimiter it crossed */
  private[cont] def bind[Q, X, Z](op: E[X], k: X => Cont[R, Q, Q, Z]): Cont[R, Q, Q, Z] = Cont.Suspend(op, in, k)
