package okay.zero

// the rows and handlers, okay.core's: imported, not exported — Scala 2's TASTy reader fails on an export of a sealed trait
import okay.core.{Row, +:, Pure, In, :?, %, Sub, Req, Paths, Handler, Answering, Value}

import scala.annotation.tailrec

/**
 * THE MONAD OF shift0 AND $, answers modified — Materzok–Biernacki's. `Cont0[I, O, A]`: a program of value `A`
 * which, with a continuation answering `I`, answers `O`, `(A => I) => O`. Under a delimiter the answers are programs
 * of the level outside — the levels nested in the answer types.
 *
 *  - `ret $ body` (`Dollar`): the body's value through `ret` to its answer `I`, a program outside; the body's final
 *    answer the delimiter's.
 *  - `shift0(k => e)`: `k` the rest up to the nearest delimiter and the delimiter — `λx. ret $ K[x]`; `e` runs in
 *    the delimiter's place, outside it.
 *
 * Four nodes, open: a monad of its own, raw shift0 and `$`, the levels named in the types. The rest are this
 * package's, each a shift0 program in one node, for speed — a map, a tail call, an operation (a shift0 whose body
 * is its request), a `shift` (the same, a control request), a handler and a reset (a `$` whose answers are read
 * there), a resumption — and run by the machine (Eff.scala), each delimiter in a loop of its own. `step` runs any
 * of them to its head, the machine's by their shift0 programs, in constant stack.
 */
sealed abstract class Cont0[I, O, A] extends okay.kernel.Prog[A]:
  final def flatMap[I2, B](f: A => Cont0[I2, I, B]): Cont0[I2, O, B] = Cont0.bind(this, f)
  final def map[B](f: A => B): Cont0[I, O, B] = Cont0.Map(this, f)

object Cont0:
  final case class Return[S, A](a: A) extends Cont0[S, S, A]
  final case class Bind[I, T, O, A, B](m: Cont0[T, O, A], f: A => Cont0[I, T, B]) extends Cont0[I, O, B]
  final case class Shift0[I1, O1, X1, O, A](f: (A => Cont0[I1, O1, X1]) => O) extends Cont0[Cont0[I1, O1, X1], O, A]
  final case class Dollar[I, I2, O2, B, A](body: Cont0[I, Cont0[I2, O2, B], A], ret: A => I) extends Cont0[I2, O2, B]

  /** `m.map(f)`: `Bind(m, a => Return(f(a)))` */
  private[zero] final case class Map[I, O, A, B](m: Cont0[I, O, A], f: A => B) extends Cont0[I, O, B]
  /** a program built when the loop gets to it */
  private[zero] final case class Delay[I, O, A](c: () => Cont0[I, O, A]) extends Cont0[I, O, A]
  /** an operation of `E` at its path, at the answer `T`: `shift0(k => Return(Req(op, in, k)))` */
  private[zero] final case class Op[R <: Row, J, T, E[_], X](op: E[X], in: In[E, R]) extends Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], X]
  /** an operation with the rest bound to its value: `Bind(Op(op, in), k)` */
  private[zero] final case class OpK[R <: Row, J, T, E[_], X, I, B](op: E[X], in: In[E, R], k: X => Cont0[I, Cont0[J, J, Res[R, J, T]], B])
    extends Cont0[I, Cont0[J, J, Res[R, J, T]], B]
  /** a `shift` to the nearest delimiter: `shift0(k => Return(Ctl(f, k)))`; `same` if it keeps the answer — what a
   * handler's clauses need of the rest resumed under the handler again */
  private[zero] final case class Ctl[R <: Row, J, A, I, O](f: (A => Cont0[Cont0[J, J, Res[R, J, O]], Cont0[J, J, Res[R, J, O]], I]) => Cont0[Cont0[J, J, Res[R, J, O]], Cont0[J, J, Res[R, J, O]], O],
      same: (I =:= O) | Null)
    extends Cont0[Cont0[J, J, Res[R, J, I]], Cont0[J, J, Res[R, J, O]], A]
  /** a `shift0` to the nearest reset: `shift0(k => f(a => passAll(k(a))).map(Done))` — `f` in the reset's place,
   * outside it, its `k` the rest under the reset again */
  private[zero] final case class Ctl0[R <: Row, J, T, A, S, O](f: (A => Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], S]) => Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], O])
    extends Cont0[Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], S]], Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], O]], A]
  /** A DELIMITER NODE — a handler's, an interceptor's, a reset's: the machine enters each the same way, its body in
   * a loop of its own under it, over `RB`, outside it `R`; what differs, the handlers in force inside, `inside` */
  private[zero] sealed abstract class Node[R <: Row, J, T, RB <: Row, I0, V, A] extends Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], V]
      with Delim[RB, Cont0[J, J, Res[R, J, T]], I0, V, A]:
    def body: Cont0[Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[RB, Cont0[J, J, Res[R, J, T]], I0]], Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[RB, Cont0[J, J, Res[R, J, T]], V]], A]
    /** the handlers in force inside, those outside `ev` */
    def inside(ev: Ev[R]): Ev[RB]
  /** a handler of `E`: `Done∘h.ret $ body`, its answers read by `h` — the delimiter its loop answers to, itself */
  private[zero] final case class Handle[E[_], R <: Row, J, T, A, Ans](mk: () => Cont0[Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[E +: R, Cont0[J, J, Res[R, J, T]], Ans]], Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[E +: R, Cont0[J, J, Res[R, J, T]], Ans]], A],
      h: Handler[[X] =>> Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], X], E, A, Ans]) extends Node[R, J, T, E +: R, Ans, Ans, A]
      with ByHandler[E, R, J, T, A, Ans, Ans] with (A => Ans):
    /** the body, built when the machine enters the handler — a handler in a handler… built in constant stack */
    def body = mk()
    def inside(ev: Ev[R]): Ev[E +: R] = within(h, ev)
    def clauses: Handler[[Y] =>> Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Y], E, ?, Ans] = h
    def ret(a: A): Ans = h.ret(a)
    /** the node its own `ret`, a function: no closure made for it, a shift each */
    def apply(a: A): Ans = h.ret(a)
    def rets: A => Ans = this
  /** an interceptor of `E` at `target`, over the row `R`: `Done∘h.ret $ body`, its operations at `target` read by
   * `h`, the others passed on — the delimiter its loop answers to, itself */
  private[zero] final case class Icpt[E[_], R <: Row, J, T, A, Ans](target: In[E, R],
      mk: () => Cont0[Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], Ans]], Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], Ans]], A],
      h: Handler[[X] =>> Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], X], E, A, Ans]) extends Node[R, J, T, R, Ans, Ans, A]
      with ByIntercept[E, R, J, T, A, Ans] with (A => Ans):
    def body = mk()
    def inside(ev: Ev[R]): Ev[R] = masked(ev, target)
    def clauses: Handler[[Y] =>> Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Y], E, ?, Ans] = h
    def ret(a: A): Ans = h.ret(a)
    def apply(a: A): Ans = h.ret(a)
    def rets: A => Ans = this
  /** `m`, a program outside the nearest delimiter, run there, its value back here: `shift0(k => Bind(m, k))` — what
   * a `shift`'s `k` is from inside the reset it is under again */
  private[zero] final case class Lift[RB <: Row, L, O, X](m: Cont0[L, L, X]) extends Cont0[Cont0[L, L, Res[RB, L, O]], Cont0[L, L, Res[RB, L, O]], X]
  /** a reset: `Done $ body`, every operation in it passed on */
  private[zero] final case class Reset[R <: Row, J, T, S, O](mk: () => Cont0[Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], S]], Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Res[R, Cont0[J, J, Res[R, J, T]], O]], S])
    extends Node[R, J, T, R, S, O, S]
    with ByReset[R, J, T, S, O]:
    def body = mk()
    def inside(ev: Ev[R]): Ev[R] = ev
  /** a captured rest `f` then `k` applied to `x`: the rest under its delimiter `d` again, the handlers in force those
   * where the node runs — `ev`, those of when it was captured, kept if the ones outside are the same */
  private[zero] final case class Resume[RB <: Row, L, I0, V, A, X, T1, Y](x: X, f: X => Cont0[T1, Cont0[L, L, Res[RB, L, V]], Y],
      k: Frames[Y, Cont0[L, L, Res[RB, L, I0]], T1, A],
      d: Delim[RB, L, I0, V, A], ev: Ev[RB]) extends Cont0[L, L, V]

  /** AN OPERATION OF THE KERNEL'S (okay-kernel's `Op`, okay.core.kernel): no path — its handler found by its effect's
   * identity as it runs, answered in place by the handlers in force or routed to the delimiters; its value read for
   * a signal where it was performed (okay.core.kernel's `Machine.check`). Matched after every typed node */
  private[okay] final case class KOp[I, O, A](op: okay.kernel.Op[?]) extends Cont0[I, O, A]
  /** a kernel operation with the rest bound to its value as it is; `region`, an `uncancelable` region crossed */
  private[okay] final case class KOpK[I, O, A](op: okay.kernel.Op[?], region: Boolean, k: Any => Cont0[?, ?, ?]) extends Cont0[I, O, A]

  /** `c.flatMap(f)`: an operation bound one node, `OpK` */
  private[zero] def bind[I, O, A, I2, B](c: Cont0[I, O, A], f: A => Cont0[I2, I, B]): Cont0[I2, O, B] = c match
    case o: Op[r, j, t, e, x] => OpK[r, j, t, e, x, I2, B](o.op, o.in, f)
    case _ => Bind(c, f)

  def pure[S, A](a: A): Cont0[S, S, A] = Return(a)
  def shift0[I1, O1, X1, O, A](f: (A => Cont0[I1, O1, X1]) => O): Cont0[Cont0[I1, O1, X1], O, A] = Shift0(f)
  def dollar[I, I2, O2, B, A](ret: A => I)(body: Cont0[I, Cont0[I2, O2, B], A]): Cont0[I2, O2, B] = Dollar(body, ret)

  extension [J, I2, O2, B, A](body: Cont0[Cont0[J, J, A], Cont0[I2, O2, B], A])
    /** `pure $ body` */
    def reset0: Cont0[I2, O2, B] = Dollar(body, Return(_))

  /** the program to its head: binds followed, a delimiter entered — the machine's nodes by their shift0 programs,
   * a handler, a reset and a resumption by the machine, nothing known of the handlers around */
  private[zero] def step[I, T, O, X, A](c: Cont0[T, O, X], k: Frames[X, I, T, A]): Head[I, O, A] =
    go[I, T, O, X, A, Head[I, O, A]](c, k, KMeta.top)

  /** THE `$`s THE KERNEL'S LOOP IS INSIDE, innermost first — its metacontinuation: what takes the head of the
   * innermost's body, `Head[I, O, A]`, toward the loop's own result `Z`. At the top, the caller; under a `$`, its
   * `ret` and the frames outside it — gone on in the same loop. So `$`s nested to any depth run in constant stack */
  private[zero] enum KMeta[I, O, A, Z]:
    case Top[I, O, A]() extends KMeta[I, O, A, Head[I, O, A]]
    case Under[Ib, I2, O2, B, Ab, I, A, Z](ret: Ab => Ib, k: Frames[B, I, I2, A], up: KMeta[I, O2, A, Z])
      extends KMeta[Ib, Cont0[I2, O2, B], Ab, Z]
  private[zero] object KMeta:
    private val top0: KMeta[Any, Any, Any, Head[Any, Any, Any]] = Top()
    def top[I, O, A]: KMeta[I, O, A, Head[I, O, A]] = empty[KMeta[I, O, A, Head[I, O, A]]](top0)

  @tailrec private def go[I, T, O, X, A, Z](c: Cont0[T, O, X], k: Frames[X, I, T, A], meta: KMeta[I, O, A, Z]): Z =
    // the head of the innermost `$`'s body: to the caller; or, under a `$`, the value through its `ret`, a capture's
    // body in the `$`'s place, the rest under it again — on in the frames outside it
    inline def head(h: Head[I, O, A]): Z = meta match
      case _: KMeta.Top[i, o, a] => h
      case u: KMeta.Under[ib, i2, o2, b, ab, ii, aa, z] => h match
        case Head.Done(a) => go[ii, i2, o2, b, aa, Z](u.ret(a), u.k, u.up)
        case Head.Cut(f, kk) => go[ii, i2, o2, b, aa, Z](f(x => Dollar(resumed(x, kk), u.ret)), u.k, u.up)
    c match
    case b: Bind[i, t, o, a, bb] => b.m match
      case r: Return[s, aa] => go(b.f(r.a), k, meta)
      // a handler, a reset, an interceptor bound: entered at once — its value, the usual, bound with no frame pushed
      case dn: Node[r, j, tt, rb, i0, v, a2] => enterNode(dn, Ev.Unknown, 0) match
        case v: Return[s, vv] => go(b.f(v.a), k, meta)
        case m => go(m, Frames.Then(b.f, k), meta)
      case _ => go(b.m, Frames.Then(b.f, k), meta)
    case Return(a) => k match
      case Frames.End() => head(Head.Done(a))
      case Frames.Then(f, next) => go(f(a), next, meta)
      case mp: Frames.Mapped[a1, x1, i1, t1, b1] => mp.next match
        case Frames.End() => head(Head.Done(mp.f(a)))
        case th: Frames.Then[a2, x2, i2, t3, t4, b2] => go(th.f(mp.f(a)), th.next, meta)
        case mp2: Frames.Mapped[a2, x2, i2, t3, b2] => go(Return(mp2.f(mp.f(a))), mp2.next, meta)
    case Map(m, f) => go(m, Frames.Mapped(f, k), meta)
    case Delay(t) => go(t(), k, meta)
    case o: Op[r, j, t, e, x] => head(Head.Cut[I, j, j, Res[r, j, t], Cont0[j, j, Res[r, j, t]], x, A](kk => Return(Res.Req(o.op, o.in, kk)), k))
    case o: OpK[r, j, t, e, x, i, b] => head(Head.Cut[I, j, j, Res[r, j, t], Cont0[j, j, Res[r, j, t]], x, A](kk => Return(Res.Req(o.op, o.in, kk)), Frames.Then(o.k, k)))
    case l: Lift[rb, l, o, x] => head(Head.Cut[I, l, l, Res[rb, l, o], Cont0[l, l, Res[rb, l, o]], x, A](kk => Bind(l.m, kk), k))
    case c: Ctl[r, j, a, i, o] => head(Head.Cut[I, j, j, Res[r, j, i], Cont0[j, j, Res[r, j, o]], a, A](kk => Return(Res.Ctl(c.f, kk)), k))
    case dn: Node[r, j, t, rb, i0, v, a] => go(enterNode(dn, Ev.Unknown, 0), k, meta)
    case r: Resume[rb, l, i0, v, a, x, t1, y] => go(enterUnknown(r), k, meta)
    case c: Ctl0[r, j, t, a, s, o] => go(shift0Program(c), k, meta)
    case Shift0(f) => head(Head.Cut(f, k))
    // a `$`: its body in this loop, the frames outside it and its `ret` pushed
    case d: Dollar[ib, i2, o2, b, ab] => go[ib, ib, Cont0[i2, o2, b], ab, ab, Z](d.body, Frames.end, KMeta.Under[ib, i2, o2, b, ab, I, A, Z](d.ret, k, meta))
    // the kernel's operations, as an operation is: a request with the rest — read for a signal first, a `KOp`'s
    case o: KOp[t, o, x] => head(kernelCut[I, O, A](o.op, false, kcheck, k))
    case o: KOpK[t, o, x] => head(kernelCut[I, O, A](o.op, o.region, o.k, k))

  /** a kernel operation's head, nothing known of the handlers around: a request by its effect's identity, `f` then
   * `k` its rest. THE TRUSTED STEP of the kernel's nodes here: untyped, okay-core's types phantom at run time */
  private def kernelCut[I, O, A](op: okay.kernel.Op[?], region: Boolean, f: Any => Cont0[?, ?, ?], k: Frames[?, I, ?, A]): Head[I, O, A] =
    Head.Cut[I, Any, Any, Any, O, Any, A](
      kk => Return[Any, Any](Res.KReq[Pure, Any, Any](op, region, kk.asInstanceOf[Any => Cont0[Any, Any, Res[Pure, Any, Any]]])).asInstanceOf[O],
      Frames.Then[Any, Any, I, Any, Cont0[Any, Any, Any], A](f.asInstanceOf[Any => Cont0[Any, Cont0[Any, Any, Any], Any]], k.asInstanceOf[Frames[Any, I, Any, A]]))

  /** a kernel operation's value read for a signal — the machine's `check`, at this monad's type */
  private[zero] def kcheck: Any => Cont0[?, ?, ?] = ZeroMachine.check.asInstanceOf[Any => Cont0[?, ?, ?]]

  /** `x` into the rest `k` — LAZILY: a bind the loop takes when it gets to it, so applying `k` runs nothing of the
   * rest; an empty rest, the value itself */
  private[zero] def resumed[I, T, X, A](x: X, k: Frames[X, I, T, A]): Cont0[I, T, A] = k match
    case Frames.End() => Return(x)
    case _ => Bind(Return(x), k)

  extension [A](c: Cont0[A, A, A])
    /** a program at the top, its value — the top `identity $` */
    def value: A = step(c, Frames.end) match
      case Head.Done(a) => a
      case Head.Cut(f, k) => f(x => Dollar(resumed(x, k), identity))

/** A DELIMITER the machine runs: its body over `RB`, the outside at `L`, the body's value `A` to the answer `I0`, the
 * delimiter's answer `V` — a reset's or a handler's, the node itself or one the machine makes */
private[zero] sealed trait Delim[RB <: Row, L, I0, V, A]:
  /** the top of a loop entered at this delimiter */
  final def top: Meta[L, V, Cont0[L, L, V]] = Meta.top

/** a reset's: its body over `R`, written at `S`, every operation passed on */
private[zero] sealed trait ByReset[R <: Row, J, T, S, V] extends Delim[R, Cont0[J, J, Res[R, J, T]], S, V, S]

/** a handler's of `E`: its body over `E +: R`, its value to the answer by `ret` */
private[zero] sealed trait ByHandler[E[_], R <: Row, J, T, A, I0, V] extends Delim[E +: R, Cont0[J, J, Res[R, J, T]], I0, V, A]:
  /** its clauses, at the delimiter's answer */
  def clauses: Handler[[Y] =>> Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Y], E, ?, V]
  def ret(a: A): I0
  /** `ret` as a function — the one there is, passed on as it is, not wrapped once more each shift */
  def rets: A => I0

/** an interceptor's of `E` at `target`: its body over `R`, the row the same, its value to the answer by `ret` */
private[zero] sealed trait ByIntercept[E[_], R <: Row, J, T, A, V] extends Delim[R, Cont0[J, J, Res[R, J, T]], V, V, A]:
  def target: In[E, R]
  def clauses: Handler[[Y] =>> Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Y], E, ?, V]
  def ret(a: A): V
  def rets: A => V

/** a reset's, made by the machine: for a `shift`'s body under the reset again, and for its rest resumed */
private[zero] final case class Rst[R <: Row, J, T, S, V]() extends ByReset[R, J, T, S, V]
private[zero] object Rst:
  private val one: Rst[Pure, Any, Any, Any, Any] = Rst()
  def of[R <: Row, J, T, S, V]: Rst[R, J, T, S, V] = empty[Rst[R, J, T, S, V]](one)

/** a handler's, made by the machine: for a `shift`'s body under the handler again, its value the answer as it is,
 * and for its rest resumed, the body's value to the handler's answer by `r` */
private[zero] final case class Hd[E[_], R <: Row, J, T, A, I0, V](h: Handler[[Y] =>> Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Y], E, ?, V], r: A => I0)
    extends ByHandler[E, R, J, T, A, I0, V]:
  def clauses: Handler[[Y] =>> Cont0[Cont0[J, J, Res[R, J, T]], Cont0[J, J, Res[R, J, T]], Y], E, ?, V] = h
  def ret(a: A): I0 = r(a)
  def rets: A => I0 = r

/** THE ONE CAST: an empty case, no fields — `Frames.End`, `Meta.Top`, `KMeta.Top`, `Rst` — one object at any
 * types, not one each time. Its type parameters tie the types of what it is in, it holds no value of them: nothing
 * is ever read from it at a wrong type, and a match on it refines to the types the place it is used at gave it.
 * Its type always written: inferred, it is `Nothing`, and a cast to `Nothing` is a test that fails */
private[zero] inline def empty[E](e: AnyRef): E = e.asInstanceOf[E]

/** what a value goes through, innermost first; as a function, the rest */
private[zero] enum Frames[A, I, T, B] extends (A => Cont0[I, T, B]):
  case End[S, A]() extends Frames[A, S, S, A]
  case Then[A, X, I, T1, T, B](f: A => Cont0[T1, T, X], next: Frames[X, I, T1, B]) extends Frames[A, I, T, B]
  case Mapped[A, X, I, T, B](f: A => X, next: Frames[X, I, T, B]) extends Frames[A, I, T, B]

  def apply(a: A): Cont0[I, T, B] = this match
    case End() => Cont0.Return(a)
    // the rest bound after the first frame — none, the first frame's program itself: an empty stack bound as a
    // frame would be one more each resumption, nested
    case Then(f, next) => next match
      case End() => f(a)
      case _ => Cont0.Bind(f(a), next)
    case Mapped(f, next) => next match
      case End() => Cont0.Return(f(a))
      case _ => Cont0.Bind(Cont0.Return(f(a)), next)
private[zero] object Frames:
  private val end0: Frames[Any, Any, Any, Any] = End()
  def end[S, A]: Frames[A, S, S, A] = empty[Frames[A, S, S, A]](end0)

/** a value through the maps on top of the frames, at once: to the end, its value; to a bind, the program it makes */
private[zero] enum Fed[I, T, B]:
  case Value[S, B](b: B) extends Fed[S, S, B]
  case Next[X, I, T1, T, B](c: Cont0[T1, T, X], next: Frames[X, I, T1, B]) extends Fed[I, T, B]

@tailrec private[zero] def feed[A, I, T, B](a: A, k: Frames[A, I, T, B]): Fed[I, T, B] = k match
  case Frames.End() => Fed.Value(a)
  case th: Frames.Then[a, x, i, t1, t, b] => Fed.Next(th.f(a), th.next)
  case mp: Frames.Mapped[a, x, i, t, b] => feed(mp.f(a), mp.next)

/** a program's head: its value, or a capture with the rest */
private[zero] enum Head[I, O, A]:
  case Done[S, A](a: A) extends Head[S, S, A]
  case Cut[I, I1, O1, X1, O, X, A](f: (X => Cont0[I1, O1, X1]) => O, k: Frames[X, I, Cont0[I1, O1, X1], A]) extends Head[I, O, A]
