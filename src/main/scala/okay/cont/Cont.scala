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
enum Cont[R <: Row, I, O, A] extends okay.kernel.Prog[A]:
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
  case Reset[R <: Row, Q, S, O, A](body: Cont[R, S, O, A], ret: A => S) extends Cont[R, Q, Q, O] with ByReset[R, Q, S, O, A]
  /** `m` with the rest as a stack already: what applying a captured `k` makes — `Bind(m, k)`, where the loop can
   * take `k` as its stack as it is, not as a function to push. Only `Frames` make it */
  case Push[R <: Row, I, T, O, A, B](m: Cont[R, T, O, A], k: Frames[R, A, I, T, B]) extends Cont[R, I, O, B]
  /** a HANDLER of `E` over its body, a node: the body built when the loop enters it, its value to `S` by `ret`, an
   * operation of `E` given to `h`'s clause — answered in place if it answers so — another's on outside, one effect
   * shorter: the fold the loop does there */
  case Fold[E[_], T <: Row, Q, S, Ans, A](body: () => Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans],
      an: Value[E] | Null) extends Cont[T, Q, Q, Ans] with ByFold[E, T, Q, S, Ans, A]
  /** a handler's body resumed: `k` applied to `x` under the handler again — a clause's `k`, a forwarded operation's
   * rest — one node, no closure */
  case FoldK[E[_], T <: Row, Q, S, Ans, A, X](k: X => Cont[E +: T, S, Ans, A], x: X, ret: A => S, h: Handler[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans],
      an: Value[E] | Null) extends Cont[T, Q, Q, Ans] with ByFold[E, T, Q, S, Ans, A]
  /** an INTERCEPTOR of `E` at `in`, over its body, a node: the row the same, an operation at `in` given to `h`'s
   * clause, another on outside as it is */
  case Icept[E[_], R <: Row, Q, S, Ans, A](body: () => Cont[R, S, Ans, A], ret: A => S, in: In[E, R], h: Handler[[Z] =>> Cont[R, Q, Q, Z], E, ?, Ans])
    extends Cont[R, Q, Q, Ans] with ByIcept[E, R, Q, S, Ans, A]

  /** AN OPERATION OF THE KERNEL'S (okay-kernel's `Op`, okay.core.kernel): no path — its handler found by its effect's
   * identity as it runs, answered in place by the handlers in force or routed to the delimiters; its value read for
   * a signal where it was performed. Matched after every typed node */
  case KInject[R <: Row, S, X](op: okay.kernel.Op[?]) extends Cont[R, S, S, X]
  /** a kernel operation with the rest bound to its value as it is; `region`, an `uncancelable` region crossed */
  case KSuspend[R <: Row, I, O, A](op: okay.kernel.Op[?], region: Boolean, k: Any => Cont[R, I, O, A], checked: Boolean = false)
    extends Cont[R, I, O, A]

  def flatMap[I2, B](f: A => Cont[R, I2, I, B]): Cont[R, I2, O, B] = this match
    case i: Inject[e, r, s, x] => Cont.bound(i, f)
    case _ => Bind(this, f)
  def map[B](f: A => B): Cont[R, I, O, B] = Map(this, f)

/** A DELIMITER the loop reads: its body over `R`, of type `Cont[R, I, O, B]`, the program leaving it `P` — a node
 * itself (a reset, a handler, an interceptor), one the loop makes for a shift's body, or the top */
private[cont] sealed trait Delim[R <: Row, I, O, B, P]
/** a reset's: its body's value to `S` by `ret`, every operation passed on */
private[cont] sealed trait ByReset[R <: Row, Q, S, O, A] extends Delim[R, S, O, A, Cont[R, Q, Q, O]]:
  def ret: A => S
/** a handler's of `E`: its body over `E +: T`, its clauses `h`, `an` them as answering in place if they are */
private[cont] sealed trait ByFold[E[_], T <: Row, Q, S, Ans, A] extends Delim[E +: T, S, Ans, A, Cont[T, Q, Q, Ans]]:
  def ret: A => S
  def h: Handler[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans]
  def an: Value[E] | Null
/** an interceptor's of `E` at `in` */
private[cont] sealed trait ByIcept[E[_], R <: Row, Q, S, Ans, A] extends Delim[R, S, Ans, A, Cont[R, Q, Q, Ans]]:
  def ret: A => S
  def in: In[E, R]
  def h: Handler[[Z] =>> Cont[R, Q, Q, Z], E, ?, Ans]
/** the top: what leaves it is the head of the program, for `step`'s caller */
private[cont] final class TopOf[R <: Row, I, O, B]() extends Delim[R, I, O, B, Head[R, I, O, B]]
/** a reset's, a handler's, an interceptor's made by the loop for a shift's body run in their place: `ret` the
 * identity, the shift's body the delimiter's answer */
private[cont] final class Rs[R <: Row, Q, O]() extends ByReset[R, Q, O, O, O]:
  def ret: O => O = identity
private[cont] final class Hd[E[_], T <: Row, Q, Ans](val h: Handler[[Z] =>> Cont[T, Q, Q, Z], E, ?, Ans], val an: Value[E] | Null)
    extends ByFold[E, T, Q, Ans, Ans, Ans]:
  def ret: Ans => Ans = identity
private[cont] final class Ic[E[_], R <: Row, Q, Ans](val in: In[E, R], val h: Handler[[Z] =>> Cont[R, Q, Q, Z], E, ?, Ans])
    extends ByIcept[E, R, Q, Ans, Ans, Ans]:
  def ret: Ans => Ans = identity

/** THE HANDLERS IN FORCE around the loop as it runs, innermost first, one per effect of its row — or not known: an
 * operation then goes the way it always may, to the delimiters */
private[cont] enum Ev[+R <: Row]:
  case Unknown extends Ev[Nothing]
  case Entry[E[_], T <: Row](outer: Ev[T], answers: Value[E] | Null) extends Ev[E +: T]

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
    def step: Head[R, I, O, A] = run[R, I, I, O, A, A, Head[R, I, O, A], Head[R, I, O, A]](c, Frames.End(), TopOf(), Ev.Unknown, null, <:<.refl, 0)

  /** how deep the loop runs delimiters it enters nested in itself, a JVM frame each — the cheapest way, nothing
   * pushed; past it, pushed on the metacontinuation (`Meta`): any depth in constant stack */
  private inline val Deep = 32

  /** THE DELIMITERS OUTSIDE THE INNERMOST, pushed — the loop's METACONTINUATION: what takes the program leaving the
   * innermost one, `P` — run in the frames `k` under the delimiter `d` outside it, the handlers in force there `ev`.
   * None (`null`): the loop's caller takes it, `ex` the proof it is the loop's result */
  private[cont] enum Meta[P, Z]:
    case Under[R <: Row, I, T, O, A, B, P2, Z](k: Frames[R, A, I, T, B], d: Delim[R, I, O, B, P2], ev: Ev[R],
        up: Meta[P2, Z] | Null, ex: (P2 <:< Z) | Null) extends Meta[Cont[R, T, O, A], Z]

  /** THE LOOP: `c` with the frames `k` under the delimiter `d`, the handlers in force `ev`, the delimiters outside
   * `m` — what reaches the delimiter read by it, the program leaving it run outside; a delimiter met entered, nested
   * up to `Deep`, pushed past it; an operation whose handler answers in place answered where it is */
  @tailrec private[cont] def run[R <: Row, I, T, O, A, B, P, Z](c: Cont[R, T, O, A], k: Frames[R, A, I, T, B], d: Delim[R, I, O, B, P],
      ev: Ev[R], m: Meta[P, Z] | Null, ex: (P <:< Z) | Null, depth: Int): Z =
    // `p`, the program leaving the delimiter: to the caller, or on in the delimiter outside — the same loop
    inline def leave(p: P): Z = m match
      case null => ex.nn(p)
      case u: Meta.Under[r, i, t, o, a, b, p2, z] => run[r, i, t, o, a, b, p2, Z](p, u.k, u.d, u.ev, u.up, u.ex, depth)
    c match
    case Return(a) => k match
      case Frames.End() => leave(returned[R, I, B, P](a, d))
      case Frames.Then(f, next) => run(f(a), next, d, ev, m, ex, depth)
      case Frames.Mapped(f, next) => next match
        case Frames.End() => run(Return(f(a)), Frames.End(), d, ev, m, ex, depth)
        case Frames.Then(g, rest) => run(g(f(a)), rest, d, ev, m, ex, depth)
        case Frames.Mapped(g, rest) => run(Return(g(f(a))), rest, d, ev, m, ex, depth)
    case Inject(op, in) => inPlace(in, ev) match
      case null => leave(performed(op, in, k, d))
      // its value straight into the frame bound to it, the usual — no `Return` made to carry it there
      case v => k match
        case th: Frames.Then[r1, a1, x1, i1, t1, t2, b1] => run(th.f(v.value(op)), th.next, d, ev, m, ex, depth)
        case _ => run(Return(v.value(op)), k, d, ev, m, ex, depth)
    case sp: Suspend[e0, r0, i0, o0, x, a0] => inPlace(sp.in, ev) match
      case null =>
        // the rest, the operation's own then the frames: made only where the operation leaves
        val g: x => Cont[R, I, O, B] = k match
          case Frames.End() => sp.k
          case _ => Frames.Then(sp.k, k)
        leave(performed(sp.op, sp.in, g, d))
      case v => run(sp.k(v.value(sp.op)), k, d, ev, m, ex, depth)
    // a capture: up to the innermost delimiter — its body in place of the rest, under the delimiter again, the rest
    // under a delimiter of its own with `ret` as `k`
    case Shift(f) => d match
      case _: TopOf[r, i, o, b] => leave(Head.Cut(f, k))
      case dr: ByReset[r, q, s, o, a1] => run(f(under(k, dr.ret)), Frames.End(), Rs[r, q, o](), ev, m, ex, depth)
      case df: ByFold[e, t, q, s, ans, a1] => run(f(under(k, df.ret)), Frames.End(), Hd[e, t, q, ans](df.h, df.an), ev, m, ex, depth)
      case di: ByIcept[e, r, q, s, ans, a1] => run(f(under(k, di.ret)), Frames.End(), Ic[e, r, q, ans](di.in, di.h), ev, m, ex, depth)
    // a delimiter: entered — nested, a JVM frame, its program run on here; past `Deep`, this loop pushed under it
    case rs: Reset[r, q, s, o, a1] =>
      if depth < Deep then
        run(nested[r, s, s, o, a1, a1, Cont[r, q, q, o], Cont[r, q, q, o]](rs.body, Frames.End(), rs, ev, null, <:<.refl, depth + 1), k, d, ev, m, ex, depth)
      else run[r, s, s, o, a1, a1, Cont[r, q, q, o], Z](rs.body, Frames.End(), rs, ev, Meta.Under(k, d, ev, m, ex), null, depth)
    case fo: Fold[e, t, q, s, ans, a1] =>
      val inner = within(fo.an, ev)
      if depth < Deep then
        run(nested[e +: t, s, s, ans, a1, a1, Cont[t, q, q, ans], Cont[t, q, q, ans]](fo.body(), Frames.End(), fo, inner, null, <:<.refl, depth + 1), k, d, ev, m, ex, depth)
      else run[e +: t, s, s, ans, a1, a1, Cont[t, q, q, ans], Z](fo.body(), Frames.End(), fo, inner, Meta.Under(k, d, ev, m, ex), null, depth)
    case fo: FoldK[e, t, q, s, ans, a1, x] =>
      val inner = within(fo.an, ev)
      if depth < Deep then
        run(nested[e +: t, s, s, ans, a1, a1, Cont[t, q, q, ans], Cont[t, q, q, ans]](fo.k(fo.x), Frames.End(), fo, inner, null, <:<.refl, depth + 1), k, d, ev, m, ex, depth)
      else run[e +: t, s, s, ans, a1, a1, Cont[t, q, q, ans], Z](fo.k(fo.x), Frames.End(), fo, inner, Meta.Under(k, d, ev, m, ex), null, depth)
    case ic: Icept[e, r, q, s, ans, a1] =>
      val inner = masked(ev, ic.in)
      if depth < Deep then
        run(nested[r, s, s, ans, a1, a1, Cont[r, q, q, ans], Cont[r, q, q, ans]](ic.body(), Frames.End(), ic, inner, null, <:<.refl, depth + 1), k, d, ev, m, ex, depth)
      else run[r, s, s, ans, a1, a1, Cont[r, q, q, ans], Z](ic.body(), Frames.End(), ic, inner, Meta.Under(k, d, ev, m, ex), null, depth)
    case Delay(t) => run(t(), k, d, ev, m, ex, depth)
    case Map(mm, f) => run(mm, Frames.Mapped(f, k), d, ev, m, ex, depth)
    case Push(mm, ks) => k match
      case Frames.End() => run(mm, ks, d, ev, m, ex, depth)
      case _ => run(mm, Frames.Then(ks, k), d, ev, m, ex, depth)
    case Bind(mm, g) => mm match
      case Return(a) => run(g(a), k, d, ev, m, ex, depth)
      case _ => run(mm, Frames.Then(g, k), d, ev, m, ex, depth)
    // the kernel's operations, by their effect's identity: answered in place by the nearest handler in force taking
    // it, if it can; else to the delimiter, as an operation is — after every typed node, costing them nothing
    // (`KInject`, `KSuspend`: one case, the loop kept small)
    case _ => kernelInPlace(c, ev) match
      case null => leave(kernelPerformed(c, kernelThen(kernelRest(c), k), d))
      case h => run(trustK[Cont[R, T, O, A]](kernelAnswered(c, h)), k, d, ev, m, ex, depth)

  /** THE TRUSTED STEP of the kernel's nodes in the loop: untyped, okay-core's types phantom at run time */
  private inline def trustK[C](p: Any): C = p.asInstanceOf[C]

  /** a kernel operation's value read for a signal — the machine's `check`, at this monad's type */
  private def kcheck: Any => Cont[?, ?, ?, ?] = ContMachine.check.asInstanceOf[Any => Cont[?, ?, ?, ?]]

  /** the kernel operation of a kernel node */
  private def kernelOp(c: Cont[?, ?, ?, ?]): okay.kernel.Op[?] = c match
    case ks: KSuspend[?, ?, ?, ?] => ks.op
    case ki: KInject[?, ?, ?] => ki.op
    case _ => throw IllegalStateException("not a kernel operation")

  /** a kernel operation's rest, leaving the loop: its value read first if it is to be */
  private def kernelRest(c: Cont[?, ?, ?, ?]): Any => Cont[?, ?, ?, ?] = c match
    case ks: KSuspend[?, ?, ?, ?] =>
      if ks.checked then trustK[Any => Cont[?, ?, ?, ?]](ContMachine.checked(trustK[Any => AnyRef](ks.k))) else ks.k
    case _ => kcheck

  /** a kernel operation answered in place by `h`: the rest with its value — read first if it is to be */
  private def kernelAnswered(c: Cont[?, ?, ?, ?], h: okay.core.kernel.KHandler): AnyRef = c match
    case ks: KSuspend[?, ?, ?, ?] =>
      val v = h.value(ks.op)
      if ks.checked then ContMachine.checkThen(v, trustK[Any => AnyRef](ks.k)) else ks.k(v)
    case ki: KInject[?, ?, ?] => ContMachine.check(h.value(ki.op))
    case _ => throw IllegalStateException("not a kernel operation")

  /** `f` then the frames `k`: a kernel operation's rest */
  private def kernelThen[R <: Row, A, I, T, B](f: Any => Cont[?, ?, ?, ?], k: Frames[R, A, I, T, B]): Any => Cont[?, ?, ?, ?] = k match
    case Frames.End() => f
    case _ => trustK[Any => Cont[?, ?, ?, ?]](Frames.Then[R, Any, A, I, T, Any, B](trustK[Any => Cont[R, T, Any, A]](f), k))

  /** the nearest handler in force taking the kernel operation, if it answers it in place; `HereOp`, the offer of
   * those known to a foreign computation */
  private def kernelInPlace(c: Cont[?, ?, ?, ?], ev: Ev[?]): okay.core.kernel.KHandler | Null =
    val op = kernelOp(c)
    if op eq okay.core.kernel.HereOp then okay.core.kernel.Machine.here(kernelHandlers(ev))
    else
      val t = op.effect()
      val internal = ContMachine.internal(t)
      var e: Ev[?] = ev
      while e.isInstanceOf[Ev.Entry[?, ?]] do
        val en = e.asInstanceOf[Ev.Entry[?, ?]]
        en.answers match
          case h: okay.core.kernel.KHandler => h.decide(op, t, internal) match
            case 0 =>
            case 1 => return h
            case _ => return null
          case _ =>
        e = en.outer
      null

  private def kernelHandlers(ev: Ev[?]): List[okay.core.kernel.KHandler] = ev match
    case en: Ev.Entry[?, ?] => en.answers match
      case h: okay.core.kernel.KHandler => h :: kernelHandlers(en.outer)
      case _ => kernelHandlers(en.outer)
    case _ => Nil

  /** the program leaving the delimiter `d` with a kernel operation not answered in place, `g` the rest: at a handler
   * that takes it, its clause, `k` the rest under it again; else on outside, the rest under the delimiter again. At
   * the top, none: a kernel program runs under its computation's top handler */
  private def kernelPerformed[R <: Row, I, O, B, P](c: Cont[?, ?, ?, ?], g: Any => Cont[?, ?, ?, ?], d: Delim[R, I, O, B, P]): P =
    val op = kernelOp(c)
    val region = c match
      case ks: KSuspend[?, ?, ?, ?] => ks.region
      case _ => false
    d match
      case _: TopOf[r, i, o, b] => throw IllegalStateException(s"a kernel operation with no handler: $op")
      case dr: ByReset[r, q, s, o, a1] => trustK[P](KSuspend[r, q, q, o](op, region, Redelimit[r, q, s, o, a1, Any](trustK[Any => Cont[r, s, o, a1]](g), dr.ret)))
      case df: ByFold[e, t, q, s, ans, a1] =>
        val rest = trustK[Any => Cont[e +: t, s, ans, a1]](g)
        df.h match
          case kh: okay.core.kernel.KHandler =>
            if kh.accepts(op) then trustK[P](kh.clause(op, trustK[Any => AnyRef](Resume[e, t, q, s, ans, a1, Any](rest, df)), region))
            else trustK[P](KSuspend[t, q, q, ans](op, region || kh.region, Refold[e, t, q, s, ans, a1, Any](rest, df)))
          case _ => trustK[P](KSuspend[t, q, q, ans](op, region, Refold[e, t, q, s, ans, a1, Any](rest, df)))
      case di: ByIcept[e, r, q, s, ans, a1] => trustK[P](KSuspend[r, q, q, ans](op, region, Reintercept[e, r, q, s, ans, a1, Any](trustK[Any => Cont[r, s, ans, a1]](g), di)))

  /** the program leaving the delimiter `d` with the value `a`: its `ret` applied — out of `run`, so the loop stays
   * small enough for the JIT to compile it whole */
  private def returned[R <: Row, S, B, P](a: B, d: Delim[R, S, S, B, P]): P = d match
    case _: TopOf[r, i, o, b] => Head.Done[R, S, B](a)
    case dr: ByReset[r, q, s, o, a1] => Return[r, q, o](dr.ret(a))
    case df: ByFold[e, t, q, s, ans, a1] => Return[t, q, ans](df.ret(a))
    case di: ByIcept[e, r, q, s, ans, a1] => Return[r, q, ans](di.ret(a))

  /** the program leaving the delimiter `d` with an operation not answered in place, `g` the rest: to the head at the
   * top, on outside past a reset, to the clause at a handler of its effect or on outside one effect shorter, to the
   * clause at an interceptor at its path or on outside */
  private def performed[E[_], R <: Row, I, O, B, P, X](op: E[X], in: In[E, R], g: X => Cont[R, I, O, B], d: Delim[R, I, O, B, P]): P = d match
    case _: TopOf[r, i, o, b] => in.op(op, g)
    case dr: ByReset[r, q, s, o, a1] => in.bind(op, Redelimit[r, q, s, o, a1, X](g, dr.ret))
    case df: ByFold[e, t, q, s, ans, a1] => in match
      case In.Here() => df.h(op, Resume[e, t, q, s, ans, a1, X](g, df))
      case In.There(out) => out.bind(op, Refold[e, t, q, s, ans, a1, X](g, df))
    case di: ByIcept[e, r, q, s, ans, a1] => In.same(in, di.in) match
      case null => in.bind(op, Reintercept[e, r, q, s, ans, a1, X](g, di))
      case same => di.h(same(op), Reintercept[e, r, q, s, ans, a1, X](g, di))

  /** a delimiter's body run nested, a JVM frame — up to `Deep` of them — its leaving program the result */
  private def nested[R <: Row, I, T, O, A, B, P, Z](c: Cont[R, T, O, A], k: Frames[R, A, I, T, B], d: Delim[R, I, O, B, P],
      ev: Ev[R], m: Meta[P, Z] | Null, ex: (P <:< Z) | Null, depth: Int): Z = run(c, k, d, ev, m, ex, depth)

  /** the handler at the path, if it answers in place — through the handlers in force; not known so, `null`. The
   * first handler's at once; one further out by the path's last lookup, if from these handlers: the same path
   * through the same handlers finds the same — a loop's operations come with one path, so a lookup across any number
   * of handlers is a compare (`In.found`, as `okay.zero`'s machine) */
  private def inPlace[E[_], R <: Row](in: In[E, R], ev: Ev[R]): Value[E] | Null = ev match
    case e: Ev.Entry[e, t] => in match
      case In.Here() => e.answers
      case _ =>
        val f = in.found
        if f != null && (f.from eq e) then f.answers
        else
          val v = walk(in, ev)
          in.found = In.Found(e, v)
          v
    case _ => null
  @tailrec private def walk[E[_], R <: Row](in: In[E, R], ev: Ev[R]): Value[E] | Null = ev match
    case e: Ev.Entry[e, t] => in match
      case In.Here() => e.answers
      case In.There(out) => walk(out, e.outer)
    case _ => null

  /** the handlers in force inside a handler that answers in place by `an`, if it does: it in front — or, if it does
   * not and nothing is known outside, nothing known still */
  private def within[E[_], R <: Row](an: Value[E] | Null, ev: Ev[R]): Ev[E +: R] = an match
    case null => ev match
      case Ev.Unknown => Ev.Unknown
      case _ => Ev.Entry(ev, null)
    case _ => Ev.Entry(ev, an)

  /** the handlers in force inside an interceptor at `in`: the one at `in` answering nothing in place — its
   * operations go to the interceptor. A walk as long as the path */
  private def masked[E[_], R <: Row](ev: Ev[R], in: In[E, R]): Ev[R] = ev match
    case e: Ev.Entry[e, t] => in match
      case In.Here() => Ev.Entry[e, t](e.outer, null)
      case In.There(out) => Ev.Entry[e, t](masked(e.outer, out), e.answers)
    case _ => ev

  /** a handler's clauses as one that answers in place, if it is one */
  private[cont] def answers[P[_], E[_]](h: Handler[P, E, ?, ?]): Value[E] | Null = h match
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
  private final class Resume[E[_], T <: Row, Q, S, Ans, A, X](k: X => Cont[E +: T, S, Ans, A], d: ByFold[E, T, Q, S, Ans, A])
      extends (X => Cont[T, Q, Q, Ans]):
    def apply(x: X): Cont[T, Q, Q, Ans] = FoldK(k, x, d.ret, d.h, d.an)
  /** the rest of a handler's body, once an operation it passed on is answered outside: under the handler again */
  private final class Refold[E[_], T <: Row, Q, S, Ans, A, X](k: X => Cont[E +: T, S, Ans, A], d: ByFold[E, T, Q, S, Ans, A])
      extends (X => Cont[T, Q, Q, Ans]):
    def apply(x: X): Cont[T, Q, Q, Ans] = FoldK(k, x, d.ret, d.h, d.an)
  /** the rest of an interceptor's body: under the interceptor again */
  private final class Reintercept[E[_], R <: Row, Q, S, Ans, A, X](k: X => Cont[R, S, Ans, A], d: ByIcept[E, R, Q, S, Ans, A])
      extends (X => Cont[R, Q, Q, Ans]):
    def apply(x: X): Cont[R, Q, Q, Ans] = Icept(() => k(x), d.ret, d.in, d.h)

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
