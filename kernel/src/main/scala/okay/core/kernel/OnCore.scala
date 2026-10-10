package okay.core.kernel

import okay.core.{In, Row, Top as CoreTop}
import okay.kernel.{Aborting, Answering, Answers, Computation, Effects, Fn, General, Handler, Op, Prog, Program, Resume,
  Returning, Step, Thunk, Type}
import scala.util.control.NonFatal

/** the kernel's operations, every one, as ONE effect of okay-core's — the kernel's `Op` itself, no wrapper. Its
 * handlers are okay-core's handlers of `Dyn`; which one an operation goes to is its path, found by the bridge */
type Dyn[X] = Op[X]

/**
 * OKAY-CORE AS AN IMPLEMENTATION OF THE KERNEL: okay-kernel's `Effects` (and `Direct`) on okay-core's machines —
 * `OnCore.zero` on Cont0's, `OnCore.cont` on Cont's. Nothing of the kernel in okay-core itself: this module is the
 * bridge.
 *
 * HOW. A kernel program (`Prog`) is a small tree of the bridge's nodes, context-free as the kernel's programs are;
 * it is translated into okay-core's program where it is placed — at a context `Ctx`: okay-core's context there, the
 * bridge's handlers in force there (`Slot`s, innermost first), the host. A bind's function is translated when the
 * bind runs, a handler's body when the handler is entered: so the translation is lazy and a run is okay-core's own
 * loop, in constant stack. Translating where it is placed is what makes `shift0` exact: its body, written with the
 * same `Effects` as the code around it, runs one delimiter out, and is translated there.
 *
 *  - An operation goes to the nearest handler of its effect BY IDENTITY (`Type`): its index among the slots is its
 *    okay-core path, `In` — `There` that many times, then `Here`.
 *  - Handlers, by shape: `Answering` is okay-core's `Answering`, answered in place by its machine; `Aborting` is an
 *    okay-core `Answering` too, its value a signal that unwinds to the handler (below); `General` an okay-core
 *    handler whose clause is the kernel's, `k` okay-core's. `Returning`'s return is okay-core's `ret` itself — its
 *    answer a value, nothing flattened; a `General` return (a program) makes the handler's okay-core answer a program,
 *    run when the handler is left (flattened: one bind).
 *  - `shift0` and `$` are the bridge's own: `$` an okay-core handler whose return is `k`, `shift0` an operation to
 *    the nearest delimiter — a `$` or a kernel handler — whose clause is the body with okay-core's `k` (deep: the
 *    delimiter included). Not okay-core's `shift`/`reset`/`shift0`: the bridge's own delimiters (`recover`,
 *    `ensuring`, `uncancelable`, the abort's catcher) are okay-core handlers, and okay-core's control would stop at
 *    them; the kernel's does not. An `Answering` handler has a delimiter of the bridge around it for that (`Delim`):
 *    one okay-core handler more, transparent to everything but `shift0`.
 *  - FAILURES, ABORTS, CANCELLATION: okay-core has none of them as primitives, so they are effects of the bridge —
 *    an operation `Unwind` to the nearest of `recover`, `ensuring`, an `Aborting` handler's catcher, the top: each
 *    takes what it concerns and performs the rest again outward (its clause runs outside it), `k` dropped. An
 *    exception of the program's functions — a bind's, a delay's, a clause, an `Answering`'s value, a `Program.in` —
 *    is caught where the bridge calls it and becomes an `Unwind` there. What a handler or the host answers at an
 *    operation can be a SIGNAL (a failure, an abort, a cancellation, a cancellation pending): the operation checks
 *    its value and unwinds from where it was performed — `k.fail`, `k.cancel`, an abort with its finalizers.
 *  - EXPORT (`computation`): the program under the bridge's top handler on okay-core's top. An operation no handler
 *    of which is in force: answered in place by the host if it answers (no okay-core operation at all), else
 *    performed to the top handler, whose clause answers `Step.Yield` with okay-core's `k` as the `Resume` — run again
 *    on okay-core's top for each resumption: multi-shot, from any thread. IMPORT (`foreign`): another's computation
 *    started where it is placed, its yields performed here as operations of this program.
 *
 * WHAT IS APPROXIMATED, honestly: an exception of a `Returning` handler's `answer` (okay-core's `ret`, which is a
 * value, not a program) is not catchable by `recover`: it escapes okay-core's run and is the step's `Failed`; a
 * foreign computation cancelled by an abort or a cancellation crossing it: its own cancellation's steps are not
 * answered (as the reference machine does); a `k` resumed under other handlers than it was made under runs under
 * forwarders (`rebased`) — exact, but its operations through them are not answered in place, and a foreign
 * computation inside it is offered in place the answers of the handlers it was made under. The rest is the
 * reference machine's semantics (okay-kernel's `Steps`), checked by the same suites and the TCK.
 */
final class OnCore private (name: String, val top: CoreTop) extends Effects with okay.kernel.Direct:
  import OnCore.*

  override def toString: String = s"OnCore.$name"

  // -- Effects: the nodes

  def pure[A](a: A): Prog[A] = PureN[A](a)
  def flatMap[A, B](p: Prog[A], f: Fn[A, Prog[B]]): Prog[B] = BindN(node(p), f)
  override def map[A, B](p: Prog[A], f: Fn[A, B]): Prog[B] = MapN(node(p), f)
  def delay[A](p: Thunk[Prog[A]]): Prog[A] = DelayN[A](p)
  def perform[X](op: Op[X]): Prog[X] = PerformN[X](op)
  def handle[A, R](h: Handler[A, R], body: Program[A]): Prog[R] = HandleN[R](h, body)
  def shift0[A, I, O](body: Fn[Fn[A, Prog[I]], Prog[O]]): Prog[A] = Shift0N[A](body)
  def dollar[A, I, O](body: Program[A], k: Fn[A, Prog[I]]): Prog[O] = DollarN[O](body, k)
  def failed[A](e: Throwable): Prog[A] = FailedN[A](e)
  def recover[A](body: Program[A], h: Fn[Throwable, Prog[A]]): Prog[A] = RecoverN(body, h)
  def ensuring[A](body: Program[A], finalizer: Program[Void]): Prog[A] = EnsuringN(body, finalizer)
  def uncancelable[A](body: Program[A]): Prog[A] = UncancelableN(body)
  def foreign[A](c: Computation[A]): Prog[A] = ForeignN(c)

  // -- Direct: the same nodes, untyped — a kernel `Prog` is already the bridge's node, no wrapper to skip; a bind's
  // function passed as it is

  def directPure(a: Object): Object = PureN[Any](a)
  def directFlatMap(p: Object, f: Fn[Object, Object]): Object = BindN[Any, Any](node(p), x => prog[Any](f(x.asInstanceOf[Object])))
  def directMap(p: Object, f: Fn[Object, Object]): Object = MapN[Any, Any](node(p), x => f(x.asInstanceOf[Object]))
  def directDelay(p: Thunk[Object]): Object = DelayN[Any](p)
  def directPerform(op: Op[?]): Object = PerformN[Any](op)
  def directHandle(h: Handler[?, ?], body: Program[?]): Object = HandleN[Any](h, body)
  def prog[A](p: Object): Prog[A] = node(p) match
    case n: Node[A @unchecked] => n
  def unprog(p: Prog[?]): Object = node(p)

  // -- export

  def computation[A](program: Program[A], row: java.util.Set[Type]): Computation[A] =
    val fixed = java.util.Collections.unmodifiableSet(new java.util.HashSet[Type](row))
    new Computation[A]:
      def start(host: Answers): Step[A] = trust(runTop(root(program, host)))
      def effects(): java.util.Set[Type] = fixed

  /** the program under the top handler, at okay-core's top: an operation reaching it handed out, an unwinding
   * ended there */
  private def root(program: Program[?], host: Answers): Any =
    val x0 = Ctx(top.at[Any], Nil, -1, host)
    x0.handle(Slot.Host, hostClauses(x0))(z => in(z, program))

  private def hostClauses(x0: Ctx): Clauses = new Clauses:
    override def ret(v: Any): Any = new Step.Done[Any](v)
    def clause(op: Op[?], k: Any => Any): Any = x0.pure(op match
      case u: Unwind => u.why match
        case f: FailSig => new Step.Failed[Any](f.e)
        case CancelSig => new Step.Cancelled[Any]()
        case _ => new Step.Failed[Any](new IllegalStateException("an abort with its handler gone"))
      case o: Outward => new Step.Yield[Any, Any](trust(o.op), resumption(k, o, pending = false))
      case _ => new Step.Failed[Any](new IllegalStateException(s"an operation of the bridge with nothing for it: $op")))

  /** okay-core's `k` of an operation handed out, as the protocol's: each call a run of it on okay-core's top. Inside
   * an `uncancelable` region a cancellation waits: the same operation again, the cancellation pending — each value
   * given then marks it to its region, which cancels when it ends */
  private def resumption(k: Any => Any, o: Outward, pending: Boolean): Resume[Any, Any] = new Resume[Any, Any]:
    private def on(v: Any): Step[Any] = runTop(k(if pending then PendingSig(v) else v))
    def resume(x: Any): Step[Any] = on(x)
    def fail(e: Throwable): Step[Any] = on(FailSig(e))
    def cancel(): Step[Any] =
      if o.uncancelable then new Step.Yield[Any, Any](trust(o.op), resumption(k, o, pending = true)) else on(CancelSig)

  /** a run on okay-core's top: the top handler's answer, a step; an exception escaping okay-core's run (a
   * `Returning`'s `answer`), a failure */
  private def runTop(raw: Any): Step[Any] =
    try trust[Step[Any]](top.run[Any](trust[top.M[Any, Any, Any, Any]](raw)))
    catch case NonFatal(e) => new Step.Failed[Any](e)

  // -- the translation: a node, placed at `x`

  private def build(n: Node[?], x: Ctx): Any = n match
    case p: PureN[?] => x.pure(p.a)
    case _: BindN[?, ?] | _: MapN[?, ?] => spine(n, x)
    case d: DelayN[?] => x.delay(guard(x, d.t.get()))
    case p: PerformN[?] => perform(x, p.op)
    case h: HandleN[?] => handle(x, h.h, h.body)
    case s: Shift0N[?] => shift0(x, s.body)
    case d: DollarN[?] => dollar(x, d.body, d.k)
    case f: FailedN[?] => x.unwind(FailSig(f.e))
    case r: RecoverN[?] => recover(x, r.body, r.h)
    case e: EnsuringN[?] => ensuring(x, e.body, e.fin)
    case u: UncancelableN[?] => uncancelable(x, u.body)
    case f: ForeignN[?] => x.delay(drive(x, safely(f.c.start(answersHere(x)))))
    case b: Bound[?] => rebased(x, b.raw, b.effs)

  /** binds and maps nested to the left, a loop: the innermost first, then each function around it */
  private def spine(n0: Node[?], x: Ctx): Any =
    var n: Node[?] = n0
    var fs: List[Node[?]] = Nil
    while n.isInstanceOf[BindN[?, ?]] || n.isInstanceOf[MapN[?, ?]] do
      fs = n :: fs
      n = n match
        case b: BindN[?, ?] => b.p
        case m: MapN[?, ?] => m.p
        case _ => n
    var raw = build(n, x)
    while fs.nonEmpty do
      raw = fs.head match
        case b: BindN[a, ?] => x.flatMap(raw, v => guard(x, b.f.apply(trust[a](v))))
        // a map's function may throw: a bind, so its failure is the program's (okay-core's map would let it escape)
        case m: MapN[a, ?] => x.flatMap(raw, v => try x.pure(m.f.apply(trust[a](v))) catch case NonFatal(e) => x.unwind(FailSig(e)))
        case _ => raw
      fs = fs.tail
    raw

  /** a program the user's code gives, placed at `x`: its exception a failure there */
  private def guard(x: Ctx, p: => Any): Any =
    try build(node(p), x) catch case NonFatal(e) => x.unwind(FailSig(e))

  /**
   * A PROGRAM OF OKAY-CORE'S made under the handlers `from` — a `k` — placed at `x`. Under the same handlers (by
   * shape: their effects and kinds) it is itself. Elsewhere — a `k` resumed further out than it was made, under other
   * handlers — its paths are `from`'s: it runs under a forwarding handler for each of `from`'s it may reach (those of
   * them `x` has not, if `x`'s are the outer part of `from`; else all), each performing again here, by the kernel's
   * rules, what reaches it, and resuming it with the answer. Exact; slower — no answer in place through them
   */
  private def rebased(x: Ctx, raw: Any, from: List[Slot]): Any =
    val (n, m) = (from.length, x.effs.length)
    val forwarders = if n >= m && sameShape(from.drop(n - m), x.effs) then n - m else n
    def under(y: Ctx, left: Int): Any =
      if left == 0 then raw
      else y.handle(Slot.Forward, new Clauses:
        def clause(op: Op[?], k: Any => Any): Any = y.flatMap(again(y, op), k)
      )(z => under(z, left - 1))
    under(x, forwarders)

  private def sameShape(a: List[Slot], b: List[Slot]): Boolean =
    a.corresponds(b)((s, t) => (s.effect eq t.effect) && s.delim == t.delim && s.unwind == t.unwind && s.region == t.region &&
      (s.answering == null) == (t.answering == null))

  /** an operation performed again at `y`, its answer as it is — a signal read where it was first performed */
  private def again(y: Ctx, op: Op[?]): Any = op match
    case u: Unwind => y.unwind(u.why)
    case s: Shift0Op =>
      val i = y.index(_.delim)
      if i < 0 then y.unwind(FailSig(IllegalStateException("shift0 with no delimiter around it"))) else y.performAt(s, i)
    case Mark => y.mark
    case o: Outward =>
      val i = y.find(o.op.effect())
      if i >= 0 then y.performAt(o.op, i) else y.delay(outward(y, o.op, check = false, o.uncancelable || y.inRegion))
    case _ =>
      val i = y.find(op.effect())
      if i >= 0 then y.performAt(op, i) else y.delay(outward(y, op, check = false, y.inRegion))

  /** a `Program`, built where it is placed — `in` called then, as the reference machine calls it */
  private def in(x: Ctx, p: Program[?]): Any = guard(x, p.in(this))

  // -- an operation

  private def perform(x: Ctx, op: Op[?]): Any =
    val i = x.find(op.effect())
    if i >= 0 then x.checked(x.performAt(op, i)) else x.delay(outward(x, op, check = true, x.inRegion))

  /** an operation with no handler of it in force: the host's in place, if it answers it — no okay-core operation —
   * else handed out to the top. `check`: its value read for a signal (not for a foreign computation's, which reads
   * it itself); `region`, whether a region a cancellation waits for is around it */
  private def outward(x: Ctx, op: Op[?], check: Boolean, region: Boolean): Any =
    if x.host.answers(op.effect()) then
      try x.pure(x.host.value(op))
      catch case NonFatal(e) => if check then x.unwind(FailSig(e)) else x.pure(FailSig(e))
    else
      val raw = x.performAt(Outward(op, region), x.depth)
      if check then x.checked(raw) else raw

  // -- handlers

  private def handle(x: Ctx, h: Handler[?, ?], body: Program[?]): Any =
    val t = h.effect()
    val g: General[Any, Any] | Null = h match
      case g: General[?, ?] => trust[General[Any, Any]](g)
      case _ => null
    // a return that is a program: the okay-core handler's answer that program, run when it is left
    val flat = g != null && !h.isInstanceOf[Returning[?, ?]]
    def rets(y: Ctx): Any => Any = h match
      case r: Returning[?, ?] => v => trust[Returning[Any, Any]](r).answer(v)
      case _ if g != null => v => guard(y, g.ret(v, this))
      case _ => v => v
    h match
      case a: Answering[?, ?] =>
        val an = trust[Answering[Any, Any]](a)
        x.handle(Slot.Delim, delimiter(x)) { y =>
          flatten(y, flat, y.handle(Slot(t, false, false, false, an), inPlace(rets(y),
            op => try an.value(trust(op)) catch case NonFatal(e) => FailSig(e)))(z => in(z, body)))
        }
      case a: Aborting[?, ?] =>
        val ab = trust[Aborting[Any, Any]](a)
        val catcher = abortCatcher(x)
        x.handle(Slot.Catcher, catcher) { y =>
          flatten(y, flat, y.handle(Slot(t, false, false, false, null), inPlace(rets(y),
            op => try AbortSig(catcher, ab.abort(trust(op))) catch case NonFatal(e) => FailSig(e)))(z => in(z, body)))
        }
      case _ if g != null =>
        val returned = rets(x)
        flatten(x, flat, x.handle(Slot(t, true, false, false, null), new Clauses:
          override def ret(v: Any): Any = returned(v)
          def clause(op: Op[?], k: Any => Any): Any = op match
            case s: Shift0Op => shift0Clause(x, s.body, k, flat)
            case _ => answered(x, flat, guard(x, g.apply[Any](trust(op), resumed(x, k, flat), OnCore.this)))
        )(z => in(z, body)))
      case _ => x.unwind(FailSig(IllegalStateException(s"a handler of no shape: $h")))

  /** a handler's answer, a program when its return is one: run */
  private def flatten(x: Ctx, flat: Boolean, raw: Any): Any = if flat then x.flatMap(raw, p => p) else raw

  /** okay-core's `k` of a handler at `x`, as the kernel's: its program, made under `x`'s handlers, flattened as the
   * handler's answer is */
  private def resumed(x: Ctx, k: Any => Any, flat: Boolean): Fn[Any, Prog[Any]] =
    v => Bound[Any](flatten(x, flat, k(v)), x.effs)

  /** a clause's program, as the okay-core handler's answer: itself, or — the answer a program — that program */
  private def answered(x: Ctx, flat: Boolean, raw: Any): Any = if flat then x.pure(raw) else raw

  /** a `shift0` at its delimiter: the body in the delimiter's place, `k` the rest and the delimiter */
  private def shift0Clause(x: Ctx, body: Fn[Fn[Any, Prog[Any]], Prog[Any]], k: Any => Any, flat: Boolean): Any =
    answered(x, flat, guard(x, body.apply(resumed(x, k, flat))))

  /** the delimiter around an `Answering` handler: `shift0`'s, transparent to the rest */
  private def delimiter(x: Ctx): Clauses = new Clauses:
    def clause(op: Op[?], k: Any => Any): Any = op match
      case s: Shift0Op => shift0Clause(x, s.body, k, false)
      case _ => unexpected(x, op)

  /** the catcher around an `Aborting` handler: its own abort, its answer; another unwinding on outward; and
   * `shift0`'s delimiter */
  private def abortCatcher(x: Ctx): Clauses = new Clauses:
    def clause(op: Op[?], k: Any => Any): Any = op match
      case s: Shift0Op => shift0Clause(x, s.body, k, false)
      case u: Unwind => u.why match
        case a: AbortSig if a.to eq this => x.pure(a.answer)
        case w => x.unwind(w)
      case _ => unexpected(x, op)

  private def unexpected(x: Ctx, op: Op[?]): Any =
    x.unwind(FailSig(IllegalStateException(s"an operation of the bridge where none was expected: $op")))

  // -- control

  private def shift0(x: Ctx, body: Fn[Fn[Any, Prog[Any]], Prog[Any]]): Any =
    val i = x.index(_.delim)
    if i < 0 then x.unwind(FailSig(IllegalStateException("shift0 with no delimiter around it")))
    else x.performAt(Shift0Op(body), i)

  private def dollar(x: Ctx, body: Program[?], k: Fn[Any, Prog[Any]]): Any =
    flatten(x, true, x.handle(Slot.Delim, new Clauses:
      override def ret(v: Any): Any = guard(x, k.apply(v))
      def clause(op: Op[?], kk: Any => Any): Any = op match
        case s: Shift0Op => shift0Clause(x, s.body, kk, true)
        case _ => unexpected(x, op)
    )(z => in(z, body)))

  // -- failures, finalizers, regions

  private def recover(x: Ctx, body: Program[?], h: Fn[Throwable, Prog[Any]]): Any =
    x.handle(Slot.Unwinds, new Clauses:
      def clause(op: Op[?], k: Any => Any): Any = op match
        case u: Unwind => u.why match
          case f: FailSig => guard(x, h.apply(f.e))
          case w => x.unwind(w)
        case _ => unexpected(x, op)
    )(z => in(z, body))

  private def ensuring(x: Ctx, body: Program[?], fin: Program[Void]): Any =
    val unwound = x.handle(Slot.Unwinds, new Clauses:
      def clause(op: Op[?], k: Any => Any): Any = op match
        case u: Unwind => x.flatMap(in(x, fin), _ => x.unwind(u.why))
        case _ => unexpected(x, op)
    )(z => in(z, body))
    x.flatMap(unwound, v => x.flatMap(in(x, fin), _ => x.pure(v)))

  /** a region a cancellation waits for: marked pending inside, it cancels from outside when it ends */
  private def uncancelable(x: Ctx, body: Program[?]): Any =
    x.handle(Slot.Region, new Clauses:
      def clause(op: Op[?], k: Any => Any): Any = x.flatMap(k(()), _ => x.unwind(CancelSig))
    )(z => in(z, body))

  // -- import

  /** what a foreign computation's operation is answered in place by here: the nearest handler of its effect, if it
   * answers in place; none here, the host */
  private def answersHere(x: Ctx): Answers = new Answers:
    private def nearest(t: Type): Slot | Null = x.effs.find(_.effect eq t).orNull
    def answers(t: Type): Boolean = nearest(t) match
      case null => x.host.answers(t)
      case s => s.answering != null
    def value[X](op: Op[X]): X = nearest(op.effect()) match
      case null => x.host.value(op)
      case s if s.answering != null => trust(s.answering.value(op))
      case _ => throw IllegalStateException(s"not answered in place: $op")

  /** a foreign computation's step, taken here: its operation performed here, its answer — a value, a failure — given
   * to its `k`; a cancellation or an abort crossing it cancels it */
  private def drive(x: Ctx, s: Step[?]): Any = s match
    case d: Step.Done[?] => x.pure(d.value())
    case f: Step.Failed[?] => x.unwind(FailSig(f.error()))
    case _: Step.Cancelled[?] => x.unwind(CancelSig)
    case y: Step.Yield[?, ?] =>
      val k = trust[Resume[Any, Any]](y.k())
      val op = y.op()
      val i = x.find(op.effect())
      x.flatMap(if i >= 0 then x.performAt(op, i) else x.delay(outward(x, op, check = false, x.inRegion)), v => answerForeign(x, k, v))

  private def answerForeign(x: Ctx, k: Resume[Any, Any], v: Any): Any = v match
    case f: FailSig => drive(x, safely(k.fail(f.e)))
    case p: PendingSig => x.flatMap(x.mark, _ => answerForeign(x, k, p.v))
    case s: Signal =>
      safely(k.cancel())
      x.raise(s)
    case _ => drive(x, safely(k.resume(v)))

object OnCore:
  /** okay-core's Cont0, its machine */
  val zero: Effects & okay.kernel.Direct = OnCore("zero", okay.zero.top)
  /** okay-core's Cont, its loop */
  val cont: Effects & okay.kernel.Direct = OnCore("cont", okay.cont.top)

  // -- the trusted steps: values given where the program, built typed, expects them (the reference machine's
  // `trust`); okay-core's programs, untyped here, of the context they were built in (`Ctx`)

  private[kernel] def trust[T](v: Any): T = v.asInstanceOf[T]

  private[kernel] def node(p: Any): Node[?] = p match
    case n: Node[?] => n
    case _ => throw IllegalArgumentException(s"not a program of okay-core's bridge: $p")

  private[kernel] def safely(s: => Step[?]): Step[?] =
    try s catch case NonFatal(e) => new Step.Failed[Any](e)

  // -- the nodes: a kernel program, its value's type a phantom

  private[kernel] sealed abstract class Node[A] extends Prog[A]
  private[kernel] final class PureN[A](val a: Any) extends Node[A]
  private[kernel] final class BindN[A, B](val p: Node[?], val f: Fn[A, Prog[B]]) extends Node[B]
  private[kernel] final class MapN[A, B](val p: Node[?], val f: Fn[A, B]) extends Node[B]
  private[kernel] final class DelayN[A](val t: Thunk[?]) extends Node[A]
  private[kernel] final class PerformN[A](val op: Op[?]) extends Node[A]
  private[kernel] final class HandleN[A](val h: Handler[?, ?], val body: Program[?]) extends Node[A]
  private[kernel] final class Shift0N[A](b: Fn[?, ?]) extends Node[A]:
    def body: Fn[Fn[Any, Prog[Any]], Prog[Any]] = trust(b)
  private[kernel] final class DollarN[A](val body: Program[?], k0: Fn[?, ?]) extends Node[A]:
    def k: Fn[Any, Prog[Any]] = trust(k0)
  private[kernel] final class FailedN[A](val e: Throwable) extends Node[A]
  private[kernel] final class RecoverN[A](val body: Program[?], h0: Fn[Throwable, ?]) extends Node[A]:
    def h: Fn[Throwable, Prog[Any]] = trust(h0)
  private[kernel] final class EnsuringN[A](val body: Program[?], val fin: Program[Void]) extends Node[A]
  private[kernel] final class UncancelableN[A](val body: Program[?]) extends Node[A]
  private[kernel] final class ForeignN[A](val c: Computation[?]) extends Node[A]
  /** a program of okay-core's — a clause's `k`, resumed — made under the bridge's handlers `effs` */
  private[kernel] final class Bound[A](val raw: Any, val effs: List[Slot]) extends Node[A]

  // -- signals: what an operation can be answered, read where it was performed

  private[kernel] sealed abstract class Signal
  private[kernel] final class FailSig(val e: Throwable) extends Signal
  private[kernel] object CancelSig extends Signal
  private[kernel] final class AbortSig(val to: AnyRef, val answer: Any) extends Signal
  /** a value given inside a region a cancellation waits for: the region marked, then the value */
  private[kernel] final class PendingSig(val v: Any) extends Signal

  // -- the bridge's own operations, all of the effect `Dyn`, each routed by its own path

  private val internal: Type = Type.of(classOf[OnCore])
  private[kernel] final class Unwind(val why: Signal) extends Op[Any]:
    def effect(): Type = internal
  private[kernel] final class Shift0Op(val body: Fn[Fn[Any, Prog[Any]], Prog[Any]]) extends Op[Any]:
    def effect(): Type = internal
  private[kernel] object Mark extends Op[Any]:
    def effect(): Type = internal
  /** an operation with no handler of it in force, to the top: and whether a region a cancellation waits for is
   * around it */
  private[kernel] final class Outward(val op: Op[?], val uncancelable: Boolean) extends Op[Any]:
    def effect(): Type = internal

  // -- okay-core's handlers of `Dyn`, untyped: their programs those of the context outside them

  private[kernel] type Raw[X] = Any
  private[kernel] abstract class Clauses extends okay.core.Handler[Raw, Dyn, Any, Any]:
    def ret(a: Any): Any = a
    // okay-core's `k`, its value's type erased: the trusted step of every clause
    final def apply[X](op: Dyn[X], k: X => Any): Any = clause(op, trust[Any => Any](k))
    def clause(op: Op[?], k: Any => Any): Any
  private[kernel] def inPlace(rets: Any => Any, values: Op[?] => Any): okay.core.Answering[Raw, Dyn, Any, Any] =
    new okay.core.Answering[Raw, Dyn, Any, Any]:
      def ret(a: Any): Any = rets(a)
      // the value, or a signal: the operation reads it (`Ctx.checked`)
      def value[X](op: Dyn[X]): X = trust(values(op))

  /** a handler of the bridge in force: the effect it handles (a kernel handler's), whether it is a delimiter of
   * `shift0`, takes an unwinding, is a region; and the kernel handler if it answers in place */
  private[kernel] final class Slot(val effect: Type | Null, val delim: Boolean, val unwind: Boolean, val region: Boolean,
                                   val answering: Answering[Any, Any] | Null)
  private[kernel] object Slot:
    val Delim = Slot(null, true, false, false, null)
    val Catcher = Slot(null, true, true, false, null)
    val Unwinds = Slot(null, false, true, false, null)
    val Region = Slot(null, false, false, true, null)
    val Host = Slot(null, false, false, false, null)
    val Forward = Slot(null, false, false, false, null)

  /**
   * WHERE A PROGRAM IS PLACED: okay-core's context there, the bridge's handlers in force (innermost first, the top
   * handler last), how many, the host. Its operations are okay-core's, on programs untyped: each a program of `c` —
   * the trusted step of the translation, okay-core's types phantom at run time and the bridge placing each program
   * at the context it was made for
   */
  private[kernel] final class Ctx(val c: okay.core.Effects, val effs: List[Slot], val depth: Int, val host: Answers):
    private type R = c.M[Any, Any, Any]
    private def r(p: Any): R = p.asInstanceOf[R]

    def pure(a: Any): Any = c.pure[Any, Any](a)
    def flatMap(p: Any, f: Any => Any): Any = c.flatMap(r(p))[Any, Any](a => r(f(a)))
    def delay(p: => Any): Any = c.delay[Any, Any, Any](r(p))
    def performAt(op: Op[?], i: Int): Any = c.perform[Dyn, Any, Any](trust(op), trust[In[Dyn, c.Effs]](path(i)))

    def handle(slot: Slot, h: okay.core.Handler[Raw, Dyn, Any, Any])(body: Ctx => Any): Any =
      c.handle[Dyn, Any, Any](trust[okay.core.Handler[c.Prog, Dyn, Any, Any]](h))((inner: c.Handled[Dyn, Any]) ?=>
        trust[c.Handling[Dyn, Any, Any, Any]](body(Ctx(inner, slot :: effs, depth + 1, host))))

    /** the index of the nearest handler of the effect `t`, or -1 */
    def find(t: Type): Int =
      var i = 0
      var s = effs
      while s.nonEmpty && (s.head.effect ne t) do
        s = s.tail
        i += 1
      if s.isEmpty then -1 else i

    def index(p: Slot => Boolean): Int = effs.indexWhere(p)
    def inRegion: Boolean = effs.exists(_.region)

    /** an operation's value read: a signal unwinds from here */
    private val check: Any => Any = v => v match
      case s: Signal => raise(s)
      case _ => pure(v)
    def checked(raw: Any): Any = flatMap(raw, check)

    def raise(s: Signal): Any = s match
      case p: PendingSig => flatMap(mark, _ => check(p.v))
      case _ => unwind(s)

    /** to the nearest region: a cancellation pending there */
    def mark: Any =
      val i = index(_.region)
      if i < 0 then pure(()) else performAt(Mark, i)

    /** to the nearest handler taking an unwinding, or the top */
    def unwind(s: Signal): Any =
      val i = index(_.unwind)
      performAt(Unwind(s), if i >= 0 then i else depth)

  /** the path past `i` handlers — the same in every context, made once: `There` `i` times, then `Here` */
  @volatile private var paths: Array[In[?, ?]] = Array(In.Here())
  private def path(i: Int): In[?, ?] =
    val known = paths
    if i < known.length then known(i)
    else
      val grown = new Array[In[?, ?]](math.max(i + 1, known.length * 2))
      System.arraycopy(known, 0, grown, 0, known.length)
      var j = known.length
      while j < grown.length do
        grown(j) = In.There(trust[In[Dyn, Row]](grown(j - 1)))
        j += 1
      paths = grown
      grown(i)
