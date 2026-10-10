package okay.core.kernel

import okay.kernel.{Aborting, Answering, Answers, Computation, Effects, Fn, General, Handler, Op, Prog, Program, Resume,
  Returning, Step, Thunk, Type}
import scala.util.control.NonFatal

/**
 * OKAY-KERNEL'S `Effects`, NATIVE ON OKAY-CORE'S MACHINES (docs/kernel.md, "native"): `OnCore.zero` on Cont0's,
 * `OnCore.cont` on Cont's. A kernel program is the machine's own nodes — `Cont0` and `Cont` are `Prog`s — built
 * directly, nothing wrapped, nothing translated:
 *
 *  - `pure`, `flatMap`, `delay`: the machine's `Return`, `Bind`, `Delay` — a function of the program's guarded, its
 *    exception a failure where it was called;
 *  - `perform`: the machine's kernel operation (`KOp`), its handler found BY ITS EFFECT'S IDENTITY at run time —
 *    answered in place by the handlers in force (the machine's evidence, `Ev`), else routed to the delimiters, each
 *    taking it if it is its own (`KHandler.accepts`), passing it on otherwise;
 *  - `handle`: the machine's own handler node, its clauses a `KHandler` of the kernel handler's shape;
 *  - `shift0` / `$`: `$` a handler node whose return is `k`; `shift0` an operation to the nearest delimiter — a `$`
 *    or a kernel handler — its body in the delimiter's place, `k` the rest and the delimiter;
 *  - failures, aborts, cancellation: a kernel operation `Unwind` to the nearest handler taking it (`recover`,
 *    `ensuring`, an `uncancelable` region, an `Aborting` handler its own abort, the top); what an operation is
 *    answered can be a `Signal`, read where the operation was performed;
 *  - export: the program under a top handler (`Host`) on the machine's top; an operation no handler of which is in
 *    force — answered in place by the host if it answers, else handed out, the machine's `k` the protocol's
 *    `Resume`. Import: another's computation, offered in place what the handlers in force answer so (`Here`).
 *
 * okay-core's typed programs (paths, `A ! E1 + E2`) are untouched: a kernel node is matched after every typed one,
 * and costs nothing where there is none.
 */
final class OnCore private[okay] (name: String, m: Machine) extends Effects:
  import OnCore.*

  override def toString: String = s"OnCore.$name"

  /** THE TRUSTED STEP of building: the machine's nodes, `Prog`s, of the value the program was built at — a cast of
   * the type argument only, nothing checked at run time */
  private inline def prog[A](p: Prog[Any]): Prog[A] = p.asInstanceOf[Prog[A]]

  /** a program of this machine, or a failure saying it is not */
  private inline def own(p: Any): AnyRef =
    if m.owns(p) then p.asInstanceOf[AnyRef] else throw IllegalArgumentException(s"not a program of okay-core: $p")

  /** the user's code giving a program: its exception a failure where it is called */
  private inline def guard(inline p: Any): AnyRef = try own(p) catch case NonFatal(e) => m.raise(FailSig(e))

  /** a `Program`, built when it runs, as the reference machine builds it */
  private def in(p: Program[?]): AnyRef = guard(p.in(this))

  /** `p` then `f`: an operation bound one node (the machine's `fuse`, as a typed operation is), else a bind */
  private def bound(p: AnyRef, f: Any => AnyRef): Prog[Any] = m.fuse(p, f) match
    case null => m.bind(p, f)
    case n: Prog[Any] @unchecked => n

  def pure[A](a: A): Prog[A] = prog(m.pure(a))
  // a bind's value read first: a handler's failing answer (a `Returning`'s) comes as a signal, raised at the next
  // bind — no bind of its own a handler
  def flatMap[A, B](p: Prog[A], f: Fn[A, Prog[B]]): Prog[B] =
    val g = untyped(f)
    prog(bound(own(p), a => a match
      case s: Signal => m.raise(s)
      case _ => guard(g.apply(a))))
  // the machine's map: the function's failure a signal, raised at the next bind or handler's return as a failing
  // answer is; a signal it is given passed on as it is
  override def map[A, B](p: Prog[A], f: Fn[A, B]): Prog[B] =
    val g: Any => Any = a => a match
      case s: Signal => s
      case _ => try f.apply(trust(a)) catch case NonFatal(e) => FailSig(e)
    val q = own(p)
    // over an operation: one node, as a bind over it is
    prog(m.fuse(q, a => m.pure(g(a))) match
      case null => m.map(q, g)
      case n: Prog[Any] @unchecked => n)
  def delay[A](p: Thunk[Prog[A]]): Prog[A] =
    val t = p.asInstanceOf[Thunk[Any]]
    prog(m.delay(() => guard(t.get())))
  def perform[X](op: Op[X]): Prog[X] = prog(m.perform(op))
  def handle[A, R](h: Handler[A, R], body: Program[A]): Prog[R] = prog(handled(h, body))
  def shift0[A, I, O](body: Fn[Fn[A, Prog[I]], Prog[O]]): Prog[A] = prog(m.perform(Shift0Op(trust(body))))
  def dollar[A, I, O](body: Program[A], k: Fn[A, Prog[I]]): Prog[O] =
    prog(m.bind(m.handle(new Dollar(trust(k)), () => in(body)), flatten))
  def failed[A](e: Throwable): Prog[A] = prog(m.raise(FailSig(e)))
  // its body's value read: a handler's failing answer, the body's last, is a failure of the body
  def recover[A](body: Program[A], h: Fn[Throwable, Prog[A]]): Prog[A] =
    val hh = trust[Fn[Throwable, Prog[Any]]](h)
    prog(m.bind(m.handle(new Recover(hh), () => in(body)), v => v match
      case f: FailSig => guard(hh.apply(f.e))
      case _ => m.pure(v)))
  def ensuring[A](body: Program[A], finalizer: Program[Void]): Prog[A] =
    prog(m.bind(m.handle(new Ensuring(finalizer), () => in(body)), v => m.bind(in(finalizer), _ => m.check(v))))
  def uncancelable[A](body: Program[A]): Prog[A] =
    prog(m.bind(m.handle(new Region(), () => in(body)), v => v match
      case e: Ended => m.raise(e.why)
      case _ => m.check(v)))
  def foreign[A](c: Computation[A]): Prog[A] =
    prog(m.performThen(HereOp, here => drive(safely(c.start(trust[Answers](here))), null)))

  def computation[A](program: Program[A], row: java.util.Set[Type]): Computation[A] =
    val fixed = java.util.Collections.unmodifiableSet(new java.util.HashSet[Type](row))
    new Computation[A]:
      def start(host: Answers): Step[A] = trust(runTop(m.handle(new Host(host), () => in(program))))
      def effects(): java.util.Set[Type] = fixed

  /** a run on the machine's top: the top handler's answer, a step */
  private def runTop(p: AnyRef): Step[Any] =
    try trust[Step[Any]](m.run(p)) catch case NonFatal(e) => new Step.Failed[Any](e)

  // -- handlers: the kernel's shapes as the machine's own

  private def handled(h: Handler[?, ?], body: Program[?]): Prog[Any] =
    val t = h.effect()
    val g: General[Any, Any] | Null = h match
      case g: General[?, ?] => trust[General[Any, Any]](g)
      case _ => null
    // a return that is a program: the machine's handler answers it, run when the handler is left
    val flat = g != null && !h.isInstanceOf[Returning[?, ?]]
    // a signal its body came to — a failing answer of a handler inside, its last — passed on as it is
    val rets: Any => Any = h match
      // its failure a signal, raised at the next bind or handler's return
      case r: Returning[?, ?] => v => v match
        case s: Signal => s
        case _ => try trust[Returning[Any, Any]](r).answer(v) catch case NonFatal(e) => FailSig(e)
      case _ if g != null => v => v match
        case s: Signal => m.raise(s)
        case _ => guard(g.ret(v, this))
      case _ => v => v
    val clauses: KHandler = h match
      case a: Answering[?, ?] => new KAnswering(t, trust(a), rets, flat)
      case a: Aborting[?, ?] => new KAborting(t, trust(a), rets, flat)
      case _ => new KGeneral(t, g, rets, flat)
    val node = m.handle(clauses, () => in(body))
    if flat then m.bind(node, flatten) else node

  /** the rest `k` of a delimiter's body, as the kernel's: flattened as the delimiter's answer is */
  private def resumed(k: Any => AnyRef, flat: Boolean): Fn[Any, Prog[Any]] =
    v => prog(if flat then m.bind(k(v), flatten) else m.node(k(v)))

  /** a clause's program as the machine handler's answer: itself, or — the answer a program — that program */
  private def answered(flat: Boolean, p: AnyRef): AnyRef = if flat then m.pure(p) else p

  /** a `shift0` at its delimiter: the body in the delimiter's place, `k` the rest and the delimiter */
  private def shift0At(s: Shift0Op, k: Any => AnyRef, flat: Boolean): AnyRef =
    answered(flat, guard(untyped(s.body).apply(resumed(k, flat))))

  /** a kernel handler of `t`: its operations, and `shift0`'s — a handler is a delimiter */
  private abstract class Of(t: Type, rets: Any => Any, inPlace: Boolean) extends KHandler(t, inPlace, false):
    override def ret(a: Any): Any = rets(a)
    def accepts(op: Op[?]): Boolean = (op.effect() eq t) || op.isInstanceOf[Shift0Op]
    override def handles(e: Type): Boolean = e eq t

  /** `Answering`: answered in place by the machine; its failure a signal */
  private final class KAnswering(t: Type, a: Answering[Any, Any], rets: Any => Any, flat: Boolean) extends Of(t, rets, true):
    override def answersInPlace(op: Op[?]): Boolean = !op.isInstanceOf[Shift0Op]
    override def inPlaceFor(e: Type): Boolean = true
    override def value[X](op: Dyn[X]): X = trust[X](try a.value(op) catch case NonFatal(e) => FailSig(e))
    def clause(op: Op[?], k: Any => AnyRef, region: Boolean): AnyRef = op match
      case s: Shift0Op => shift0At(s, k, flat)
      case _ => k(value(op))

  /** `Aborting`: answered in place by a signal that unwinds to it — itself the abort's mark */
  private final class KAborting(t: Type, a: Aborting[Any, Any], rets: Any => Any, flat: Boolean) extends Of(t, rets, true):
    override def accepts(op: Op[?]): Boolean = super.accepts(op) || (op match
      case u: Unwind => u.why match
        case s: AbortSig => s.to eq this
        case _ => false
      case _ => false)
    override def answersInPlace(op: Op[?]): Boolean = op.effect() eq t
    override def value[X](op: Dyn[X]): X = trust[X](try AbortSig(this, a.abort(op)) catch case NonFatal(e) => FailSig(e))
    def clause(op: Op[?], k: Any => AnyRef, region: Boolean): AnyRef = op match
      case s: Shift0Op => shift0At(s, k, flat)
      case u: Unwind => u.why match
        case s: AbortSig => m.pure(s.answer)
        case _ => m.raise(u.why)
      case _ => k(value(op))

  /** `General` — or no clause at all (a `Returning` alone: its operation fails where it was performed) */
  private final class KGeneral(t: Type, g: General[Any, Any] | Null, rets: Any => Any, flat: Boolean) extends Of(t, rets, false):
    def clause(op: Op[?], k: Any => AnyRef, region: Boolean): AnyRef = op match
      case s: Shift0Op => shift0At(s, k, flat)
      case _ if g != null => answered(flat, guard(g.apply[Any](trust(op), resumed(k, flat), OnCore.this)))
      case _ => k(FailSig(IllegalStateException(s"a handler of no shape for an operation: $op")))

  // -- control, failures, finalizers, regions

  private final class Dollar(k: Fn[Any, Prog[Any]]) extends KHandler(null, false, false):
    private val kk = untyped(k)
    override def ret(v: Any): Any = v match
      case s: Signal => m.raise(s)
      case _ => guard(kk.apply(v))
    def accepts(op: Op[?]): Boolean = op.isInstanceOf[Shift0Op]
    def clause(op: Op[?], kk: Any => AnyRef, region: Boolean): AnyRef = shift0At(trust(op), kk, true)

  private final class Recover(h: Fn[Throwable, Prog[Any]]) extends KHandler(null, false, false):
    def accepts(op: Op[?]): Boolean = op match
      case u: Unwind => u.why.isInstanceOf[FailSig]
      case _ => false
    def clause(op: Op[?], k: Any => AnyRef, region: Boolean): AnyRef = guard(h.apply(trust[Unwind](op).why.asInstanceOf[FailSig].e))

  private final class Ensuring(fin: Program[Void]) extends KHandler(null, false, false):
    def accepts(op: Op[?]): Boolean = op.isInstanceOf[Unwind]
    def clause(op: Op[?], k: Any => AnyRef, region: Boolean): AnyRef = m.bind(in(fin), _ => m.raise(trust[Unwind](op).why))

  /** a region a cancellation waits for: marked pending inside, it cancels from outside when it ends — with a value
   * or unwound, an unwinding ending it as a value (`Ended`): so a mark's clause, the rest of the region resumed, sees
   * the region end either way; the unwinding goes on outside it otherwise */
  private final class Region() extends KHandler(null, false, false):
    override def region: Boolean = true
    def accepts(op: Op[?]): Boolean = (op eq Mark) || op.isInstanceOf[Unwind]
    def clause(op: Op[?], k: Any => AnyRef, region: Boolean): AnyRef = op match
      case u: Unwind => m.pure(Ended(u.why))
      case _ => m.bind(k(()), _ => m.raise(CancelSig))

  // -- export: the top

  /** the top handler of a computation: an operation reaching it handed out; an unwinding ended there */
  private final class Host(host: Answers) extends KHandler(null, false, true):
    override def ret(v: Any): Any = v match
      case f: FailSig => new Step.Failed[Any](f.e)
      case _ => new Step.Done[Any](v)
    def accepts(op: Op[?]): Boolean = true
    override def handles(e: Type): Boolean = true
    override def answersInPlace(op: Op[?]): Boolean = !op.isInstanceOf[Internal] && host.answers(op.effect())
    override def inPlaceFor(e: Type): Boolean = host.answers(e)
    override def hostAnswers(e: Type): Boolean = host.answers(e)
    override def value[X](op: Dyn[X]): X = trust[X](try host.value(op) catch case NonFatal(e) => FailSig(e))
    def clause(op: Op[?], k: Any => AnyRef, region: Boolean): AnyRef = op match
      case u: Unwind => m.pure(u.why match
        case f: FailSig => new Step.Failed[Any](f.e)
        case CancelSig => new Step.Cancelled[Any]()
        case _ => new Step.Failed[Any](IllegalStateException("an abort with its handler gone")))
      case _: Shift0Op => k(FailSig(IllegalStateException("shift0 with no delimiter around it")))
      case Mark => k(())
      case HereOp => k(Answers.NONE)
      case _ => m.pure(new Step.Yield[Any, Any](trust(op), resumption(k, op, region, pending = false)))

  /** the machine's `k` of an operation handed out, as the protocol's: each call a run of it on the machine's top.
   * Inside an `uncancelable` region a cancellation waits: the same operation again, the cancellation pending — each
   * value given then marks it to its region, which cancels when it ends */
  private def resumption(k: Any => AnyRef, op: Op[?], uncancelable: Boolean, pending: Boolean): Resume[Any, Any] =
    new Resume[Any, Any]:
      private def on(v: Any): Step[Any] = runTop(k(if pending then PendingSig(v) else v))
      def resume(x: Any): Step[Any] = on(x)
      def fail(e: Throwable): Step[Any] = on(FailSig(e))
      def cancel(): Step[Any] =
        if uncancelable then new Step.Yield[Any, Any](trust(op), resumption(k, op, uncancelable, pending = true))
        else on(CancelSig)

  // -- import

  /** a foreign computation's step, taken here: its operation performed here, its answer — a value, a failure — given
   * to its `k`. `after`: a cancellation or an abort that crossed it — it is being cancelled, its finalizers'
   * operations answered here as any; when it ends (however), `after` goes on */
  private def drive(s: Step[?], after: Signal | Null): AnyRef = s match
    case y: Step.Yield[?, ?] =>
      val k = trust[Resume[Any, Any]](y.k())
      m.performThen(y.op(), v => answerForeign(k, v, after))
    case _ if after != null => m.raise(after)
    case d: Step.Done[?] => m.pure(d.value())
    case f: Step.Failed[?] => m.raise(FailSig(f.error()))
    case _ => m.raise(CancelSig)

  private def answerForeign(k: Resume[Any, Any], v: Any, after: Signal | Null): AnyRef = v match
    case f: FailSig => drive(safely(k.fail(f.e)), after)
    case p: PendingSig => m.performThen(Mark, _ => answerForeign(k, p.v, after))
    case s: Signal => drive(safely(k.cancel()), s)
    case _ => drive(safely(k.resume(v)), after)

object OnCore:
  /** okay-core's Cont0, its machine */
  val zero: Effects = OnCore("zero", okay.zero.ZeroMachine)
  /** okay-core's Cont, its loop */
  val cont: Effects = OnCore("cont", okay.cont.ContMachine)

  /** THE TRUSTED STEP of running: a value given where the program, built typed, expects it — the reference
   * machine's `trust` */
  private[okay] def trust[T](v: Any): T = v.asInstanceOf[T]

  private val flatten: Any => AnyRef = p => p.asInstanceOf[AnyRef]

  /** a function of the user's, its result untyped: the program it gives checked by the machine's class (`own`), not
   * cast to the kernel's interface on the way */
  private inline def untyped(f: Fn[?, ?]): Fn[Any, Any] = f.asInstanceOf[Fn[Any, Any]]

  private def safely(s: => Step[?]): Step[?] = try s catch case NonFatal(e) => new Step.Failed[Any](e)

/** the kernel's operations, every one, as one effect of okay-core's: its handlers the machines' handler nodes, their
 * clauses `KHandler`s */
type Dyn[X] = Op[X]

/** WHAT AN OPERATION CAN BE ANSWERED BESIDES A VALUE, read where it was performed: a failure (`k.fail`, a throwing
 * `Answering`), a cancellation (`k.cancel`), an abort (to its handler, its finalizers run), a value given inside a
 * region a cancellation waits for (the region marked, then the value) */
private[okay] sealed abstract class Signal
private[okay] final class FailSig(val e: Throwable) extends Signal
private[okay] object CancelSig extends Signal
private[okay] final class AbortSig(val to: AnyRef, val answer: Any) extends Signal
private[okay] final class PendingSig(val v: Any) extends Signal
/** a region ended by an unwinding: its answer, read where it is left */
private[okay] final class Ended(val why: Signal)

/** the operations of the native layer itself, of an effect no handler of the user's has */
private[okay] sealed abstract class Internal extends Op[Any]:
  def effect(): Type = Internal.tpe
private[okay] object Internal:
  val tpe: Type = Type.of("okay.core.kernel.Internal")
/** an unwinding, to the nearest handler taking it */
private[okay] final class Unwind(val why: Signal) extends Internal
/** `shift0`, to the nearest delimiter */
private[okay] final class Shift0Op(val body: Fn[Fn[Any, Prog[Any]], Prog[Any]]) extends Internal
/** a cancellation pending, to the nearest region */
private[okay] object Mark extends Internal
/** what the handlers in force answer in place, as the protocol's `Answers`: what a foreign computation is offered */
private[okay] object HereOp extends Internal

/**
 * A HANDLER OF THE KERNEL'S, AS THE MACHINES' OWN: the clauses of a machine's handler node (Cont0's `Handle`, Cont's
 * `Fold`). An okay-core `Answering`, so the machines keep it among the handlers in force (`Ev`) whatever it is — its
 * `value` asked by the kernel's operations only, when it answers in place (`answersInPlace`); okay-core's typed
 * operations never reach it (their paths go to their own handlers). The machine routes a kernel operation by
 * `accepts` — the nearest handler that takes it — and runs `clause` with its `k`, the rest under the handler again
 */
private[okay] abstract class KHandler(
    /** the effect it handles, a kernel handler's; its operations it answers in place, if `answersOwn` */
    val key: Type | Null, val answersOwn: Boolean,
    /** the top: every operation reaching it its own */
    val isHost: Boolean) extends okay.core.Answering[[X] =>> Any, Dyn, Any, Any]:
  def ret(a: Any): Any = a
  def value[X](op: Dyn[X]): X = throw IllegalStateException(s"not answered in place: $op")
  /** whether the operation is this handler's */
  def accepts(op: Op[?]): Boolean
  /** whether it answers the operation, its own, in place: `value` */
  def answersInPlace(op: Op[?]): Boolean = false
  /** its clause: the program outside it, `k` the rest under it again; `region`, a region crossed on the way */
  def clause(op: Op[?], k: Any => AnyRef, region: Boolean): AnyRef
  /** a region a cancellation waits for: what crosses it is marked so */
  def region: Boolean = false
  /** for a foreign computation's offer: whether operations of `e` are its, and answered in place */
  def handles(e: Type): Boolean = false
  def inPlaceFor(e: Type): Boolean = false
  def hostAnswers(e: Type): Boolean = false

  /** for the machines' walk of the handlers in force, an operation `op` of the effect `t`: 0 not this handler's, 1
   * answered in place by it, 2 its own, not in place. An operation of the user's by fields (its effect compared to
   * the handler's, no call); one of the native layer's (`internal`) by `accepts` */
  final def decide(op: Op[?], t: Type, internal: Boolean): Int =
    if internal then (if accepts(op) then (if answersInPlace(op) then 1 else 2) else 0)
    else if key eq t then (if answersOwn then 1 else 2)
    else if isHost then (if hostAnswers(t) then 1 else 2)
    else 0

/**
 * A MACHINE, AS THE KERNEL'S `Effects` NEEDS IT: its nodes made — untyped here, the kernel's programs carry no row or
 * answer types; okay-core's are phantom at run time — and run. Each machine's loop takes `perform`'s and
 * `performThen`'s nodes itself (`KOp`, `KOpK`; `KInject`, `KSuspend`): the nearest handler in force that `accepts`
 * the operation answers it in place if it can, else the operation is routed to the delimiters
 */
private[okay] abstract class Machine:
  // its nodes typed as `Prog`s, which they are: no cast to the kernel's interface where `OnCore` hands them out
  def pure(a: Any): Prog[Any]
  def bind(p: AnyRef, f: Any => AnyRef): Prog[Any]
  def map(p: AnyRef, f: Any => Any): Prog[Any]
  def delay(p: () => AnyRef): Prog[Any]
  /** a kernel operation, its value read for a signal where it was performed */
  def perform(op: Op[?]): Prog[Any]
  /** a kernel operation, its value as it is — a signal too — to `k` */
  def performThen(op: Op[?], k: Any => AnyRef): Prog[Any]
  def handle(h: KHandler, body: () => AnyRef): Prog[Any]
  def run(p: AnyRef): Any
  def owns(p: Any): Boolean
  /** `p` bound to `k` as one node, if `p` is a kernel operation (`perform`'s): its value read for a signal, then `k`
   * — no bind, no frame; else `null` */
  def fuse(p: AnyRef, k: Any => AnyRef): Prog[Any] | Null
  /** one of its nodes, as a `Prog`: a check of its class, not of the kernel's interface */
  def node(p: AnyRef): Prog[Any]

  /** an operation's value read, then the rest `k`: a signal raised there instead */
  final def checkThen(v: Any, k: Any => AnyRef): AnyRef = v match
    case p: PendingSig => performThen(Mark, _ => checkThen(p.v, k))
    case s: Signal => performThen(Unwind(s), check)
    case _ => k(v)

  /** `k` with its value read first, as a function: for a fused operation leaving the loop */
  final def checked(k: Any => AnyRef): Any => AnyRef = v => checkThen(v, k)

  /** the walk's start: whether the operation is one of the native layer's */
  final def internal(t: Type): Boolean = t eq Internal.tpe

  /** a signal raised where the program is */
  final def raise(s: Signal): Prog[Any] = s match
    case p: PendingSig => performThen(Mark, _ => check(p.v))
    case _ => performThen(Unwind(s), check)

  /** an operation's value read: a signal raised there, a value the program's */
  final val check: Any => AnyRef = v => v match
    case s: Signal => raise(s)
    case _ => pure(v)

private[okay] object Machine:
  /** `HereOp` answered in place: the offer to a foreign computation, made of the handlers in force — those the
   * machine knows, innermost first */
  def here(hs: List[KHandler]): KHandler = Here(hs)

  /** the offer to a foreign computation: the nearest handler of each effect, if it answers in place */
  private final class Here(hs: List[KHandler]) extends KHandler(null, false, false):
    def accepts(op: Op[?]): Boolean = true
    def clause(op: Op[?], k: Any => AnyRef, region: Boolean): AnyRef = k(this)
    override def value[X](op: Dyn[X]): X = OnCore.trust[X](answers)
    private val answers: Answers = new Answers:
      def answers(e: Type): Boolean = hs.find(_.handles(e)).exists(_.inPlaceFor(e))
      def value[X](op: Op[X]): X = hs.find(_.handles(op.effect())) match
        case Some(h) => h.value(op) match
          case f: FailSig => throw f.e
          case v => OnCore.trust[X](v)
        case None => throw IllegalStateException(s"not answered in place: $op")
