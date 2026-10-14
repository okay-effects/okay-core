package okay.core.kernel

import okay.kernel.{Aborting, Answering, Answers, Computation, Control, Effects, Fn, General, Handler, Op, Prog, Program, Resume,
  Returning, Step, Thunk, Translating, Type}
import okay.one.{!, Cont}
import okay.one.Cont.{defer, pure}
import okay.one.handler.Deep
import okay.one.row.{+:, Pure}
import okay.one.step.Step as OStep
import okay.one.step.Step.step
import scala.util.control.NonFatal

/**
 * OKAY-CORE AS AN IMPLEMENTATION OF THE KERNEL: okay-kernel's `Effects` and `Control` on the one monad's machine (`okay.one`) —
 * written first as okay-kernel's `one/` module (OnOne), the same here. A kernel
 * program (`Prog`) IS a program of okay-one (`P`, a `Cont` in a box) over one row, `Req +: Pure`: `pure` a `Return`,
 * `flatMap` a `Bind`, `delay` a `Defer`, a delimiter of the bridge a `Deep` handler of `Req`, an operation a `Perform`
 * of it — nothing of the bridge's own runs a program, the machine's loop does, in constant stack, `k` data.
 *
 * HOW. The kernel finds a handler at RUN time, by the operation's key, where the operation runs — a `k` resumed under
 * other handlers sees those. So every operation of the kernel is a REQUEST to the nearest delimiter of the bridge,
 * `Req`, and every delimiter is a `Deep` handler of `Req` over the same row outside: it takes the requests it
 * concerns and FORWARDS the rest outward — `perform(request).flatMap(k)`, its `k` the rest under it with itself put
 * back (okay-one's `k` is data: the levels passed, put back when resumed).
 *
 *  - Handlers, by shape: `Answering` resumes `k` with `value(op)`; `Aborting` resumes it with an ABORT (a signal,
 *    below) unwinding from where the operation was to the handler, its finalizers run on the way; `Translating` runs
 *    its program in the handler's place, the rest resumed with its value; `General` runs the clause with `k` deep;
 *    a return `Returning`'s `answer`, else `General`'s `ret`, else the value.
 *  - `shift0`: a request only a handler's delimiter or a `$`'s takes — the bridge's others (`recover`, `ensuring`,
 *    `uncancelable`) forward it. `$`: a delimiter whose return is `k`.
 *  - FAILURES, ABORTS, CANCELLATION: requests too — `Unwind`, `k` dropped — to the nearest delimiter taking them:
 *    `recover` (a failure), `ensuring` (any: the finalizer, then on), an `Aborting` handler (its own abort), a region
 *    (a cancellation pending there turns any into a cancellation), the top (the step). An exception of the program's
 *    functions is caught where the bridge calls it and unwinds from there — never a `Raise` of the machine's, which
 *    the bridge's delimiters would not see. What an operation is answered can be a SIGNAL: the operation reads its
 *    value and unwinds from where it was performed — `k.fail`, `k.cancel`, an abort through finalizers.
 *  - EXPORT (`computation`): the program stepped (`Step`): a request come to the top is the host's in place if it
 *    answers its effect, else `Step.Yield` with the rest as the `Resume` — multi-shot, from any thread. IMPORT
 *    (`foreign`): the handlers in force asked for (`Env`), the computation started with them as its host in place,
 *    its yields performed here as operations of this program.
 *
 * TRUSTED STEPS, two: `trust` (a value given where the program, built typed, expects it: the protocol is untyped by
 * design) and `inside` (the row is phantom to the machine: a body of the bridge under one more delimiter of the
 * bridge is the same program, its operations of the nearest delimiter — `Here` — as before).
 */
object OnCore extends Effects with Control:

  override def toString: String = "OnCore"

  /** THE BRIDGE'S ONE EFFECT: a request to the nearest delimiter of the bridge. Its value `Any`: what the operation
   * is answered, a value or a signal, read where it was performed */
  sealed abstract class Req[X]
  /** a kernel operation; `region`: performed inside a region a cancellation may wait for */
  final class Perform(val op: Op[?], val region: Boolean) extends Req[Any]
  final class Shift(val body: Fn[Fn[Any, Prog[Any]], Prog[Any]]) extends Req[Any]
  /** to the nearest region: a cancellation pending there */
  final class Mark extends Req[Any]
  /** the handlers in force, innermost first: each adds itself on the way out; the top adds the host */
  final class Env(val acc: List[Handler[?, ?]]) extends Req[Any]
  /** an unwinding: the rest dropped, never answered */
  final class Unwind(val s: Signal) extends Req[Any]

  /** the bridge's row, and its programs, untyped: every program of the bridge over it */
  type KR = Req +: Pure
  type U = Any ! KR

  /** A KERNEL PROGRAM ON OKAY-ONE'S MACHINE: okay-one's program itself, in a box */
  final class P[A] private[kernel] (val c: U) extends Prog[A]

  // -- the trusted steps

  /** THE TRUSTED STEP of the values: a value given where the program, built typed, expects it */
  private[kernel] def trust[T](v: Any): T = v.asInstanceOf[T]

  /** THE TRUSTED STEP of the row: a body of the bridge, over `KR`, under one more delimiter of the bridge — over
   * `Req +: KR`. The row is phantom to the machine; every operation of the bridge is of the nearest delimiter, `Here`,
   * in both */
  private[kernel] def inside(u: U): Any ! (Req +: KR) = u.asInstanceOf[Any ! (Req +: KR)]

  private[kernel] def unbox(p: Prog[?]): U = p match
    case p: P[?] => p.c
    case _ => throw IllegalArgumentException(s"not a program of okay-core's bridge: $p")

  // -- the nodes

  private[kernel] def request(r: Req[Any]): U = Cont.perform[Req, KR, Any](r)
  /** `body` under a delimiter of the bridge: the body's value to its `ret`, its requests to its clause */
  private[kernel] def delimit(body: U, d: Delim): U = inside(body).handle(d)

  /** a program the user's code gives: its exception a failure where it is run */
  private[kernel] def guard(p: => Prog[?]): U =
    try unbox(p) catch case NonFatal(e) => unwind(FailSig(e))
  private def built(p: Program[?]): U = guard(p.in(this))

  // -- Effects

  def pure[A](a: A): Prog[A] = P(Cont.pure(a))
  def flatMap[A, B](p: Prog[A], f: Fn[A, Prog[B]]): Prog[B] = P(unbox(p).flatMap(v => guard(f.apply(trust[A](v)))))
  override def map[A, B](p: Prog[A], f: Fn[A, B]): Prog[B] =
    P(unbox(p).flatMap(v => try Cont.pure(f.apply(trust[A](v))) catch case NonFatal(e) => unwind(FailSig(e))))
  def delay[A](p: Thunk[Prog[A]]): Prog[A] = P(defer(guard(p.get())))
  def perform[X](op: Op[X]): Prog[X] = P(performU(op))
  def handle[A, R](h: Handler[A, R], body: Program[A]): Prog[R] = P(defer(delimit(built(body), HandlerD(h))))
  def shift0[A, I, O](body: Fn[Fn[A, Prog[I]], Prog[O]]): Prog[A] =
    P(request(Shift(trust[Fn[Fn[Any, Prog[Any]], Prog[Any]]](body))).flatMap(check))
  def dollar[A, I, O](body: Program[A], k: Fn[A, Prog[I]]): Prog[O] =
    P(defer(delimit(built(body), DollarD(trust[Fn[Any, Prog[Any]]](k)))))
  def failed[A](e: Throwable): Prog[A] = P(defer(unwind(FailSig(e))))
  def recover[A](body: Program[A], h: Fn[Throwable, Prog[A]]): Prog[A] = P(defer(delimit(built(body), RecoverD(h))))
  def ensuring[A](body: Program[A], finalizer: Program[Void]): Prog[A] =
    P(defer(delimit(built(body), EnsuringD(finalizer))))
  def uncancelable[A](body: Program[A]): Prog[A] = P(defer(delimit(built(body), RegionD())))
  def foreign[A](c: Computation[A]): Prog[A] =
    P(request(Env(Nil)).flatMap(env => drive(safely(c.start(answersFrom(trust[EnvV](env)))), null)))

  /** an operation: a request; its answer read — a signal unwinds from here */
  private[kernel] def performU(op: Op[?]): U = request(Perform(op, region = false)).flatMap(check)

  // -- signals: what an operation can be answered, read where it was performed

  private[kernel] sealed abstract class Signal
  private[kernel] final class FailSig(val e: Throwable) extends Signal
  private[kernel] object CancelSig extends Signal
  /** an abort to the handler `to` (its delimiter, one a run of the handler), its answer */
  private[kernel] final class AbortSig(val to: AnyRef, val answer: Any) extends Signal
  /** a value given inside a region a cancellation waits for: the region marked, then the value */
  private[kernel] final class PendingSig(val v: Any) extends Signal

  /** an operation's answer `v` read where it was performed: a signal unwinds from here, a value goes to `next` */
  private[kernel] def answered(v: Any)(next: Any => U): U = v match
    case p: PendingSig => request(Mark()).flatMap(_ => answered(p.v)(next))
    case s: Signal => unwind(s)
    case _ => next(v)
  private val check: Any => U = v => answered(v)(Cont.pure)

  /** to the nearest delimiter taking it: `recover`, `ensuring`, a handler (its abort), a region, the top. Its answer
   * never comes: the rest is dropped by whoever takes it */
  private[kernel] def unwind(s: Signal): U = request(Unwind(s))

  /** the handlers in force where a foreign computation starts, and the host */
  private[kernel] final class EnvV(val handlers: List[Handler[?, ?]], val host: Answers)

  // -- the delimiters: each a Deep handler of Req over the row outside, its body's value through `done`

  private[kernel] abstract class Delim extends Deep[Req, KR, Any, Any]:
    def done(v: Any): U
    final def ret(v: Any): U = done(v)
    final def apply[X](r: Req[X], k: X => U): U = req(trust[Req[Any]](r), trust[Any => U](k))
    def req(r: Req[Any], k: Any => U): U = forward(r, k)
    /** a request this delimiter does not take, on outward: an unwinding as it is, `k` dropped; a request with `k`
     * the rest up to here, this delimiter, the rest up to the next */
    final def forward(r: Req[Any], k: Any => U): U = r match
      case u: Unwind => request(u)
      case p: Perform => request(Perform(p.op, p.region || this.isInstanceOf[RegionD])).flatMap(k)
      case e: Env => request(this match
        case h: HandlerD => Env(h.h :: e.acc)
        case _ => e).flatMap(k)
      case other => request(other).flatMap(k)

  /** a run of a kernel handler */
  private final class HandlerD(val h: Handler[?, ?]) extends Delim:
    private val t: Type = h.effect()
    def done(v: Any): U = h match
      case r: Returning[?, ?] => try Cont.pure(trust[Returning[Any, Any]](r).answer(v)) catch case NonFatal(e) => unwind(FailSig(e))
      case g: General[?, ?] => guard(trust[General[Any, Any]](g).ret(v, OnCore))
      case _ => Cont.pure(v)
    override def req(r: Req[Any], k: Any => U): U = r match
      case p: Perform if p.op.effect() eq t => h match
        case a: Answering[?, ?] => k(try a.value(trust[Op[Any]](p.op)) catch case NonFatal(e) => FailSig(e))
        case a: Aborting[?, ?] =>
          k(try AbortSig(this, trust[Aborting[Any, Any]](a).abort(trust[Op[Any]](p.op))) catch case NonFatal(e) => FailSig(e))
        case tr: Translating[?, ?] =>
          // its program in the handler's place, the rest then resumed with its value: no k given to code
          built(trust[Translating[Any, Any]](tr).translate(trust[Op[Any]](p.op))).flatMap(v => k(v))
        case g: General[?, ?] => guard(trust[General[Any, Any]](g).apply[Any](trust[Op[Any]](p.op), x => P(k(x)), OnCore))
        case _ => k(FailSig(IllegalStateException(s"a handler of no shape: $h")))
      case s: Shift => guard(s.body.apply(x => P(k(x))))
      case u: Unwind => u.s match
        case a: AbortSig if a.to.eq(this) => Cont.pure(a.answer)
        case _ => forward(u, k)
      case _ => forward(r, k)

  /** `body $ k` */
  private final class DollarD(kf: Fn[Any, Prog[Any]]) extends Delim:
    def done(v: Any): U = guard(kf.apply(v))
    override def req(r: Req[Any], k: Any => U): U = r match
      case s: Shift => guard(s.body.apply(x => P(k(x))))
      case _ => forward(r, k)

  private final class RecoverD[A](h: Fn[Throwable, Prog[A]]) extends Delim:
    def done(v: Any): U = Cont.pure(v)
    override def req(r: Req[Any], k: Any => U): U = r match
      case u: Unwind => u.s match
        case f: FailSig => guard(h.apply(f.e))
        case _ => forward(u, k)
      case _ => forward(r, k)

  private final class EnsuringD(fin: Program[Void]) extends Delim:
    def done(v: Any): U = built(fin).flatMap(_ => Cont.pure(v))
    override def req(r: Req[Any], k: Any => U): U = r match
      case u: Unwind => built(fin).flatMap(_ => request(u))
      case _ => forward(r, k)

  /** a region a cancellation waits for; `pending`, one waits — it cancels when the region ends, by a value or not.
   * One a run of `uncancelable`; `k` resumed holds it, so the mark is its state */
  private final class RegionD extends Delim:
    private var pending = false
    def done(v: Any): U = if pending then unwind(CancelSig) else Cont.pure(v)
    override def req(r: Req[Any], k: Any => U): U = r match
      case _: Mark =>
        pending = true
        k(())
      case u: Unwind if pending => unwind(CancelSig)
      case _ => forward(r, k)

  // -- the top: the program stepped, its requests the host's or the protocol's

  /** a run of okay-one's machine to the protocol's step: a request come to the top — the host's in place if it
   * answers its effect, else a yield; an unwinding its end; the bridge's own answered here */
  private def loop(s: OStep[KR, Any], host: Answers): Step[Any] = s match
    case OStep.Done(a) => new Step.Done[Any](a)
    case OStep.Failed(e) => new Step.Failed[Any](e)
    case y: OStep.Yield[KR, Any, e, x] =>
      val resume = trust[OStep.Resume[Any, KR, Any]](y.resume)
      trust[Req[Any]](y.op) match
        case p: Perform =>
          val answers = try host.answers(p.op.effect()) catch case NonFatal(_) => false
          if answers then loop(resume(try host.value(trust[Op[Any]](p.op)) catch case NonFatal(e) => FailSig(e)), host)
          else new Step.Yield[Any, Any](trust[Op[Any]](p.op), resumption(p, resume, host, pending = false))
        case u: Unwind => u.s match
          case f: FailSig => new Step.Failed[Any](f.e)
          case CancelSig => new Step.Cancelled[Any]()
          case _ => new Step.Failed[Any](IllegalStateException("an abort with its handler gone"))
        case _: Shift => loop(resume(FailSig(IllegalStateException("shift0 with no delimiter around it"))), host)
        case _: Mark => loop(resume(()), host)
        case e: Env => loop(resume(EnvV(e.acc.reverse, host)), host)

  /** `k` of an operation handed out, as the protocol's: each call a run of the machine from the rest. Inside a region
   * a cancellation waits: the same operation again, the cancellation pending — each answer given then marks it */
  private def resumption(p: Perform, r: OStep.Resume[Any, KR, Any], host: Answers, pending: Boolean): Resume[Any, Any] =
    new Resume[Any, Any]:
      private def on(v: Any): Step[Any] = safely(loop(r(if pending then PendingSig(v) else v), host))
      def resume(x: Any): Step[Any] = on(x)
      def fail(e: Throwable): Step[Any] = on(FailSig(e))
      def cancel(): Step[Any] =
        if p.region then new Step.Yield[Any, Any](trust[Op[Any]](p.op), resumption(p, r, host, pending = true))
        else on(CancelSig)

  private def safely(s: => Step[?]): Step[Any] = try trust[Step[Any]](s) catch case NonFatal(e) => new Step.Failed[Any](e)

  // -- the protocol

  /** EXPORT: the program stepped on okay-one's machine */
  def computation[A](program: Program[A], row: java.util.Set[Type]): Computation[A] =
    val fixed = java.util.Collections.unmodifiableSet(new java.util.HashSet[Type](row))
    new Computation[A]:
      def start(host: Answers): Step[A] = trust[Step[A]](safely(loop(built(program).step, host)))
      def effects(): java.util.Set[Type] = fixed

  /** what a foreign computation's operation is answered in place by here: the nearest handler of its effect, if it
   * answers in place; none, the host */
  private def answersFrom(env: EnvV): Answers = new Answers:
    private def nearest(t: Type): Handler[?, ?] | Null = env.handlers.find(_.effect() eq t).orNull
    def answers(t: Type): Boolean = nearest(t) match
      case null => env.host.answers(t)
      case h => h.isInstanceOf[Answering[?, ?]]
    def value[X](op: Op[X]): X = nearest(op.effect()) match
      case null => env.host.value(op)
      case a: Answering[?, ?] => trust[X](a.value(op))
      case _ => throw IllegalStateException(s"not answered in place: $op")

  /** IMPORT: a foreign computation's steps, taken here — its operation performed here, the answer (a value, a
   * failure) given to its `k`; an abort or a cancellation crossing it cancels it, its finalizers' operations answered
   * here, then goes on. `after`: the signal it is being cancelled for — its last step, whatever it is, gives way to it */
  private def drive(s: Step[?], after: Signal | Null): U = s match
    case y: Step.Yield[?, ?] =>
      val k = trust[Resume[Any, Any]](y.k())
      request(Perform(y.op(), region = false)).flatMap(v => answerForeign(k, v, after))
    case _ if after != null => unwind(after)
    case d: Step.Done[?] => Cont.pure(d.value())
    case f: Step.Failed[?] => unwind(FailSig(f.error()))
    case _: Step.Cancelled[?] => unwind(CancelSig)

  private def answerForeign(k: Resume[Any, Any], v: Any, after: Signal | Null): U = v match
    case f: FailSig => drive(safely(k.fail(f.e)), after)
    case p: PendingSig => request(Mark()).flatMap(_ => answerForeign(k, p.v, after))
    case s: Signal => drive(safely(k.cancel()), if after != null then after else s)
    case _ => drive(safely(k.resume(v)), after)

  /** run a program with nothing left to perform: its value, or its failure thrown */
  def runValue[A](p: Program[A]): A = computation(p, java.util.Collections.emptySet[Type]()).start(Answers.NONE) match
    case d: Step.Done[A @unchecked] => d.value()
    case f: Step.Failed[A @unchecked] => throw f.error()
    case _: Step.Cancelled[A @unchecked] => throw IllegalStateException("cancelled")
    case y: Step.Yield[?, ?] => throw IllegalStateException(s"an operation with no handler: ${y.op()}")
