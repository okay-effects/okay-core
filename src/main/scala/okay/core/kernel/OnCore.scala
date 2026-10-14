package okay.core.kernel

import okay.kernel.{Aborting as KAborting, Answering as KAnswering, Answers, Computation, Control, Effects, Fn, General,
  Handler as KHandler, Op, Prog, Program, Resume, Returning, Step, Thunk, Translating as KTranslating, Type}
import okay.one.{!, Cont}
import okay.one.Cont.{defer, pure}
import okay.one.handler.{Aborting, Deep, Linear, Recovering}
import okay.one.machine.Machine
import okay.one.row.{+:, In, Key, Pure, Row}
import okay.one.step.Step as OStep
import okay.one.step.Step.step
import scala.util.control.NonFatal

/**
 * OKAY-CORE AS AN IMPLEMENTATION OF THE KERNEL: okay-kernel's `Effects` and `Control` on the one monad's machine
 * (`okay.one`), NATIVELY — a kernel program (`Prog`) IS a program of the machine (a `Cont` is a `Prog`: nothing boxed) over one
 * row, `Req +: Pure`: `pure` a `Return`, `flatMap` a `Bind`, `delay` a `Defer`; a kernel handler a handler of the
 * machine's, of the form its shape asks for; `recover`, `ensuring`, `uncancelable` the machine's guards; failures the
 * machine's `Raise`, unwound by it; export the machine's `Step`. Nothing of the bridge's own runs a program or
 * forwards a request: the machine's loop does it all, in constant stack, `k` data.
 *
 * HOW. The kernel finds a handler at RUN time, by the operation's key, where the operation runs — a `k` resumed
 * under other handlers sees those. So every operation of the kernel is performed AT A PLACE BY KEY (`In.At`): the
 * machine walks the levels in force to the nearest handler that says the key is its own (`Handler.at`), as it walks
 * a path — answered in place, aborted, or out to it with the rest as `k`, the levels passed in it. Every delimiter
 * of the bridge is a handler of the one effect `Req` over the row `KR`, at its own keys:
 *  - a kernel handler, by shape: `Answering` and `Translating` a `Linear` (the rest through it; a shift0 takes it),
 *    `General` (`Returning`'s answer its `ret`) a `Deep`, `Aborting` an `Aborting` — each at its effect's key, and
 *    the first two at a shift0's key (a handler is a delimiter) and at the key the handlers in force are asked by;
 *  - `$` a `Deep` at a shift0's key alone, its `ret` the `k` given;
 *  - a request: a kernel operation (`Perform`), a shift0's body (`Shift`), the handlers in force asked for (`Env`,
 *    each handler adding itself and asking on).
 * A failure, an abort, a cancel are the machine's: `recover` a `Recovering` guard, `ensuring` a guard with a
 * finalizer, `uncancelable` a masked one; an `Aborting` handler's abort unwinds through the guards between; a `k`
 * handed out resumed with a value, failed (`Fail`, raised inside its levels), or cancelled (the machine's cancel —
 * under a mask the same step given back, the cancel fired when the mask is left).
 *
 * THE PROTOCOL. Export: the program stepped (`Step`): a request come to the top is the host's in place if it answers
 * its effect, else `Step.Yield` with the rest as the `Resume` — multi-shot, from any thread. Import: the handlers in
 * force asked for (`Env`), the computation started with them as its host in place, its yields performed here as
 * operations of this program, each under a guard: its value to the computation's `k`, a failure to `k.fail`, an abort
 * or a cancel crossing to `k.cancel` — its finalizers' operations performed here — then on.
 *
 * TRUSTED STEPS, two of the bridge's: `trust` (a value given where the program, built typed, expects it: the protocol
 * is untyped by design) and `inside` (the row is phantom to the machine: a body of the bridge under one more delimiter
 * of the bridge is the same program). The kernel's own two, in `okay.one.row`: a key names one effect (`Key.at`), a
 * place by key is in every row (`In.At.again`).
 */
object OnCore extends Effects with Control:

  override def toString: String = "OnCore"

  /** THE BRIDGE'S ONE EFFECT: a request, performed at a place by key */
  sealed abstract class Req[X]
  /** a kernel operation, at its effect's key */
  final class Perform(val op: Op[Any]) extends Req[Any]
  /** a shift0's body, at the nearest handler's or `$`'s */
  final class Shift(val body: Fn[Fn[Any, Prog[Any]], Prog[Any]]) extends Req[Any]
  /** the handlers in force, innermost first: each adds itself and asks on; the top adds the host */
  final class Env(val acc: List[KHandler[?, ?]]) extends Req[Any]

  /** the bridge's row, and its programs, untyped: every program of the bridge over it */
  type KR = Req +: Pure
  type U = Any ! KR

  /** A KERNEL PROGRAM ON THE MACHINE: the machine's program itself — a `Cont` is a `Prog`, nothing boxed */
  private def P[A](c: U): Prog[A] = trust[Prog[A]](c)

  // -- the trusted steps

  /** THE TRUSTED STEP of the values: a value given where the program, built typed, expects it */
  private[kernel] def trust[T](v: Any): T = v.asInstanceOf[T]

  /** THE TRUSTED STEP of the row: a body of the bridge, over `KR`, under one more delimiter of the bridge — over
   * `Req +: KR`. The row is phantom to the machine; every operation of the bridge is at a place by key, in both */
  private[kernel] def inside(u: U): Any ! (Req +: KR) = u.asInstanceOf[Any ! (Req +: KR)]

  /** a kernel program given to this implementation: one of the machine's, over the bridge's row — `trust`'s row */
  private[kernel] def unbox(p: Prog[?]): U = p match
    case c: Cont[?, ?, ?, ?] => trust[U](c)
    case _ => throw IllegalArgumentException(s"not a program of okay-core's bridge: $p")

  // -- the keys: a kernel handler's its effect's (okay-kernel's `Key`, interned, one a type — the ref); a shift0's;
  //    the handlers in force asked for

  private val shiftKey: Key[Req] = Key.fresh[Req]()
  private val envKey: Key[Req] = Key.fresh[Req]()

  /** a request at a place by key */
  private[kernel] def request(r: Req[Any], at: In[Req, KR]): U = Cont.perform[Req, KR, Any](r)(using at)
  private def built(p: Program[?]): U = unbox(p.in(this))

  // -- Effects

  def pure[A](a: A): Prog[A] = P(Cont.pure(a))
  def flatMap[A, B](p: Prog[A], f: Fn[A, Prog[B]]): Prog[B] = P(unbox(p).flatMap(v => unbox(f.apply(trust[A](v)))))
  override def map[A, B](p: Prog[A], f: Fn[A, B]): Prog[B] = P(unbox(p).map(v => f.apply(trust[A](v))))
  def delay[A](p: Thunk[Prog[A]]): Prog[A] = P(defer(unbox(p.get())))
  def perform[X](op: Op[X]): Prog[X] = P(request(Perform(trust[Op[Any]](op)), In.At[Req, KR](op.key())))
  def handle[A, R](h: KHandler[A, R], body: Program[A]): Prog[R] =
    val own = Key.of[Req](h.key())
    P(defer(h match
      case ab: KAborting[?, ?] => inside(built(body)).handle(AbortingD(ab, own))
      case _: General[?, ?] => inside(built(body)).handle(GeneralD(h, own))
      case _: (KAnswering[?, ?] | KTranslating[?, ?]) => inside(built(body)).handle(PassingD(h, own))
      // a return alone: its operations none to take
      case _ => inside(built(body)).handle(GeneralD(h, own))))
  def shift0[A, I, O](body: Fn[Fn[A, Prog[I]], Prog[O]]): Prog[A] =
    P(request(Shift(trust[Fn[Fn[Any, Prog[Any]], Prog[Any]]](body)), shiftKey.place[KR]))
  def dollar[A, I, O](body: Program[A], k: Fn[A, Prog[I]]): Prog[O] =
    P(defer(inside(built(body)).handle(DollarD(trust[Fn[Any, Prog[Any]]](k)))))
  /** a failure: raised where it is run — the machine's `Raise`, from the guarded call */
  def failed[A](e: Throwable): Prog[A] = P(defer(throw e))
  def recover[A](body: Program[A], h: Fn[Throwable, Prog[A]]): Prog[A] =
    P(defer(built(body).recover(e => unbox(h.apply(e)))))
  def ensuring[A](body: Program[A], finalizer: Program[Void]): Prog[A] =
    P(defer(built(body).ensure(built(finalizer).map(_ => ()))))
  def uncancelable[A](body: Program[A]): Prog[A] = P(defer(built(body).uncancelable))
  /** IMPORT: another's program, its operations answered where it runs */
  def foreign[A](c: Computation[A]): Prog[A] =
    P(request(Env(Nil), envKey.place[KR]).flatMap(env => drive(safely(c.start(answersFrom(trust[EnvV](env)))), false)))

  // -- the handlers: each at its keys

  /** a kernel handler's return: `Returning`'s answer, `General`'s program, else the value */
  private def returned(h: KHandler[?, ?], v: Any): U = h match
    case r: Returning[?, ?] => Cont.pure(trust[Returning[Any, Any]](r).answer(v))
    case g: General[?, ?] => unbox(trust[General[Any, Any]](g).ret(v, OnCore))
    case _ => Cont.pure(v)

  /** the places a handler of the kernel is at: its effect's key; a shift0's (a handler is a delimiter); the key the
   * handlers in force are asked by */
  private def owns[F[_], R <: Row](own: Key[Req], in: In.At[F, R]): In.Same[Req, F] | Null = own.at(in) match
    case null => shiftKey.at(in) match
      case null => envKey.at(in)
      case s => s
    case s => s

  /** a handler leaving the rest — `Answering`: its value, the rest with it; `Translating`: its program outside, the
   * rest then with its value — a `Linear` of the machine's: the rest given once; a shift0 to it takes the rest
   * through it */
  private final class PassingD(h: KHandler[?, ?], own: Key[Req]) extends Linear[Req, KR, Any, Any]:
    override def at[F[_], R <: Row](in: In.At[F, R]): In.Same[Req, F] | Null = owns(own, in)
    def ret(v: Any): U = returned(h, v)
    def apply[I, O, X](r: Req[X], k: X => Cont[KR, I, O, Any]): Cont[KR, I, O, Any] = r match
      case p: Perform => h match
        case a: KAnswering[?, ?] => k(trust[X](a.value(p.op)))
        case tr: KTranslating[?, ?] => unbox(tr.translate(p.op).in(OnCore)).flatMap(v => k(trust[X](v)))
        case _ => throw IllegalStateException(s"a handler of no shape: $h")
      case s: Shift => trust[Cont[KR, I, O, Any]](unbox(s.body.apply(x => P(trust[U](k(trust[X](x)))))))
      case e: Env => request(Env(h :: e.acc), envKey.place[KR]).flatMap(v => k(trust[X](v)))

  /** a handler with a clause given `k`: `General` — a `Deep` of the machine's, `k` the rest, data */
  private final class GeneralD(h: KHandler[?, ?], own: Key[Req]) extends Deep[Req, KR, Any, Any]:
    override def at[F[_], R <: Row](in: In.At[F, R]): In.Same[Req, F] | Null = owns(own, in)
    def ret(v: Any): U = returned(h, v)
    def apply[X](r: Req[X], k: X => U): U = r match
      case p: Perform => h match
        case g: General[?, ?] => unbox(trust[General[Any, Any]](g).apply[Any](p.op, x => P(k(trust[X](x))), OnCore))
        case _ => throw IllegalStateException(s"a handler of no shape: $h")
      case s: Shift => unbox(s.body.apply(x => P(k(trust[X](x)))))
      case e: Env => request(Env(h :: e.acc), envKey.place[KR]).flatMap(v => k(trust[X](v)))

  /** a handler that never resumes: `Aborting` — the machine's, its abort unwinding through the guards between */
  private final class AbortingD(h: KAborting[?, ?], own: Key[Req]) extends Aborting[Req, KR, Any, Any]:
    override def at[F[_], R <: Row](in: In.At[F, R]): In.Same[Req, F] | Null = own.at(in)
    def ret(v: Any): U = returned(h, v)
    def abort[X](r: Req[X]): U = r match
      case p: Perform => Cont.pure(trust[KAborting[Any, Any]](h).abort(p.op))
      case _ => throw IllegalStateException("an aborting handler asked what it does not take")

  /** `body $ k`: a delimiter at a shift0's key alone, its return `k` */
  private final class DollarD(kf: Fn[Any, Prog[Any]]) extends Deep[Req, KR, Any, Any]:
    override def at[F[_], R <: Row](in: In.At[F, R]): In.Same[Req, F] | Null = shiftKey.at(in)
    def ret(v: Any): U = unbox(kf.apply(v))
    def apply[X](r: Req[X], k: X => U): U = r match
      case s: Shift => unbox(s.body.apply(x => P(k(trust[X](x)))))
      case _ => throw IllegalStateException("a $ asked what it does not take")

  // -- the top: the program stepped, its requests the host's or the protocol's

  /** a run of the machine to the protocol's step: a request come to the top — the host's in place if it answers its
   * effect, else a yield; the bridge's own answered here */
  private def loop(s: OStep[KR, Any], host: Answers): Step[Any] = s match
    case OStep.Done(a) => new Step.Done[Any](a)
    case OStep.Failed(e) => new Step.Failed[Any](e)
    case OStep.Cancelled() => new Step.Cancelled[Any]()
    case y: OStep.Yield[KR, Any, e, x] =>
      val resume = trust[OStep.Resume[Any, KR, Any]](y.resume)
      trust[Req[Any]](y.op) match
        case p: Perform =>
          val answers = try host.answers(p.op.effect()) catch case NonFatal(_) => false
          if answers then
            val next = try resume(host.value(p.op)) catch case NonFatal(e) => resume.fail(e)
            loop(next, host)
          else new Step.Yield[Any, Any](p.op, resumption(resume, host))
        case _: Shift => loop(resume.fail(IllegalStateException("shift0 with no delimiter around it")), host)
        case e: Env => loop(resume(EnvV(e.acc.reverse, host)), host)

  /** `k` of an operation handed out, as the protocol's: each call a run of the machine from the rest — resumed,
   * failed where the operation was, cancelled (under a mask the same step again, the cancel kept for its end) */
  private def resumption(r: OStep.Resume[Any, KR, Any], host: Answers): Resume[Any, Any] =
    new Resume[Any, Any]:
      def resume(x: Any): Step[Any] = safely(loop(r(x), host))
      def fail(e: Throwable): Step[Any] = safely(loop(r.fail(e), host))
      def cancel(): Step[Any] = safely(loop(r.cancel(), host))

  private def safely(s: => Step[?]): Step[Any] = try trust[Step[Any]](s) catch case NonFatal(e) => new Step.Failed[Any](e)

  // -- the protocol

  /** EXPORT: the program stepped on the machine */
  def computation[A](program: Program[A], row: java.util.Set[Type]): Computation[A] =
    val fixed = java.util.Collections.unmodifiableSet(new java.util.HashSet[Type](row))
    new Computation[A]:
      def start(host: Answers): Step[A] = trust[Step[A]](safely(loop(built(program).step, host)))
      def effects(): java.util.Set[Type] = fixed

  /** the handlers in force where a foreign computation starts, and the host */
  private[kernel] final class EnvV(val handlers: List[KHandler[?, ?]], val host: Answers)

  /** what a foreign computation's operation is answered in place by here: the nearest handler of its effect, if it
   * answers in place; none, the host */
  private def answersFrom(env: EnvV): Answers = new Answers:
    private def nearest(t: Type): KHandler[?, ?] | Null = env.handlers.find(_.effect() eq t).orNull
    def answers(t: Type): Boolean = nearest(t) match
      case null => env.host.answers(t)
      case h => h.isInstanceOf[KAnswering[?, ?]]
    def value[X](op: Op[X]): X = nearest(op.effect()) match
      case null => env.host.value(op)
      case a: KAnswering[?, ?] => a.value(op)
      case _ => throw IllegalStateException(s"not answered in place: $op")

  /** IMPORT: a foreign computation's steps, taken here — its operation performed here under a guard of its `k`
   * (`Driven`), its value to the `k`; its end this program's. `ending`: the computation is being cancelled for an
   * abort or a cancel crossing it — its last step gives way: its own end, not this program's */
  private def drive(s: Step[?], ending: Boolean): U = s match
    case y: Step.Yield[?, ?] =>
      val k = trust[Resume[Any, Any]](y.k())
      val op = trust[Op[Any]](y.op())
      request(Perform(op), In.At[Req, KR](op.key())).guard(Driven(k, ending))
    case d: Step.Done[?] => Cont.pure(d.value())
    case f: Step.Failed[?] => defer(throw f.error())
    case _: Step.Cancelled[?] => if ending then Cont.pure(()) else defer(throw Machine.Cancelled)

  /** an operation performed here for a foreign computation: its value to the computation's `k`, a failure to its
   * `k.fail`, an abort or a cancel crossing it to its `k.cancel` — its finalizers' operations performed here — then
   * the unwinding on */
  private final class Driven(k: Resume[Any, Any], ending: Boolean) extends Recovering[KR, Any, Any]:
    def ret(v: Any): U = drive(safely(k.resume(v)), ending)
    def recover(e: Throwable): U = drive(safely(k.fail(e)), ending)
    override def finalizer: Unit ! KR = drive(safely(k.cancel()), true).map(_ => ())

  /** run a program with nothing left to perform: its value, or its failure thrown */
  def runValue[A](p: Program[A]): A = computation(p, java.util.Collections.emptySet[Type]()).start(Answers.NONE) match
    case d: Step.Done[A @unchecked] => d.value()
    case f: Step.Failed[A @unchecked] => throw f.error()
    case _: Step.Cancelled[A @unchecked] => throw IllegalStateException("cancelled")
    case y: Step.Yield[?, ?] => throw IllegalStateException(s"an operation with no handler: ${y.op()}")
