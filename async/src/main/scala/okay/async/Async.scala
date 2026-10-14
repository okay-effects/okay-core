package okay.async

import okay.core.*
import okay.one.Cont
import okay.one.handler.Deep
import okay.one.step.Step
import okay.one.step.Step.step
import okay.std.*

import scala.util.control.NonFatal

/** ASYNC: a program that waits — on a computation suspended, `Run`, or on a callback, `Await`, whose `Left` is the
 * error channel: it fails the whole run. Not handled in the program: the driver outside's, the program stepped
 * (`runAsync`): the run goes on in the callback's thread when the callback comes later, in place when it came
 * already — so a callback answered at once is a loop, not a frame */
enum Async[A]:
  case Run[A](run: () => A) extends Async[A]
  case Await[A](register: (Either[Throwable, A] => Unit) => Unit) extends Async[A]

/** a computation, suspended — run when the run gets to it */
def async[A](a: => A): Op[Async, A] = perform(Async.Run(() => a))
/** the value a callback is called with, once */
def await[A](register: (Either[Throwable, A] => Unit) => Unit): Op[Async, A] = perform(Async.Await(register))

object Async:
  /** what is under `Async` at the top: the failure, `Throws % Throwable` — a `Left` at a callback, a throw in a
   * computation, or the program's own `raise` */
  type Fails = Throws % Throwable +: Pure
  /** the row a body of `runAsync` runs over */
  type In = Async +: Fails
  /** a body `runAsync` runs: at its place, over `Async +: Fails` */
  type Body[A] = Place[In] ?=> Eff[In, A]

  /** where a callback's value is, against the run that waits on it: not yet; come while the run registered, to be
   * taken in place; or the run gone, to be resumed by the callback */
  private enum Slot[+A]:
    case Waiting
    case Ready(r: Either[Throwable, A])
    case Detached

  /** `runAsync(body)(done)`: the body run, `done` given its value or its failure, once — in this thread, or in a
   * callback's. The program is STEPPED: its failures handled first (`Throws`, taken from the row wherever it is),
   * each operation of `Async` given back at the top with the rest as data; a resumption after a wait is a run of
   * the rest, the run going on in the callback's thread when it comes later, in place when it came already — a
   * loop, not a frame */
  def runAsync[A](body: Body[A])(done: Either[Throwable, A] => Unit): Unit =
    val p: Eff[Async +: Pure, Either[Throwable, A]] =
      Cont.handle(body(using Place.head))(okay.one.std.Throws.either[Throwable, Async +: Pure, A])
    drive(p.step, done)

  private def drive[A](s0: Step[Async +: Pure, Either[Throwable, A]], done: Either[Throwable, A] => Unit): Unit =
    var s: Step[Async +: Pure, Either[Throwable, A]] | Null = s0
    while s != null do
      s = s match
        case Step.Done(r) =>
          done(r)
          null
        case Step.Failed(e) =>
          done(Left(e))
          null
        case y: Step.Yield[r, a, e, x] => y.in match
          case _: okay.one.row.In.Here[f, t] => next(y.op, y.resume, done)
          case th: okay.one.row.In.There[f, g, t] => okay.one.row.In.absurd(th.in)

  /** the step after `op`'s value: at once for a computation, and for a callback that came while registering; else
   * `null`, the callback driving on from its thread. A failure raised where the operation was */
  private def next[X, A](op: Async[X], resume: Step.Resume[X, Async +: Pure, Either[Throwable, A]],
      done: Either[Throwable, A] => Unit): Step[Async +: Pure, Either[Throwable, A]] | Null =
    def answered(r: Either[Throwable, X]): Step[Async +: Pure, Either[Throwable, A]] = r match
      case Right(x) => resume(x)
      case Left(e) => resume.fail(e)
    op match
      case Run(run) => answered(try Right(run()) catch { case NonFatal(e) => Left(e) })
      case Await(register) =>
        val slot = Ref[Slot[X]](Slot.Waiting)
        register: r =>
          if !slot.compareAndSet(Slot.Waiting, Slot.Ready(r)) then drive(answered(r), done)
        if slot.compareAndSet(Slot.Waiting, Slot.Detached) then null
        else slot.get match
          case Slot.Ready(r) => answered(r)
          case _ => null

  /** a source over `Async`: a program yielding `W`, waiting when it will — run by `Async.stream` */
  type Source[W] = Place[Emit % W +: In] ?=> Eff[Emit % W +: In, Unit]

  /** the end of a stream's run asked by its consumer: the failure of the `yield_` it waits at, the run aborted
   * there as by any failure; a run ended by it is a stream ended */
  object Cancelled extends RuntimeException("Async stream cancelled", null, false, false)

  /** AN ASYNCHRONOUS GENERATOR, NOT STARTED: a body that yields and waits. `map` and `filter` fused into what its
   * handler does with an element. Pulled (`iterator`, `take`): a `yield_` waits until the next element is asked for
   * — never ahead of its consumer — and `close()` cancels the rest. Pushed (`subscribe`, not waiting; `foreach`,
   * `fold`, `toList`, waiting for the end) */
  abstract class Generator[W] extends Stream[W]:
    /** the body under its `Emit` handler, each element and the rest through `out` */
    def handled(out: (W, Eff[In, Unit]) => Eff[In, Unit]): Eff[In, Unit]

    /** each element to `f` where it is yielded, in the thread the run is in; `done` at the end, once */
    def subscribe(f: W => Unit)(done: Either[Throwable, Unit] => Unit): Unit =
      runAsync(handled((w, rest) => { f(w); rest }))(done)
    def push(f: W => Unit): Unit =
      val result = Mailbox[Either[Throwable, Unit]]()
      subscribe(f)(result.put)
      result.take().fold(e => throw e, identity)

    def iterator: Stream.Iter[W] = new Stream.Iter[W]:
      private enum Pulled:
        case Next(w: W, resume: Either[Throwable, Unit] => Unit)
        case End(r: Either[Throwable, Unit])
      private val steps = Mailbox[Pulled]()
      @volatile private var cancelled = false
      private var started = false
      private var current: Pulled | Null = null
      private var resume: Either[Throwable, Unit] => Unit = _ => ()
      private def ask(): Unit =
        if !started then
          started = true
          runAsync(handled((w, rest) =>
            await[Unit](cb => if cancelled then cb(Left(Cancelled)) else steps.put(Pulled.Next(w, cb))).at[In].flatMap(_ => rest)))(r =>
            steps.put(Pulled.End(r)))
        else resume(Right(()))
        current = steps.take()
      def hasNext: Boolean =
        if current == null then ask()
        current match
          case Pulled.Next(_, cb) => resume = cb; true
          case Pulled.End(Left(e)) if e ne Cancelled => throw e
          case _ => false
      def next(): W =
        if !hasNext then throw java.util.NoSuchElementException("Async.Generator")
        current match
          case Pulled.Next(w, _) => current = null; w
          case _ => throw java.util.NoSuchElementException("Async.Generator")
      /** the run cancelled where it waits, and waited for to its end */
      override def close(): Unit =
        if started && !cancelled then
          cancelled = true
          var step: Pulled = current match
            case null => resume(Left(Cancelled)); steps.take()
            case s: Pulled => s
          while step match
              case Pulled.Next(_, cb) => cb(Left(Cancelled)); step = steps.take(); true
              case _ => false
          do ()
          current = step

    override def map[B](f: W => B): Generator[B] =
      val src = this
      new Generator[B]:
        def handled(out: (B, Eff[In, Unit]) => Eff[In, Unit]): Eff[In, Unit] = src.handled((w, rest) => out(f(w), rest))
    override def filter(p: W => Boolean): Generator[W] =
      val src = this
      new Generator[W]:
        def handled(out: (W, Eff[In, Unit]) => Eff[In, Unit]): Eff[In, Unit] =
          src.handled((w, rest) => if p(w) then out(w, rest) else rest)
    override def withFilter(p: W => Boolean): Generator[W] = filter(p)

  /** `stream(body)`: the body, yielding and waiting, as an asynchronous `Generator` */
  def stream[W](body: Source[W]): Generator[W] = new Generator[W]:
    def handled(out: (W, Eff[In, Unit]) => Eff[In, Unit]): Eff[In, Unit] =
      handleIn(new Deep[Emit % W, In, Unit, Unit]:
        def ret(u: Unit): Eff[In, Unit] = pure(())
        def apply[X](op: Emit[W, X], k: X => Eff[In, Unit]): Eff[In, Unit] = op match
          case Emit.Yield(w) => out(w, k(())))(body)

  /** `run(body)`: the body's value, this thread waiting for it, at the edge of a program only; its failure raised
   * — a program needing `Throws % Throwable` */
  def run[A](body: Body[A]): A ! Throws % Throwable = defer:
    val result = Mailbox[Either[Throwable, A]]()
    runAsync(body)(result.put)
    result.take().fold(e => raise(e).perform, a => pure(a))
