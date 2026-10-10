package okay.async

import okay.core.*
import okay.std.*

import scala.util.control.NonFatal

/** ASYNC: a program that waits — on a computation suspended, `Run`, or on a callback, `Await`, whose `Left` is the
 * error channel: it fails the whole run. Handled at the top, by `runAsync`: the run goes on in the callback's
 * thread when the callback comes later, in place when it came already — so a callback answered at once is a
 * loop, not a frame */
enum Async[+A]:
  case Run[A](run: () => A) extends Async[A]
  case Await[A](register: (Either[Throwable, A] => Unit) => Unit) extends Async[A]

/** a computation, suspended — run when the run gets to it */
def async[A](a: => A)(using c: Effects, has: Async :? c.Effs): c.Prog[A] = perform[Async, A](Async.Run(() => a))
/** the value a callback is called with, once */
def await[A](register: (Either[Throwable, A] => Unit) => Unit)(using c: Effects, has: Async :? c.Effs): c.Prog[A] =
  perform[Async, A](Async.Await(register))

object Async:
  /** what is under `Async` at the top: the failure, `Throws % Throwable` — a `Left` at a callback, a throw in a
   * computation, or the program's own `raise` */
  type Fails = Throws % Throwable +: Pure
  /** a body `runAsync` runs: in a context over `Async +: Fails`, at the answer `Unit` */
  type Body[A] = (c: Effects { type Effs = Async +: Fails; type S = Unit }) ?=> c.Prog[A]

  /** where a callback's value is, against the run that waits on it: not yet; come while the run registered, to be
   * taken in place; or the run gone, to be resumed by the callback */
  private enum Slot[+A]:
    case Waiting
    case Ready(r: Either[Throwable, A])
    case Detached

  /** `runAsync(body)(done)`: the body run on the monad of `t`, `done` given its value or its failure, once — in this
   * thread, or in a callback's: a resumption after a wait is a run of its own, at the top, the run going on in the
   * callback's thread when it comes later, in place when it came already — a loop, not a frame */
  def runAsync[A](using t: Top)(body: Body[A])(done: Either[Throwable, A] => Unit): Unit =
    val c = t.at[Unit]
    // the run up to its end or to a wait, under the failure's handler — a failure `done`'s, the rest aborted
    def go(p: c.Body[Throws % Throwable, Unit, Unit]): Unit =
      t.run(Effects.handle(using c)(new Handler[c.Prog, Throws % Throwable, Unit, Unit]:
        def ret(u: Unit): Unit = ()
        def apply[X](op: Throws[Throwable, X], k: X => c.Prog[Unit]): c.Prog[Unit] = op match
          case Throws.Raise(e) => done(Left(e)); c.pure(()))(p))
    def handled(using f: c.Handled[Throws % Throwable, Unit]): f.Prog[Unit] =
      Effects.handle(using f)(new Handler[f.Prog, Async, A, Unit]:
        def ret(a: A): Unit = done(Right(a))
        def apply[X](op: Async[X], k: X => f.Prog[Unit]): f.Prog[Unit] =
          def resume(r: Either[Throwable, X]): f.Prog[Unit] = r.fold(raise[Throwable, Unit], k)
          op match
            case Run(run) => resume(try Right(run()) catch { case NonFatal(e) => Left(e) })
            case Await(register) =>
              val slot = Ref[Slot[X]](Slot.Waiting)
              register: r =>
                if !slot.compareAndSet(Slot.Waiting, Slot.Ready(r)) then go(resume(r))
              if slot.compareAndSet(Slot.Waiting, Slot.Detached) then f.pure(())
              else slot.get match
                case Slot.Ready(r) => resume(r)
                case _ => f.pure(()))(body)
    go(handled)

  /** a source over `Async`: a program yielding `W`, waiting when it will — run by `Async.stream` */
  type Source[W] = (c: Effects { type Effs = Emit % W +: Async +: Fails; type S = Unit }) ?=> c.Prog[Unit]

  /** a context of `runAsync`'s body */
  type In = Effects { type Effs = Async +: Fails; type S = Unit }

  /** the end of a stream's run asked by its consumer: the failure of the `yield_` it waits at, the run aborted
   * there as by any failure; a run ended by it is a stream ended */
  object Cancelled extends RuntimeException("Async stream cancelled", null, false, false)

  /** AN ASYNCHRONOUS GENERATOR, NOT STARTED: a body that yields and waits, on the monad of `top`. `map` and `filter`
   * fused into what its handler does with an element. Pulled (`iterator`, `take`): a `yield_` waits until the next
   * element is asked for — never ahead of its consumer — and `close()` cancels the rest. Pushed (`subscribe`, not
   * waiting; `foreach`, `fold`, `toList`, waiting for the end) */
  abstract class Generator[W] extends Stream[W]:
    val top: Top
    /** the body under its `Emit` handler, each element and the rest through `out` */
    def handled(out: (c: In) ?=> (W, c.Prog[Unit]) => c.Prog[Unit])(using c: In): c.Prog[Unit]

    /** each element to `f` where it is yielded, in the thread the run is in; `done` at the end, once */
    def subscribe(f: W => Unit)(done: Either[Throwable, Unit] => Unit): Unit =
      runAsync(using top)(handled((w, rest) => { f(w); rest }))(done)
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
          runAsync(using top)(handled((w, rest) =>
            await[Unit](cb => if cancelled then cb(Left(Cancelled)) else steps.put(Pulled.Next(w, cb))).flatMap(_ => rest)))(r =>
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
        val top: src.top.type = src.top
        def handled(out: (c: In) ?=> (B, c.Prog[Unit]) => c.Prog[Unit])(using c: In): c.Prog[Unit] =
          src.handled((w, rest) => out(f(w), rest))
    override def filter(p: W => Boolean): Generator[W] =
      val src = this
      new Generator[W]:
        val top: src.top.type = src.top
        def handled(out: (c: In) ?=> (W, c.Prog[Unit]) => c.Prog[Unit])(using c: In): c.Prog[Unit] =
          src.handled((w, rest) => if p(w) then out(w, rest) else rest)
    override def withFilter(p: W => Boolean): Generator[W] = filter(p)

  /** `stream(t)(body)`: the body, yielding and waiting, as an asynchronous `Generator` on the monad of `t` */
  def stream[W](t: Top)(body: Source[W]): Generator[W] = new Generator[W]:
    val top: t.type = t
    def handled(out: (c: In) ?=> (W, c.Prog[Unit]) => c.Prog[Unit])(using c: In): c.Prog[Unit] =
      Effects.handle(new Handler[c.Prog, Emit % W, Unit, Unit]:
        def ret(u: Unit): Unit = ()
        def apply[X](op: Emit[W, X], k: X => c.Prog[Unit]): c.Prog[Unit] = op match
          case Emit.Yield(w) => out(w, k(())))(body)

  /** `run(body)`: the body's value, run on the context's monad — at its top, `c.top` — this thread waiting for it,
   * at the edge of a program only; its failure raised in the context */
  def run[A](body: Body[A])(using c: Effects, has: Throws % Throwable :? c.Effs): c.Prog[A] =
    c.delay:
      val result = Mailbox[Either[Throwable, A]]()
      runAsync(using c.top)(body)(result.put)
      result.take().fold(raise[Throwable, A], c.pure)
