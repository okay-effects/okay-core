package okay.std

import okay.core.*

import java.util.concurrent.atomic.AtomicReference
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
              val slot = AtomicReference[Slot[X]](Slot.Waiting)
              register: r =>
                if !slot.compareAndSet(Slot.Waiting, Slot.Ready(r)) then go(resume(r))
              if slot.compareAndSet(Slot.Waiting, Slot.Detached) then f.pure(())
              else slot.get match
                case Slot.Ready(r) => resume(r)
                case _ => f.pure(()))(body)
    go(handled)

  /** a stream's body over `Async`: a program yielding `W`, waiting when it will */
  type Yields[W] = (c: Effects { type Effs = Emit % W +: Async +: Fails; type S = Unit }) ?=> c.Prog[Unit]

  /** what an asynchronous stream hands its consumer: an element, and `more()` to ask for the next — the body waits
   * at its `yield_` until asked, so it never runs ahead of the consumer; or the end; or the failure */
  enum Step[W]:
    case Next(w: W, more: () => Unit)
    case Done()
    case Failed(e: Throwable)

  /** `stream(t)(body)(on)`: the body run on the monad of `t`, each element given to `on` with the demand for the
   * next, `more` — called in place or later, from any thread; the run goes on in the thread that asks. A `yield_` is
   * an `await` of the consumer's demand, so a consumer asking at once is a loop, not a frame */
  def stream[W](t: Top)(body: Yields[W])(on: Step[W] => Unit): Unit =
    runAsync(using t)(emitting(on)(body))(r => on(r.fold(Step.Failed(_), _ => Step.Done())))
  private def emitting[W](on: Step[W] => Unit)(body: Yields[W])(using c: Effects { type Effs = Async +: Fails; type S = Unit })
      : c.Prog[Unit] =
    Effects.handle(new Handler[c.Prog, Emit % W, Unit, Unit]:
      def ret(u: Unit): Unit = ()
      def apply[X](op: Emit[W, X], k: X => c.Prog[Unit]): c.Prog[Unit] = op match
        case Emit.Yield(w) => await[Unit](cb => on(Step.Next(w, () => cb(Right(()))))).flatMap(_ => k(())))(body)

  /** `iterator(t)(body)`: an asynchronous stream as a blocking `Iterator` — `hasNext` asks for the next element and
   * waits for it; nothing runs ahead of it. A failure thrown by `hasNext` */
  def iterator[W](t: Top)(body: Yields[W]): Iterator[W] = new Iterator[W]:
    private val steps = java.util.concurrent.LinkedBlockingQueue[Step[W]]()
    private var started = false
    private var current: Step[W] | Null = null
    private var more: () => Unit = () => ()
    def hasNext: Boolean =
      if current == null then
        if !started then { started = true; stream(t)(body)(steps.put) } else more()
        current = steps.take()
      current match
        case Step.Next(_, m) => more = m; true
        case Step.Done() => false
        case Step.Failed(e) => throw e
        case null => false
    def next(): W =
      if !hasNext then throw java.util.NoSuchElementException("Async.Step.Done")
      current match
        case Step.Next(w, _) => current = null; w
        case _ => throw java.util.NoSuchElementException("Async.Step.Done")

  /** `run(body)`: the body's value, run on the context's monad — at its top, `c.top` — this thread waiting for it,
   * at the edge of a program only; its failure raised in the context */
  def run[A](body: Body[A])(using c: Effects, has: Throws % Throwable :? c.Effs): c.Prog[A] =
    c.delay:
      val result = java.util.concurrent.CompletableFuture[Either[Throwable, A]]()
      runAsync(using c.top)(body)(result.complete)
      result.get().fold(raise[Throwable, A], c.pure)
