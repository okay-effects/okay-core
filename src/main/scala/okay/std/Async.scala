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
    val c: t.at.type = t.at
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

  /** `run(body)`: the body's value, run on the monad of `t`, this thread waiting for it — at the edge of a program
   * only; its failure raised in the context */
  def run[A](body: Body[A])(using c: Effects, has: Throws % Throwable :? c.Effs, t: Top): c.Prog[A] =
    c.delay:
      val result = java.util.concurrent.CompletableFuture[Either[Throwable, A]]()
      runAsync(using t)(body)(result.complete)
      result.get().fold(raise[Throwable, A], c.pure)
