package okay.zero.std

import okay.zero.*

import java.util.concurrent.atomic.AtomicReference
import scala.util.control.NonFatal

/** ASYNC: a program that waits — on a computation suspended, `Run`, or on a callback, `Await`, whose `Left` is the
 * error channel: it fails the whole run. Handled at the top, by `runAsync`: the run goes on in the callback's
 * thread when the callback comes later, in place when it came already */
enum Async[+A]:
  case Run[A](run: () => A) extends Async[A]
  case Await[A](register: (Either[Throwable, A] => Unit) => Unit) extends Async[A]

/** a computation, suspended — run when the run gets to it */
def async[A](a: => A)(using c: Ctx, has: Async :? c.R): c.P[A] = perform[Async, A](Async.Run(() => a))
/** the value a callback is called with, once */
def await[A](register: (Either[Throwable, A] => Unit) => Unit)(using c: Ctx, has: Async :? c.R): c.P[A] =
  perform[Async, A](Async.Await(register))

object Async:
  /** what is under `Async` at the top: the failure, `Throws % Throwable` */
  type Fails = Throws % Throwable +: Empty
  /** the top's level, its answer `Unit`; the context under the failure's handler there; under `Async` there */
  type Top = Level[Empty, Unit, Unit]
  type InFails = Level[Fails, Top, Unit]
  /** the context a body run by `runAsync` is written in */
  type Body = At[Async +: Fails, InFails, Unit]

  private enum Slot[+A]:
    case Waiting
    case Ready(r: Either[Throwable, A])
    case Detached

  /** `runAsync(body)(done)`: the body run, `done` given its value or its failure, once — in this thread, or in a
   * callback's. A resumption after a wait is a run of its own, at the top: its types are the first run's */
  def runAsync[A](body: Body ?=> Eff[Async +: Fails, InFails, Unit, Unit, A])(done: Either[Throwable, A] => Unit): Unit =
    // the run up to its end or to a wait, a failure raised in it `done`'s, the rest of the run aborted
    def go(p: Eff[Fails, Top, Unit, Unit, Unit]): Unit =
      okay.zero.run[Unit](handle(new Handler[[X] =>> Eff[Empty, Unit, Unit, Unit, X], Throws % Throwable, Unit, Unit]:
        def ret(u: Unit): Unit = ()
        def apply[X](op: Throws[Throwable, X], k: X => Eff[Empty, Unit, Unit, Unit, Unit]): Eff[Empty, Unit, Unit, Unit, Unit] = op match
          case Throws.Raise(e) => done(Left(e)); pure(()))(p))
    def handled(using c: At[Fails, Top, Unit]): Eff[Fails, Top, Unit, Unit, Unit] =
      handle(new Handler[c.P, Async, A, Unit]:
        def ret(a: A): Unit = done(Right(a))
        def apply[X](op: Async[X], k: X => c.P[Unit]): c.P[Unit] =
          def resume(r: Either[Throwable, X]): c.P[Unit] = r.fold(raise[Throwable, Unit], k)
          op match
            case Run(run) => resume(try Right(run()) catch { case NonFatal(e) => Left(e) })
            case Await(register) =>
              val slot = AtomicReference[Slot[X]](Slot.Waiting)
              register: r =>
                if !slot.compareAndSet(Slot.Waiting, Slot.Ready(r)) then go(resume(r))
              if slot.compareAndSet(Slot.Waiting, Slot.Detached) then pure(())
              else slot.get match
                case Slot.Ready(r) => resume(r)
                case _ => pure(()))(body)
    go(handled(using Ctxs.at[Fails, Top, Unit]))

  /** `run(body)`: the body's value, this thread waiting for it — at the edge of a program only; its failure raised in
   * the context */
  def run[A](body: Body ?=> Eff[Async +: Fails, InFails, Unit, Unit, A])(using c: Ctx, has: Throws % Throwable :? c.R): c.P[A] =
    delay:
      val result = java.util.concurrent.CompletableFuture[Either[Throwable, A]]()
      runAsync(body)(result.complete)
      result.get().fold(raise[Throwable, A], pure)
