package okay.cont

import okay.core.*
import okay.std.{Throws, raise}
import okay.std.Async.{Run, Await, Fails}

import java.util.concurrent.atomic.AtomicReference
import scala.util.control.NonFatal

/** `okay.std.Async` RUN ON `Cont`: master's effect, its run here — the run goes on in the callback's thread when the
 * callback comes later, in place when it came already, so a callback answered at once is a loop, not a frame */
object Async:
  /** where a callback's value is, against the run that waits on it: not yet; come while the run registered, to be
   * taken in place; or the run gone, to be resumed by the callback */
  private enum Slot[+A]:
    case Waiting
    case Ready(r: Either[Throwable, A])
    case Detached

  /** `runAsync(body)(done)`: the body run, `done` given its value or its failure, once — in this thread, or in a
   * callback's */
  def runAsync[A](body: At[okay.std.Async +: Fails, Unit] ?=> Free[okay.std.Async +: Fails, A])(done: Either[Throwable, A] => Unit): Unit =
    given At[Fails, Unit] = At()
    // the run up to its end or to a wait, a failure raised in it `done`'s, the rest of the run aborted
    def go(c: Free[Fails, Unit]): Unit =
      c.handle(new Handler[[Z] =>> Cont[Pure, Unit, Unit, Z], Throws % Throwable, Unit, Unit]:
        def ret(u: Unit): Unit = ()
        def apply[X](op: Throws[Throwable, X], k: X => Free[Pure, Unit]): Free[Pure, Unit] = op match
          case Throws.Raise(e) => done(Left(e)); Cont.pure(())).value
    val h = new Handler[[Z] =>> Cont[Fails, Unit, Unit, Z], okay.std.Async, A, Unit]:
      def ret(a: A): Unit = done(Right(a))
      def apply[X](op: okay.std.Async[X], k: X => Free[Fails, Unit]): Free[Fails, Unit] =
        def resume(r: Either[Throwable, X]): Free[Fails, Unit] = r.fold(raise[Throwable, Unit], k)
        op match
          case Run(run) => resume(try Right(run()) catch { case NonFatal(e) => Left(e) })
          case Await(register) =>
            val slot = AtomicReference[Slot[X]](Slot.Waiting)
            register: r =>
              if !slot.compareAndSet(Slot.Waiting, Slot.Ready(r)) then go(resume(r))
            if slot.compareAndSet(Slot.Waiting, Slot.Detached) then Cont.pure(())
            else slot.get match
              case Slot.Ready(r) => resume(r)
              case _ => Cont.pure(())
    go(body(using At()).handle(h))

  /** `run(body)`: the body's value, this thread waiting for it — at the edge of a program only; its failure raised
   * in the context */
  def run[A](body: At[okay.std.Async +: Fails, Unit] ?=> Free[okay.std.Async +: Fails, A])
            (using c: Effects, has: Throws % Throwable :? c.Effs): c.Prog[A] =
    c.delay:
      val result = java.util.concurrent.CompletableFuture[Either[Throwable, A]]()
      runAsync(body)(result.complete)
      result.get().fold(raise[Throwable, A], c.pure)
