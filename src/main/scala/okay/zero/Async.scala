package okay.zero

import okay.core.Effects
import okay.std.{Throws, raise}
import okay.std.Async.{Run, Await, Fails}

import java.util.concurrent.atomic.AtomicReference
import scala.util.control.NonFatal

/** `okay.std.Async` RUN ON `Cont0`: master's effect, its run here — the run goes on in the callback's thread when
 * the callback comes later, in place when it came already. A resumption after a wait is a run of its own, at the
 * top: its types are the first run's */
object Async:
  /** the top's level, its answer `Unit`; the context under the failure's handler there; under `Async` there */
  type Top = Level[Pure, Unit, Unit]
  type InFails = Level[Fails, Top, Unit]
  /** the context a body run by `runAsync` is written in */
  type Body = Fx[okay.std.Async +: Fails, InFails, Unit]

  private enum Slot[+A]:
    case Waiting
    case Ready(r: Either[Throwable, A])
    case Detached

  /** `runAsync(body)(done)`: the body run, `done` given its value or its failure, once */
  def runAsync[A](body: Body ?=> Eff[okay.std.Async +: Fails, InFails, Unit, Unit, A])(done: Either[Throwable, A] => Unit): Unit =
    // the run up to its end or to a wait, a failure raised in it `done`'s, the rest of the run aborted
    def go(p: Eff[Fails, Top, Unit, Unit, Unit]): Unit =
      Fx.run[Unit](Effects.handle(new okay.core.Handler[[X] =>> Eff[Pure, Unit, Unit, Unit, X], Throws % Throwable, Unit, Unit]:
        def ret(u: Unit): Unit = ()
        def apply[X](op: Throws[Throwable, X], k: X => Eff[Pure, Unit, Unit, Unit, Unit]): Eff[Pure, Unit, Unit, Unit, Unit] = op match
          case Throws.Raise(e) => done(Left(e)); okay.zero.pure(()))(p))
    def handled(using c: Fx[Fails, Top, Unit]): Eff[Fails, Top, Unit, Unit, Unit] =
      Effects.handle(new okay.core.Handler[c.P, okay.std.Async, A, Unit]:
        def ret(a: A): Unit = done(Right(a))
        def apply[X](op: okay.std.Async[X], k: X => c.P[Unit]): c.P[Unit] =
          def resume(r: Either[Throwable, X]): c.P[Unit] = r.fold(raise[Throwable, Unit], k)
          op match
            case Run(run) => resume(try Right(run()) catch { case NonFatal(e) => Left(e) })
            case Await(register) =>
              val slot = AtomicReference[Slot[X]](Slot.Waiting)
              register: r =>
                if !slot.compareAndSet(Slot.Waiting, Slot.Ready(r)) then go(resume(r))
              if slot.compareAndSet(Slot.Waiting, Slot.Detached) then c.pure(())
              else slot.get match
                case Slot.Ready(r) => resume(r)
                case _ => c.pure(()))(body)
    go(handled(using Fx()))

  /** `run(body)`: the body's value, this thread waiting for it — at the edge of a program only; its failure raised in
   * the context */
  def run[A](body: Body ?=> Eff[okay.std.Async +: Fails, InFails, Unit, Unit, A])(using c: Effects, has: Throws % Throwable :? c.R): c.P[A] =
    c.delay:
      val result = java.util.concurrent.CompletableFuture[Either[Throwable, A]]()
      runAsync(body)(result.complete)
      result.get().fold(raise[Throwable, A], c.pure)
