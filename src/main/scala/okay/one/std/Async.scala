package okay.one.std

import java.util.concurrent.atomic.AtomicReference
import okay.one.*
import okay.one.Cont0.perform
import okay.one.row.*
import okay.one.step.Step
import okay.one.step.Step.*

/** ASYNC: a value given later, by a callback — its run going on where the callback is called. Not handled in the
 * program: the driver outside's, the program stepped (`run`) */
enum Async[X]:
  case Await[X](register: (Either[Throwable, X] => Unit) => Unit) extends Async[X]

object Async:
  def await[X, R <: Row](register: (Either[Throwable, X] => Unit) => Unit)(using In[Async, R]): X ! R =
    perform[Async, R, X](Await(register))

  /** `p` run, `done` given its value or the error a callback gave: each `await` registered, the run going on in its
   * callback — at once, in the loop, if it is called in the registering; on its thread if later */
  def run[A](p: A ! Async +: Pure)(done: Either[Throwable, A] => Unit): Unit = drive(p.step, done)

  private def drive[A](s0: Step[Async +: Pure, A], done: Either[Throwable, A] => Unit): Unit =
    var s: Step[Async +: Pure, A] | Null = s0
    while s != null do
      s = s match
        case Step.Done(a) =>
          done(Right(a))
          null
        case Step.Failed(e) =>
          done(Left(e))
          null
        case y: Step.Yield[r, a, e, x] => y.in match
          case _: In.Here[f, t] => awaited(y.op, y.resume, done)
          case th: In.There[f, g, t] => In.absurd(th.in)

  /** the step after `op`'s value if its callback came while registering; else `null`, the callback driving on */
  private def awaited[X, A](op: Async[X], resume: Step.Resume[X, Async +: Pure, A], done: Either[Throwable, A] => Unit)
      : Step[Async +: Pure, A] | Null = op match
    case Await(register) =>
      val got = AtomicReference[Slot[X]](Slot.Registering)
      register: x =>
        if !got.compareAndSet(Slot.Registering, Slot.Got(x)) then x match
          case Right(v) => drive(resume(v), done)
          case Left(e) => drive(resume.fail(e), done)
      if got.compareAndSet(Slot.Registering, Slot.Late) then null
      else got.get() match
        case Slot.Got(Right(v)) => resume(v)
        case Slot.Got(Left(e)) => resume.fail(e)
        case _ => null

  /** a callback's value: not yet, the registering going on; given in it; or to come later, the registering over */
  private enum Slot[+X]:
    case Registering
    case Late
    case Got(x: Either[Throwable, X])
