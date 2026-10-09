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
def async[A](a: => A)(using c: Effects, has: Async :? c.R): c.P[A] = perform[Async, A](Async.Run(() => a))
/** the value a callback is called with, once */
def await[A](register: (Either[Throwable, A] => Unit) => Unit)(using c: Effects, has: Async :? c.R): c.P[A] =
  perform[Async, A](Async.Await(register))

object Async:
  /** what is under `Async` at the top: the failure, `Throws % Throwable` — a `Left` at a callback, a throw in a
   * computation, or the program's own `raise` */
  type Fails = Throws % Throwable +: Pure
