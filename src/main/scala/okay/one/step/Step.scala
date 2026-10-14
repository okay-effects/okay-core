package okay.one.step

import okay.one.*
import okay.one.machine.Machine
import okay.one.row.*

/** A PROGRAM STEPPED: run to its value, or to its first operation of the row `R` — given back with the rest of the
 * program, `k` as data, to resume with the operation's value; again, or never. What a driver outside is built on:
 * async, replay, a pull stream, a foreign monad */
enum Step[R <: Row, A]:
  case Done(a: A)
  case Yield[R <: Row, A, E[_], X](op: E[X], in: In[E, R], resume: Step.Resume[X, R, A]) extends Step[R, A]
  /** an error raised in the program — user code threw — its levels unwound */
  case Failed(e: Throwable)
  /** cancelled at a step: its levels unwound, their finalizers run, no recover asked */
  case Cancelled()

object Step:
  /** THE REST of a stepped program: resumed with the operation's value, or with an error raised where it was */
  final class Resume[X, R <: Row, A](resume: X => Step[R, A], failing: Throwable => Step[R, A],
      cancelling: () => Step[R, A]):
    def apply(x: X): Step[R, A] = resume(x)
    /** an error raised where the operation was: the levels of the rest unwound, their recovers asked */
    def fail(e: Throwable): Step[R, A] = failing(e)
    /** cancelled: the rest's finalizers run, `Cancelled`; under `uncancelable`, this step given back, the cancel
     * fired when the program leaves the mask */
    def cancel(): Step[R, A] = cancelling()

  extension [R <: Row, A](p: A ! R)
    /** `p` run to its value or its first operation of `R` */
    def step: Step[R, A] = Machine.stepped(p)
