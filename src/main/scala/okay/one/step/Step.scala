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

object Step:
  /** THE REST of a stepped program: resumed with the operation's value, or with an error raised where it was */
  final class Resume[X, R <: Row, A](resume: X => Step[R, A], failing: Throwable => Step[R, A]):
    def apply(x: X): Step[R, A] = resume(x)
    def fail(e: Throwable): Step[R, A] = failing(e)

  extension [R <: Row, I0, O0, A](p: Cont0[R, I0, O0, A])
    /** `p` run to its value or its first operation of `R` */
    def step: Step[R, A] = Machine.stepped(p)
