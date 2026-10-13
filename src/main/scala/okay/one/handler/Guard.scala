package okay.one.handler

import okay.one.{!, Cont0}
import okay.one.row.*

/** A GUARD: a level with no effect of its own around a program of `A` over `T`, answering `B` — its `ret`, a
 * `finalizer` run when the level is left by an error, an abort or a cancel (and, for `ensuring`, when it ends), a
 * `recover` given an error thrown in it (not an operation's raise, not an abort or a cancel), `masked` keeping a cancel
 * for when the level is left. Taken into a `k`, put back each time it is resumed: its finalizer once a resumption */
abstract class Guard[T <: Row, A, B]:
  def ret(a: A): B ! T
  def finalizer: (Unit ! T) | Null = null
  def recover(e: Throwable): (B ! T) | Null = null
  def masked: Boolean = false

object Guard:
  /** `fin` after the program, whatever way it is left */
  def ensuring[T <: Row, A](fin: Unit ! T): Guard[T, A, A] = new Guard[T, A, A]:
    def ret(a: A) = fin.map(_ => a)
    override def finalizer = fin

  /** an error thrown in the program given to `f`, its program in the program's place */
  def recover[T <: Row, A](f: Throwable => A ! T): Guard[T, A, A] = new Guard[T, A, A]:
    def ret(a: A) = Cont0.Return(a)
    override def recover(e: Throwable) = f(e)

  /** a cancel kept for when the program is left */
  def uncancelable[T <: Row, A]: Guard[T, A, A] = new Guard[T, A, A]:
    def ret(a: A) = Cont0.Return(a)
    override def masked = true
