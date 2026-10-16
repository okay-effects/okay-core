package okay.one.handler

import okay.one.{!, Cont}
import okay.one.row.*

/** A GUARD: a level with no effect of its own around a program of `A` over `T`, answering `B` — its `ret`, a
 * `finalizer` run when the level is left by an error, an abort or a cancel (and, for `ensuring`, when it ends), a
 * `masked` keeping a cancel for when the level is left; `Recovering`, a `recover` given an error thrown in it (not an
 * operation's raise, not an abort or a cancel). At any answers: a shift0 passes it, its `k` holding it — taken into a
 * `k`, put back each time it is resumed, its finalizer once a resumption */
abstract class Guard[T <: Row, A, B]:
  def ret(a: A): B ! T
  def finalizer: (Unit ! T) | Null = null
  def masked: Boolean = false
  /** AN OPERATION ON ITS WAY OUT past the guard, to a handler taking the rest or to the top, the guard told of it:
   * what goes out in its place, and what is run each time the rest is resumed (`Guard.Out`) — `null`, the operation
   * as it is, nothing run. For a guard over what the branches of the rest SHARE (a resource kept for every branch of
   * a choice, a count taken at each resumption; released before an operation that never resumes). Not told: an
   * operation answered in place (the rest goes on at once, the guard where it was), an abort (its finalizer runs).
   * Not written `| Null`, as `Handler.at` (Scala 2 reads the class): `null` all the same */
  def passing[E[_], X](op: E[X]): Guard.Out[E, X] = null

/** a guard RECOVERING: an error thrown in the program given to `recover`, its program in the program's place — a
 * program of effects, so the program is one: a recovering guard sits on `!` only (`guard` says so) */
abstract class Recovering[T <: Row, A, B] extends Guard[T, A, B]:
  def recover(e: Throwable): (B ! T) | Null

object Guard:
  private object Attempt extends Recovering[Pure, Any, Either[Throwable, Any]]:
    def ret(a: Any) = Cont.Return(Right(a))
    def recover(e: Throwable) = Cont.Return(Left(e))
  /** a program ATTEMPTED: its value `Right`, its error `Left` — one, it holds nothing */
  def attempt[T <: Row, A]: Recovering[T, A, Either[Throwable, A]] =
    Attempt.asInstanceOf[Recovering[T, A, Either[Throwable, A]]]
  /** the evidence a program of effects has no answers */
  val noAnswers: Nothing =:= Nothing = summon[Nothing =:= Nothing]

  /** what a guard sends out in an operation's place (`op`, of the same effect at the same place), and runs each time
   * the rest is resumed, with a value or an error (`resumed`) */
  final class Out[E[_], X](val op: E[X], val resumed: () => Unit)

  /** `fin` after the program, whatever way it is left */
  def ensuring[T <: Row, A](fin: Unit ! T): Guard[T, A, A] = new Guard[T, A, A]:
    def ret(a: A) = fin.map(_ => a)
    override def finalizer = fin

  /** an error thrown in the program given to `f`, its program in the program's place */
  def recover[T <: Row, A](f: Throwable => A ! T): Recovering[T, A, A] = new Recovering[T, A, A]:
    def ret(a: A) = Cont.Return(a)
    def recover(e: Throwable) = f(e)

  /** a cancel kept for when the program is left */
  def uncancelable[T <: Row, A]: Guard[T, A, A] = new Guard[T, A, A]:
    def ret(a: A) = Cont.Return(a)
    override def masked = true
