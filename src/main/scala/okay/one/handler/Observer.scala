package okay.one.handler

import okay.one.{!, Cont}
import okay.one.row.*

/** AN OBSERVER of every operation on its way out of a program, whatever its effect: its clause given the operation
 * seen (`Seen`) and the rest `k` — over the row `T` outside it: an operation told, passed on (`proceed`), its answer
 * told each time the rest gets it, or refused. A level with no effect of its own, as a guard is; taking the rest, so
 * around a program of effects only */
abstract class Observer[T <: Row, A, B]:
  def ret(a: A): B ! T
  def apply[E[_], X](op: Seen[E, X, T], k: X => B ! T): B ! T

/** AN OPERATION SEEN by an observer: what it is, and the way on — performed outside the observer, to its handler */
final class Seen[E[_], X, T <: Row] private[one] (val op: E[X], in: In[E, T]):
  /** its effect: the class of its operation's enum, `Pay` */
  def effect: String =
    // from the class's name, on every platform (no `getEnclosingClass` on Scala.js): `…TestIntercept$Pay$Transfer` →
    // `Pay`, `okay.one.std.Reader$Ask` → `Reader`, an enum's singleton case `Pay$$anon$1` → `Pay`
    val simple = op.getClass.getName.split('.').last
    val parts = simple.split('$').filter(p => p.nonEmpty && p != "anon" && !p.forall(_.isDigit))
    if parts.length >= 2 then parts(parts.length - 2) else parts.lastOption.getOrElse(simple)
  /** its place in the row outside the observer: two effects of one type told apart by it */
  def at: In[E, T] = in
  /** the operation as written: `Transfer(acct,6)` */
  def describe: String = op.toString
  /** passed on: its value the handler's further out */
  def proceed: X ! T = Cont.perform[E, T, X](op)(using in)
