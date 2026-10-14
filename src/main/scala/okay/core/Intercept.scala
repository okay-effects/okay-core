package okay.core

import okay.one.Cont
import okay.one.step.Step
import okay.one.step.Step.step

/** AN EFFECT ANYWHERE IN A ROW HANDLED AGAIN, the row kept — what code over `F[_]` needs (tofu's `local`,
 * `handleWith`): `fa: Eff[R, A]` with its operations of `E`, at `in`, given to `clause`, each with the rest as `k`;
 * every other operation on outside as it is. By the kernel's `Step`: the program is stepped and each operation
 * come to its top is either `E`'s — the two paths the same, which proves the operation `E`'s, no cast — or
 * performed again outside, the rest resumed with its value; a `k` is the rest stepped on, data, resumable any
 * number of times. The slow road — a run of the machine an operation — until the kernel has a relocation of its own
 * for it (a `View` that keeps the row); `intercept` at the head of a row is the fast one */
object Intercept:
  /** two paths in one row, the same one: then their effects are the same — by the compiler's own reading of the
   * two paths, no cast. A walk as long as the paths, the row's length at most */
  @scala.annotation.tailrec
  def same[E1[_], E2[_], R <: Row](a: In[E1, R], b: In[E2, R]): Option[Same[E1, E2]] = a match
    case _: In.Here[e1, t1] => b match
      case _: In.Here[e2, t2] => Some(Same.Refl())
      case _ => None
    case t: In.There[e1, x1, t1] => b match
      case u: In.There[e2, x2, t2] => same(t.in, u.in)
      case _ => None

  /** the effects `E1` and `E2`, the same: an operation of one an operation of the other */
  enum Same[E1[_], E2[_]]:
    case Refl[E[_]]() extends Same[E, E]
    def apply[X](op: E1[X]): E2[X] = this match
      case Refl() => op

  /** a clause as a class: what Scala 2 writes (a polymorphic function value it cannot) */
  trait Clause[E[_], R <: Row, A]:
    def apply[X](op: E[X], k: X => Eff[R, A]): Eff[R, A]

  /** `fa`'s operations of `E` at `in` to `clause`, with the rest as `k`; the others performed again outside */
  def at[E[_], R <: Row, A](in: In[E, R])(fa: Eff[R, A])(clause: [X] => (E[X], X => Eff[R, A]) => Eff[R, A]): Eff[R, A] =
    by(in)(fa)(new Clause[E, R, A] { def apply[X](op: E[X], k: X => Eff[R, A]): Eff[R, A] = clause[X](op, k) })

  /** the same, the clause a `Clause` */
  def by[E[_], R <: Row, A](in: In[E, R])(fa: Eff[R, A])(clause: Clause[E, R, A]): Eff[R, A] =
    def go(s: Step[R, A]): Eff[R, A] = s match
      case Step.Done(a) => pure(a)
      case Step.Failed(e) => defer(throw e)
      case y: Step.Yield[r, a, f, x] =>
        val rest: x => Eff[R, A] = v => defer(go(y.resume(v)))
        same(y.in, in) match
          case Some(proof) => clause[x](proof(y.op), rest)
          case None => Cont.perform[f, R, x](y.op)(using y.in).flatMap(rest)
    defer(go(fa.step))
