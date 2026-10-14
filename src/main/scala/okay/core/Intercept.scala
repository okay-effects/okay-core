package okay.core

import okay.one.handler.Deep
import okay.one.row.Removed

/** AN EFFECT ANYWHERE IN A ROW HANDLED AGAIN, the row kept — what code over `F[_]` needs (tofu's `local`,
 * `handleWith`): `fa: Eff[R, A]` with its operations of `E`, at `in`, given to `clause`, each with the rest as `k`;
 * every other operation on outside as it is; the clauses' own operations of `E` to the handler of `E` outside. The
 * kernel's own: a `Deep` handler over `R` with `E` taken from its place and left there (`Removed.again(in)`) — the
 * body seen with `E` first, each of its places relocated by the path, no cast, no step. `intercept` at the head of
 * a row is the same with `E` first already */
object Intercept:
  /** a clause as a class: what Scala 2 writes (a polymorphic function value it cannot) */
  trait Clause[E[_], R <: Row, A]:
    def apply[X](op: E[X], k: X => Eff[R, A]): Eff[R, A]

  /** `fa`'s operations of `E` at `in` to `clause`, with the rest as `k`; the others performed outside as they are */
  def at[E[_], R <: Row, A](in: In[E, R])(fa: Eff[R, A])(clause: [X] => (E[X], X => Eff[R, A]) => Eff[R, A]): Eff[R, A] =
    by(in)(fa)(new Clause[E, R, A] { def apply[X](op: E[X], k: X => Eff[R, A]): Eff[R, A] = clause[X](op, k) })

  /** the same, the clause a `Clause` */
  def by[E[_], R <: Row, A](in: In[E, R])(fa: Eff[R, A])(clause: Clause[E, R, A]): Eff[R, A] =
    fa.handle(new Deep[E, R, A, A]:
      def ret(a: A): Eff[R, A] = pure(a)
      def apply[X](op: E[X], k: X => Eff[R, A]): Eff[R, A] = clause(op, k))(using Removed.again(in))
