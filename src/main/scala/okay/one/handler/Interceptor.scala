package okay.one.handler

import okay.one.{!, Cont}
import okay.one.row.*

/** AN INTERCEPTOR of the sensitive effect `S`: each operation of the body to its clause, with the rest `k` and the way
 * on (`pass`) — the clause over the row `T` outside the audit, `Audited[S]` in it: an operation told, passed on as it
 * is or changed, refused (not passed on), its answer changed before `k` gets it */
abstract class Interceptor[S[_], T <: Row, A, B]:
  def ret(a: A): B ! T
  def apply[X](op: S[X], k: X => B ! T, pass: Pass[S, T]): B ! T

/** THE WAY ON past the audit of `S`: an operation of `S` performed outside it, to its handler — an interceptor's alone,
 * so no other program performs `S` there */
final class Pass[S[_], T <: Row] private[one] (in: In[S, T]):
  def apply[X](op: S[X]): X ! T = Cont.perform[S, T, X](op)(using in)
