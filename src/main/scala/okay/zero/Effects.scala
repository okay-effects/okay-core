package okay.zero

/** EFFECTS: operations by name in a row, deep handlers, on the delimiters' protocol, in context. An operation is a
 * request to the nearest delimiter; a handler is a delimiter that answers its own and passes the others on
 * outside, one handler shorter — so the handler is the one the program RUNS under, and a reset between passes it */

/** a handler of `E` over programs `P` outside it: the body's value to `Ans`, each operation with the rest — deep.
 * Master's clauses, `okay.core.Clauses`: a master's handler in a context of `Fx` is one as it is */
type Handler[P[_], E[_], A, Ans] = okay.core.Clauses[P, E, A, Ans]

/** a TAIL-RESUMPTIVE handler: each clause `k(value(op))`, the value of the operation alone — `okay.core.Answers` */
type Answering[P[_], E[_], A, Ans] = okay.core.Answers[P, E, A, Ans]

/** `perform(op)`: an operation of `E`, in the context — its row has `E` */
def perform[E[_], X](op: E[X])(using c: Ctx, in: In[E, c.R]): c.P[X] = request(op, in)

/** `performAt[T](op)`: an operation at the answer `T`, not the context's — where a shift has moved the answer (a
 * `PState` run): operations are answer-polymorphic */
def performAt[T]: PerformAt[T] = PerformAt()
final class PerformAt[T] private[zero] ():
  def apply[E[_], X](op: E[X])(using c: Ctx, in: In[E, c.R]): Eff[c.R, c.J, T, T, X] = request(op, in)

/** `handle(h)(body)`: `h.ret $ body`, the body in the handler's context, its answers read by the handler */
def handle[E[_], A, Ans](using c: Ctx)(h: Handler[c.P, E, A, Ans])(body: c.Body[E, Ans, A]): c.P[Ans] =
  handleNode(h, body(using At()))

/** THE CONTEXT WITH THE EFFECTS A PROGRAM NEEDS, `Es`, found in its row */
trait Needs[Es <: AnyKind] extends Ctx:
  def find[E[_]](r: Req[E, Es]): In[E, R]
object Needs:
  given all[Es <: AnyKind](using c: Ctx, p: Paths[Es, c.R]): (Needs[Es] { type R = c.R; type J = c.J; type T = c.T }) =
    Needy[c.R, c.J, c.T, Es](p)
  /** an effect needed: its path in the context's row */
  given found[E[_], Es <: AnyKind](using n: Needs[Es], r: Req[E, Es]): In[E, n.R] = n.find(r)

/** a context with the paths of the effects a program needs */
final class Needy[R0 <: Row, J0, T0, Es <: AnyKind] private[zero] (p: Paths[Es, R0]) extends At[R0, J0, T0] with Needs[Es]:
  def find[E[_]](r: Req[E, Es]): In[E, R0] = p.find(r)

/** `A ! Es`: a program of value `A`, in any context whose row has every effect of `Es` — one, `Int ! Reader % Int`,
 * or a row of them, `Int ! Reader % Int +: Writer % String +: Pure`; their order the program's own */
infix type ![A, Es <: AnyKind] = (c: Needs[Es]) ?=> c.P[A]
