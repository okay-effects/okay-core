package okay.zero

/** EFFECTS: operations by name in a row, deep handlers, on the delimiters' protocol. An operation is a request to
 * the nearest delimiter; a handler is a delimiter that answers its own and passes the others on outside, one
 * handler shorter — so the handler is the one the program RUNS under, and a reset in between passes it on */

/** a handler of `E` over programs `P` outside it: the body's value to `Ans`, each operation with the rest — deep */
trait Handler[P[_], E[_], A, Ans]:
  def ret(a: A): Ans
  def apply[X](op: E[X], k: X => P[Ans]): P[Ans]

/** a handler's body: in the handler's context, of value `A`, at the handler's answer `Ans` — as a shift under it,
 * no reset between, sees: `k` the rest with `ret`, the body's value the handler's answer */
type Body[E[_], R <: Row, J, T, A, Ans] = Ctx[E +: R, Level[R, J, T]] ?=> Eff[E +: R, Level[R, J, T], Ans, Ans, A]

/** `perform(op)`: an operation of `E`, in the context — its row has `E` — at any answer */
def perform[E[_], R <: Row, J, T, X](op: E[X])(using c: Ctx[R, J], in: In[E, R]): Eff[R, J, T, T, X] = request(op, in)

/** `handle(h)(body)`: `h.ret $ body`, the body in the handler's context, its answers read by the handler */
def handle[E[_], R <: Row, J, T, A, Ans](using c: Ctx[R, J])(h: Handler[[X] =>> Eff[R, J, T, T, X], E, A, Ans])
    (body: Body[E, R, J, T, A, Ans]): Eff[R, J, T, T, Ans] =
  read(delimit(body(using Ctx()), h.ret), new Requests[E +: R, R, J, T, Ans]:
    def apply[E2[_], X](op: E2[X], in: In[E2, E +: R], k: X => Eff[R, J, T, T, Res[E +: R, Level[R, J, T], Ans]],
                        again: Eff[R, J, T, T, Res[E +: R, Level[R, J, T], Ans]] => Eff[R, J, T, T, Ans]): Eff[R, J, T, T, Ans] =
      in match
        case In.Here() => h(op, x => again(k(x)))
        case In.There(o) => request[E2, R, J, T, X](op, o).flatMap(x => again(k(x))))
