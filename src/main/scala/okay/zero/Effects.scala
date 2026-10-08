package okay.zero

/** EFFECTS: operations by name in a row, deep handlers, on the delimiters' protocol. An operation is a request to
 * the nearest delimiter; a handler is a delimiter that answers its own and passes the others on outside, one
 * handler shorter — so the handler is the one the program RUNS under, and a reset in between passes it on */

/** a handler of `E` over programs `P` outside it: the body's value to `Ans`, each operation with the rest — deep */
trait Handler[P[_], E[_], A, Ans]:
  def ret(a: A): Ans
  def apply[X](op: E[X], k: X => P[Ans]): P[Ans]

/** a handler's body: in the handler's context, at its answer — its own value */
type Body[E[_], R <: Row, J, T, A] = Ctx[E +: R, Level[R, J, T]] ?=> Eff[E +: R, Level[R, J, T], A, A, A]

/** `perform(op)`: an operation of `E`, in the context — its row has `E` — at any answer */
def perform[E[_], R <: Row, J, T, X](op: E[X])(using c: Ctx[R, J], in: In[E, R]): Eff[R, J, T, T, X] = request(op, in)

/** `handle(h)(body)`: the body in the handler's context, under its delimiter, its answers interpreted by `h` */
def handle[E[_], R <: Row, J, T, A, Ans](using c: Ctx[R, J])(h: Handler[[X] =>> Eff[R, J, T, T, X], E, A, Ans])
    (body: Body[E, R, J, T, A]): Eff[R, J, T, T, Ans] =
  interpret(delimit(body(using Ctx())), h)

/** a handler's answers: a value by `ret`; its operation by the clause, `k` the rest interpreted again — deep;
 * another's asked again outside, its path one shorter, the rest interpreted again */
private def interpret[E[_], R <: Row, J, T, A, Ans](out: Eff[R, J, T, T, Res[E +: R, Level[R, J, T], A]],
    h: Handler[[X] =>> Eff[R, J, T, T, X], E, A, Ans]): Eff[R, J, T, T, Ans] =
  onAnswers(out, new OnAnswers[E +: R, R, J, T, A, Ans]:
    def done(a: A): Eff[R, J, T, T, Ans] = pure(h.ret(a))
    def request[E2[_], X](op: E2[X], in: In[E2, E +: R], k: X => Eff[R, J, T, T, Res[E +: R, Level[R, J, T], A]]): Eff[R, J, T, T, Ans] =
      in match
        case In.Here() => h(op, x => interpret(k(x), h))
        case In.There(o) => okay.zero.request[E2, R, J, T, X](op, o).flatMap(x => interpret(k(x), h)))
