package okay.zero

import Cont0.*

/** a ROW: the effects of a context, nominal — `Ask +: Say +: Empty` */
sealed trait Row
sealed trait +:[E[_], T <: Row] extends Row
sealed trait Empty extends Row

/** the effect `E` in the row `R`, a path to it: the compiler builds it, the first by priority */
enum In[E[_], R <: Row]:
  case Here[E[_], T <: Row]() extends In[E, E +: T]
  case There[E[_], E2[_], T <: Row](in: In[E, T]) extends In[E, E2 +: T]
infix type :?[E[_], R <: Row] = In[E, R]
object In extends InLow:
  given here[E[_], T <: Row]: In[E, E +: T] = Here()
sealed trait InLow:
  given there[E[_], E2[_], T <: Row](using in: In[E, T]): In[E, E2 +: T] = In.There(in)

/** WHAT A HANDLER'S BODY ANSWERS IT: its value, or an operation of its row — at its path — with the rest, `k`, a
 * program of the handler's own level. An operation is a `shift0` up to the handler's delimiter whose body is this
 * request, answered by the handler, not built by the operation: so the handler is the one the program RUNS under */
enum Res[R <: Row, J, A]:
  case Done[R <: Row, J, A](a: A) extends Res[R, J, A]
  case Req[R <: Row, J, A, E[_], X](op: E[X], in: In[E, R], k: X => Cont0[J, J, Res[R, J, A]]) extends Res[R, J, A]

/** THE CONTEXT a program is written in: its row `R`, its answer `S` — the top's, or a program outside the nearest
 * handler, answering it a `Res`. A value: an operation needs one under a handler */
enum Ctx[R <: Row, S]:
  case Top[S]() extends Ctx[Empty, S]
  case Under[E[_], T <: Row, J, A](outer: Ctx[T, J]) extends Ctx[E +: T, Cont0[J, J, Res[E +: T, J, A]]]

/** the answer inside a handler of `E` answering its body's `A`, in a context over `R` at `S`: a program outside,
 * its value the body's answer to the handler */
type Inner[E[_], R <: Row, S, A] = Cont0[S, S, Res[E +: R, S, A]]

/** a handler of `E` over programs `P` outside it: the body's value to `Ans`, each operation with the rest — deep */
trait Handler[P[_], E[_], A, Ans]:
  def ret(a: A): Ans
  def apply[X](op: E[X], k: X => P[Ans]): P[Ans]

object Effects:
  /** an operation of `E` at its path: a `shift0` to the nearest delimiter, its body the request. The path first:
   * past it the row has an effect, so the context is under a handler */
  def perform[E[_], R <: Row, S, X](op: E[X], in: In[E, R], c: Ctx[R, S]): Cont0[S, S, X] = in match
    case In.Here() => c match
      case u: Ctx.Under[e, t, j, a] => shift0[j, j, Res[e +: t, j, a], S, X](k => pure[j, Res[e +: t, j, a]](Res.Req[e +: t, j, a, E, X](op, in, k)))
    case In.There(_) => c match
      case u: Ctx.Under[e, t, j, a] => shift0[j, j, Res[e +: t, j, a], S, X](k => pure[j, Res[e +: t, j, a]](Res.Req[e +: t, j, a, E, X](op, in, k)))

  /** the answers of a handler's body, interpreted: a value by `ret`; its operation by the clause, `k` the rest
   * interpreted again — deep; another's asked again outside, its path one shorter, the rest interpreted again */
  def interpret[E[_], R <: Row, S, A, Ans](out: Cont0[S, S, Res[E +: R, S, A]], h: Handler[[X] =>> Cont0[S, S, X], E, A, Ans],
                                           c: Ctx[R, S]): Cont0[S, S, Ans] =
    out.flatMap:
      case Res.Done(a) => pure(h.ret(a))
      case r: Res.Req[r, j, a, e, x] => r.in match
        case In.Here() => h(r.op, x => interpret(r.k(x), h, c))
        case In.There(o) => perform[e, R, S, x](r.op, o, c).flatMap(x => interpret(r.k(x), h, c))

/** `perform(op)`: an operation of `E`, in the context — its row has `E` */
def perform[E[_], R <: Row, S, X](op: E[X])(using c: Ctx[R, S], in: In[E, R]): Cont0[S, S, X] = Effects.perform(op, in, c)

/** `handle(h)(body)`: `Done $ body`, the body in the handler's context, its answers interpreted by `h` */
def handle[E[_], R <: Row, S, A, Ans](using c: Ctx[R, S])(h: Handler[[X] =>> Cont0[S, S, X], E, A, Ans])
    (body: Ctx[E +: R, Inner[E, R, S, A]] ?=> Cont0[Inner[E, R, S, A], Inner[E, R, S, A], A]): Cont0[S, S, Ans] =
  Effects.interpret(dollar[Inner[E, R, S, A], S, S, Res[E +: R, S, A], A](a => pure(Res.Done(a)))(body(using Ctx.Under(c))), h, c)

/** `run(body)`: the body at the top, no handler, its answer its value */
def run[A](body: Ctx[Empty, A] ?=> Cont0[A, A, A]): A = body(using Ctx.Top()).value
