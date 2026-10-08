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

/** WHAT A DELIMITER'S BODY ANSWERS IT: its value, or an operation of its row — at its path — with the rest, `k`, a
 * program of the delimiter's own level. An operation is a `shift0` up to the nearest delimiter whose body is this
 * request: the delimiter answers it — a handler its own, a reset none, each passing the others on outside. So the
 * handler is the one the program RUNS under. Made and read here only */
sealed trait Res[R <: Row, J, A]
object Res:
  private[zero] final case class Done[R <: Row, J, A](a: A) extends Res[R, J, A]
  private[zero] final case class Req[R <: Row, J, A, E[_], X](op: E[X], in: In[E, R], k: X => Cont0[J, J, Res[R, J, A]])
    extends Res[R, J, A]

/** A LEVEL: the answer at a point of a body over `R`, its delimiter's outside at `J` — a program there, pure in its
 * answer, whose value is the body's answer to the delimiter, `T`, or an operation of the body's */
opaque type Level[R <: Row, J, T] = Cont0[J, J, Res[R, J, T]]

/** A PROGRAM WITH EFFECTS AND CONTROL over the row `R`, its nearest delimiter's outside at `J`: of value `A`, which,
 * with a continuation whose answer is `I`, answers `O` — the answers modified by `shift`. Written with `pure`,
 * `flatMap`, `map`, `perform`, `handle`, `reset` and `shift` only: no raw `shift0` or `$` reaches it, so every
 * operation reaches a handler of its effect — the nearest in force where it runs — and every `shift` its `reset` */
opaque type Eff[R <: Row, J, I, O, A] = Cont0[Level[R, J, I], Level[R, J, O], A]

/** a handler's body: in the handler's context, at its answer — its own value */
type Body[E[_], R <: Row, J, T, A] = Ctx[E +: R, Level[R, J, T]] ?=> Eff[E +: R, Level[R, J, T], A, A, A]

/** THE CONTEXT a program is written in: its row `R`, its nearest delimiter's outside `J`. A type, nothing at run
 * time: made by `run`, `handle` and `reset` only */
sealed class Ctx[R <: Row, J] private[zero] ()
/** the context inside a `reset` in a context at `R`, `J`, at the answer `T` there: what a `shift` takes the types
 * of the outside from */
final class Reset[R <: Row, J, T] private[zero] () extends Ctx[R, Level[R, J, T]]

extension [R <: Row, J, I, O, A](p: Eff[R, J, I, O, A])
  def flatMap[I2, B](f: A => Eff[R, J, I2, I, B]): Eff[R, J, I2, O, B] = Bind(p, f)
  def map[B](f: A => B): Eff[R, J, I, O, B] = Bind(p, a => Return(f(a)))

def pure[R <: Row, J, T, A](a: A): Eff[R, J, T, T, A] = Return(a)

/** a handler of `E` over programs `P` outside it: the body's value to `Ans`, each operation with the rest — deep */
trait Handler[P[_], E[_], A, Ans]:
  def ret(a: A): Ans
  def apply[X](op: E[X], k: X => P[Ans]): P[Ans]

/** `perform(op)`: an operation of `E`, in the context — its row has `E` — at any answer */
def perform[E[_], R <: Row, J, T, X](op: E[X])(using c: Ctx[R, J], in: In[E, R]): Eff[R, J, T, T, X] = request(op, in)

/** `handle(h)(body)`: `Done $ body`, the body in the handler's context, its answers interpreted by `h` */
def handle[E[_], R <: Row, J, T, A, Ans](using c: Ctx[R, J])(h: Handler[[X] =>> Eff[R, J, T, T, X], E, A, Ans])
    (body: Body[E, R, J, T, A]): Eff[R, J, T, T, Ans] =
  interpret(Dollar(body(using Ctx()), a => Return(Res.Done(a))), h)

/** `reset(body)`: the delimiter of `shift`, `Done $ body` — the body's value its initial answer, its final answer
 * the reset's value. No effect handled: every operation in it passed on outside, the rest resumed in it */
def reset[R <: Row, J, T, O, A](using c: Ctx[R, J])(body: Reset[R, J, T] ?=> Eff[R, Level[R, J, T], A, O, A]): Eff[R, J, T, T, O] =
  delimitReset(body(using Reset()))

/** `resetAt[T](body)`: `reset`, the answer outside named — where no type expected says it, `resetAt[T](...).flatMap(...)` */
def resetAt[T]: ResetAt[T] = ResetAt()

final class ResetAt[T] private[zero] ():
  def apply[R <: Row, J, O, A](using c: Ctx[R, J])(body: Reset[R, J, T] ?=> Eff[R, Level[R, J, T], A, O, A]): Eff[R, J, T, T, O] =
    delimitReset(body(using Reset()))

private def delimitReset[R <: Row, J, T, O, A](body: Eff[R, Level[R, J, T], A, O, A]): Eff[R, J, T, T, O] =
  forward(Dollar(body, (a: A) => Return(Res.Done[R, Level[R, J, T], A](a))))

/** `shift[A, I, O](k => body)` up to the nearest `reset`: `A` the hole's value, `I` its answer, `O` the reset's.
 * `k` the rest up to the reset, under it again — a program outside answering `I`, its operations passed on outside;
 * the body runs outside the reset, at its answer there, its value the reset's — in the context outside, given it.
 * Only in a reset's context — the rest of the types from it: a handler is not a `shift`'s delimiter */
def shift[A, I, O]: Shift[A, I, O] = Shift()

final class Shift[A, I, O] private[zero] ():
  def apply[R <: Row, J, T](using r: Reset[R, J, T])(f: Ctx[R, J] ?=> (A => Eff[R, J, T, T, I]) => Eff[R, J, T, T, O])
      : Eff[R, Level[R, J, T], I, O, A] = shiftTo(f(using Ctx()))

/** shift0 to the reset, the body outside it: `k` the rest forwarded, the body's value the reset's */
private def shiftTo[R <: Row, J, T, A, I, O](f: (A => Eff[R, J, T, T, I]) => Eff[R, J, T, T, O]): Eff[R, Level[R, J, T], I, O, A] =
  Shift0[Level[R, J, T], Level[R, J, T], Res[R, Level[R, J, T], I], Level[R, Level[R, J, T], O], A](k =>
    Bind(f(a => forward(k(a))), (o: O) => Return(Res.Done[R, Level[R, J, T], O](o))))

/** `run(body)`: the body at the top under a delimiter of its own, no handler — nothing to ask over `Empty` — its
 * answer its value; the top outside it `identity $`, as the kernel's */
def run[A](body: Ctx[Empty, A] ?=> Eff[Empty, A, A, A, A]): A =
  Bind(Dollar(body(using Ctx()), (a: A) => Return(Res.Done[Empty, A, A](a))), (res: Res[Empty, A, A]) => res match
    case Res.Done(a) => Return[A, A](a)).value

/** an operation of `E` at its path: a `shift0` to the nearest delimiter, its body the request */
private def request[E[_], R <: Row, J, T, X](op: E[X], in: In[E, R]): Eff[R, J, T, T, X] =
  Shift0[J, J, Res[R, J, T], Level[R, J, T], X](k => Return(Res.Req[R, J, T, E, X](op, in, k)))

/** a handler's answers interpreted: a value by `ret`; its operation by the clause, `k` the rest interpreted again —
 * deep; another's asked again outside, its path one shorter, the rest interpreted again */
private def interpret[E[_], R <: Row, J, T, A, Ans](out: Cont0[Level[R, J, T], Level[R, J, T], Res[E +: R, Level[R, J, T], A]],
    h: Handler[[X] =>> Eff[R, J, T, T, X], E, A, Ans]): Eff[R, J, T, T, Ans] =
  Bind(out, (res: Res[E +: R, Level[R, J, T], A]) => res match
    case Res.Done(a) => Return(h.ret(a))
    case r: Res.Req[r, j, a, e, x] => r.in match
      case In.Here() => h(r.op, x => interpret(r.k(x), h))
      case In.There(o) => Bind(request[e, R, J, T, x](r.op, o), (x: x) => interpret(r.k(x), h)))

/** a reset's answers: a value its own; every operation asked again outside, its path the same — the row is — the
 * rest resumed under the reset */
private def forward[R <: Row, J, T, O](out: Cont0[Level[R, J, T], Level[R, J, T], Res[R, Level[R, J, T], O]]): Eff[R, J, T, T, O] =
  Bind(out, (res: Res[R, Level[R, J, T], O]) => res match
    case Res.Done(o) => Return(o)
    case r: Res.Req[r, j, a, e, x] => Bind(request[e, R, J, T, x](r.op, r.in), (x: x) => forward(r.k(x))))
