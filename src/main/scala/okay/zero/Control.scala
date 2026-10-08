package okay.zero

/** CONTROL: `shift` and `reset`, the answers modified (Danvy–Filinski's, typed as Asai–Kameyama's), on the
 * delimiters' protocol. A reset is a delimiter that answers no operation: every one is passed on outside, the
 * rest resumed under it — so effects go through it. A shift reaches the nearest reset, a handler being no
 * shift's delimiter: so handlers keep theirs */

/** the context inside a `reset` in a context at `R`, `J`, at the answer `T` there: what a `shift` takes the types
 * of the outside from */
final class Reset[R <: Row, J, T] private[zero] () extends Ctx[R, Level[R, J, T]]

/** `reset(body)`: the body's value its initial answer, its final answer the reset's value */
def reset[R <: Row, J, T, O, A](using c: Ctx[R, J])(body: Reset[R, J, T] ?=> Eff[R, Level[R, J, T], A, O, A]): Eff[R, J, T, T, O] =
  passAll(delimit(body(using Reset())))

/** `resetAt[T](body)`: `reset`, the answer outside named — where no type expected says it, `resetAt[T](...).flatMap(...)` */
def resetAt[T]: ResetAt[T] = ResetAt()
final class ResetAt[T] private[zero] ():
  def apply[R <: Row, J, O, A](using c: Ctx[R, J])(body: Reset[R, J, T] ?=> Eff[R, Level[R, J, T], A, O, A]): Eff[R, J, T, T, O] =
    passAll(delimit(body(using Reset())))

/** `shift[A, I, O](k => body)` up to the nearest `reset`: `A` the hole's value, `I` its answer, `O` the reset's.
 * `k` the rest up to the reset, under it again — a program outside answering `I`, its operations passed on
 * outside; the body runs outside the reset, at its answer there, its value the reset's — in the context outside,
 * given it. Only in a reset's context, the rest of the types from it */
def shift[A, I, O]: Shift[A, I, O] = Shift()
final class Shift[A, I, O] private[zero] ():
  def apply[R <: Row, J, T](using r: Reset[R, J, T])(f: Ctx[R, J] ?=> (A => Eff[R, J, T, T, I]) => Eff[R, J, T, T, O])
      : Eff[R, Level[R, J, T], I, O, A] =
    capture[R, J, T, A, I, O](k => f(using Ctx())(a => passAll(k(a))).map(o => Res.Done(o)))

/** a reset's answers: a value its own; every operation asked again outside — its path the same, the row is — the
 * rest under the reset again */
private def passAll[R <: Row, J, T, O](out: Eff[R, J, T, T, Res[R, Level[R, J, T], O]]): Eff[R, J, T, T, O] =
  onAnswers(out, new OnAnswers[R, R, J, T, O, O]:
    def done(o: O): Eff[R, J, T, T, O] = pure(o)
    def request[E[_], X](op: E[X], in: In[E, R], k: X => Eff[R, J, T, T, Res[R, Level[R, J, T], O]]): Eff[R, J, T, T, O] =
      okay.zero.request[E, R, J, T, X](op, in).flatMap(x => passAll(k(x))))
