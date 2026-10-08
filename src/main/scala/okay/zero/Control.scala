package okay.zero

/** CONTROL: `shift` and `reset`, the answers modified (Danvy–Filinski's, typed as Asai–Kameyama's), and `shift0`, on
 * the delimiters' protocol. A reset is a delimiter that answers no operation: every one is passed on outside, the
 * rest resumed under it — so effects go through it. A `shift` is a request to the nearest delimiter — a reset, a
 * handler, the top — each taking it alike: its body under that delimiter again */

/** the context inside a `reset` in a context at `R`, `J`, at the answer `T` there: what a `shift0` takes the types
 * of the outside from */
final class Reset[R <: Row, J, T] private[zero] () extends Ctx[R, Level[R, J, T]]

/** `reset(body)`: the body's value its initial answer, its final answer the reset's value */
def reset[R <: Row, J, T, O, A](using c: Ctx[R, J])(body: Reset[R, J, T] ?=> Eff[R, Level[R, J, T], A, O, A]): Eff[R, J, T, T, O] =
  passAll(delimit(body(using Reset()), identity))

/** `resetAt[T](body)`: `reset`, the answer outside named — where no type expected says it, `resetAt[T](...).flatMap(...)` */
def resetAt[T]: ResetAt[T] = ResetAt()
final class ResetAt[T] private[zero] ():
  def apply[R <: Row, J, O, A](using c: Ctx[R, J])(body: Reset[R, J, T] ?=> Eff[R, Level[R, J, T], A, O, A]): Eff[R, J, T, T, O] =
    passAll(delimit(body(using Reset()), identity))

/** `shift[A, I, O](k => body)` up to the nearest delimiter — a reset, a handler, the top: `A` the hole's value, `I`
 * its answer, `O` the delimiter's. `k` the rest up to it, under it again — a handler's `ret` and clauses with it;
 * the body runs under the delimiter, at its answer `O`, its value the delimiter's answer */
def shift[A, I, O]: Shift[A, I, O] = Shift()
final class Shift[A, I, O] private[zero] ():
  def apply[R <: Row, J](using c: Ctx[R, J])(f: (A => Eff[R, J, O, O, I]) => Eff[R, J, O, O, O]): Eff[R, J, I, O, A] = control(f)

/** `shift0[A, I, O](k => body)` up to the nearest `reset`: as `shift`, the body runs OUTSIDE the reset, in its place,
 * in the context outside, given it — a shift0 in it reaches the next reset out. `k` the rest under the reset again,
 * its operations passed on outside */
def shift0[A, I, O]: Shift0To[A, I, O] = Shift0To()
final class Shift0To[A, I, O] private[zero] ():
  def apply[R <: Row, J, T](using r: Reset[R, J, T])(f: Ctx[R, J] ?=> (A => Eff[R, J, T, T, I]) => Eff[R, J, T, T, O])
      : Eff[R, Level[R, J, T], I, O, A] =
    capture[R, J, T, A, I, O](k => f(using Ctx())(a => passAll(k(a))).map(o => Res.Done(o)))

/** a reset's reading: every operation asked again outside — its path the same, the row is — the rest under it again */
private def passAll[R <: Row, J, T, O](out: Eff[R, J, T, T, Res[R, Level[R, J, T], O]]): Eff[R, J, T, T, O] =
  read(out, new Requests[R, R, J, T, O]:
    def apply[E[_], X](op: E[X], in: In[E, R], k: X => Eff[R, J, T, T, Res[R, Level[R, J, T], O]],
                       again: Eff[R, J, T, T, Res[R, Level[R, J, T], O]] => Eff[R, J, T, T, O]): Eff[R, J, T, T, O] =
      request[E, R, J, T, X](op, in).flatMap(x => again(k(x))))
