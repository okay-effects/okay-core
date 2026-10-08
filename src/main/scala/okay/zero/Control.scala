package okay.zero

/** CONTROL: `shift` and `reset`, the answers modified (Danvy–Filinski's, typed as Asai–Kameyama's), and `shift0`, on
 * the delimiters' protocol, in context. A reset is a delimiter that answers no operation: every one is passed on
 * outside, the rest resumed under it — so effects go through it. A `shift` is a request to the nearest delimiter —
 * a reset, a handler, the top — each taking it alike: its body under that delimiter again */

/** the context in a reset, in a context at `R0`, `J0`, `T0`, its body written at `S0`: the outside a `shift0` takes */
final class InReset[R0 <: Row, J0, T0, S0] private[zero] () extends At[R0, Level[R0, J0, T0], S0]

/** `reset[S](body)`: the body written at the answer `S`, its value; the reset answers what the body moved the answer
 * to, `O` */
def reset[S](using c: Ctx)[O](body: c.Reset[S] ?=> Eff[c.R, c.Inner, S, O, S]): c.P[O] =
  passAll(delimit(body(using InReset()), identity))

/** `shift[A, O](k => body)` up to the nearest delimiter — a reset, a handler, the top: `A` the hole's value, `O` the
 * answer the delimiter is moved to; the hole's answer is the context's. `k` the rest up to the delimiter, under it
 * again — a handler's `ret` and clauses with it; the body runs under the delimiter, at `O`, its value the answer */
def shift[A, O](using c: Ctx)(f: c.Answer[O] ?=> (A => Eff[c.R, c.J, O, O, c.T]) => Eff[c.R, c.J, O, O, O]): Eff[c.R, c.J, c.T, O, A] =
  control(f(using At()))

/** `shift0[A, O](k => body)` up to the nearest `reset`: as `shift`, but the body runs OUTSIDE the reset, in its place
 * — in the context outside, given it — so a shift0 in it reaches the next reset out. `k` the rest under the reset
 * again, its operations passed on */
def shift0[A, O]: Shift0To[A, O] = Shift0To()
final class Shift0To[A, O] private[zero] ():
  def apply[R <: Row, J, T, S](using r: InReset[R, J, T, S])(f: At[R, J, T] ?=> (A => Eff[R, J, T, T, S]) => Eff[R, J, T, T, O])
      : Eff[R, Level[R, J, T], S, O, A] =
    capture[R, J, T, A, S, O](k => f(using At())(a => passAll(k(a))).map(o => Res.Done(o)))

/** a reset's reading: every operation asked again outside — its path the same, the row is — the rest under it again */
private def passAll[R <: Row, J, T, O](out: Eff[R, J, T, T, Res[R, Level[R, J, T], O]]): Eff[R, J, T, T, O] =
  read(out, new Requests[R, R, J, T, O]:
    def apply[E[_], X](op: E[X], in: In[E, R], k: X => Eff[R, J, T, T, Res[R, Level[R, J, T], O]],
                       again: Eff[R, J, T, T, Res[R, Level[R, J, T], O]] => Eff[R, J, T, T, O]): Eff[R, J, T, T, O] =
      request[E, R, J, T, X](op, in).flatMap(x => again(k(x))))
