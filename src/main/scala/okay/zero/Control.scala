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
  resetNode(body(using InReset()))

/** `shift[A, O](k => body)` up to the nearest delimiter — a reset, a handler, the top: `A` the hole's value, `O` the
 * answer the delimiter is moved to; the hole's answer is the context's. `k` the rest up to the delimiter, under it
 * again — a handler's `ret` and clauses with it; the body runs under the delimiter, at `O`, its value the answer */
def shift[A, O](using c: Ctx)(f: c.Answer[O] ?=> (A => Eff[c.R, c.J, O, O, c.T]) => Eff[c.R, c.J, O, O, O])
    (using same: Same[c.type, c.T, O]): Eff[c.R, c.J, c.T, O, A] =
  control(f(using At()), sameProof(same))

/** WHERE A SHIFT MAY MOVE THE ANSWER from `T` to `O`, in the context `C`: kept, anywhere — the proof of it; moved,
 * in a reset only. Up to a handler `k` is the rest under the handler again, its clauses at the handler's answer:
 * the rest must keep it */
opaque type Same[-C <: Ctx, T, O] = (T =:= O) | Null
private def sameKept[T]: Same[Ctx, T, T] = <:<.refl
private def sameMoved[C <: Ctx, T, O]: Same[C, T, O] = null
private[zero] def sameProof[C <: Ctx, T, O](s: Same[C, T, O]): (T =:= O) | Null = s
object Same extends MovedInReset:
  given kept[T]: Same[Ctx, T, T] = sameKept
private[zero] trait MovedInReset:
  given moved[R <: Row, J, T0, S, T, O]: Same[InReset[R, J, T0, S], T, O] = sameMoved

/** `shift0[A, O](k => body)` up to the nearest `reset`: as `shift`, but the body runs OUTSIDE the reset, in its place
 * — in the context outside, given it — so a shift0 in it reaches the next reset out. `k` the rest under the reset
 * again, its operations passed on */
def shift0[A, O]: Shift0To[A, O] = Shift0To()
final class Shift0To[A, O] private[zero] ():
  def apply[R <: Row, J, T, S](using r: InReset[R, J, T, S])(f: At[R, J, T] ?=> (A => Eff[R, J, T, T, S]) => Eff[R, J, T, T, O])
      : Eff[R, Level[R, J, T], S, O, A] =
    shift0Node[R, J, T, A, S, O](f(using At()))
