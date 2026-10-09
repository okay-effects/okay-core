package okay.core

/** DELIMITED CONTROL over the monad: `shift` and `reset`, answers moved — Danvy–Filinski's, typed as Asai–Kameyama's.
 * The raw operations at the context's row; `Control.shift` and `Control.reset` are them in context */
trait Control extends Context:
  /** the context in a reset here, its body written at `S2`: the monad `Inner` */
  type Reset[S2] <: Control { type M[I, O, A] = Control.this.Inner[Control.this.Effs, I, O, A]; type Effs = Control.this.Effs; type S = S2 }
  /** this context at the answer `S2`, the monad the same: a shift's body, where the delimiter was */
  type Answer[S2] <: Control { type M[I, O, A] = Control.this.M[I, O, A]; type Effs = Control.this.Effs; type S = S2 }
  def answer[S2]: Answer[S2]
  /** `shift(k => body)`: `k` the context up to the nearest delimiter, under a delimiter of its own, delivering the
   * answer at the hole, `I`; the body runs under the delimiter at its final answer `O`, its value that answer.
   * `same`, if the shift keeps the answer: what a monad whose `k` holds a handler's clauses needs */
  def shift[I, O, A](f: (A => M[O, O, I]) => M[O, O, O], same: (I =:= O) | Null): M[I, O, A]
  /** the delimiter: the body, in its context, written at `S2`, its value the answer; its final answer `O` the
   * delimiter's value here */
  def reset[S2, O](body: Reset[S2] ?=> Inner[Effs, S2, O, S2]): M[S, S, O]

/** whether a shift keeps the answer, `T` to `O`: the proof, if the types say so; else nothing known */
opaque type Keeps[T, O] = (T =:= O) | Null
object Keeps extends KeepsLow:
  given kept[T]: Keeps[T, T] = keptProof
  extension [T, O](k: Keeps[T, O]) def proof: (T =:= O) | Null = k
private def keptProof[T]: Keeps[T, T] = <:<.refl
private def unknownProof[T, O]: Keeps[T, O] = null
trait KeepsLow:
  given unknown[T, O]: Keeps[T, O] = unknownProof

object Control:
  /** `reset[S](body)`: the body written at the answer `S`, its value; the delimiter answers what the body moved
   * the answer to, `O` */
  def reset[S](using c: Control)[O](body: c.Reset[S] ?=> c.Inner[c.Effs, S, O, S]): c.Prog[O] =
    c.reset(body)

  /** `shift[A, O](k => body)`: the hole's value `A` and the answer the delimiter is moved to, `O`; the hole's
   * answer is the context's. The body is written at `O`, under the delimiter */
  def shift[A, O](using c: Control)(f: c.Answer[O] ?=> (A => c.M[O, O, c.S]) => c.M[O, O, O])
      (using k: Keeps[c.S, O]): c.M[c.S, O, A] =
    c.shift(f(using c.answer), k.proof)
