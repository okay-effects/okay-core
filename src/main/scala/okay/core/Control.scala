package okay.core

/** DELIMITED CONTROL over the monad: `shift` and `reset`, answers moved — Danvy–Filinski's, typed as Asai–Kameyama's.
 * The raw operations take every type; `Control.shift` and `Control.reset` are them in context */
trait Control extends Context:
  /** the context at the row `R2`, the answer `S2`, the monad this one's */
  type Under[R2 <: Row, S2] <: Control { type M[X <: Row, I, O, A] = Control.this.M[X, I, O, A]; type R = R2; type S = S2 }
  def under[R2 <: Row, S2]: Under[R2, S2]
  /** `shift(k => body)`: `k` the context up to the nearest delimiter, under a delimiter of its own, delivering the
   * answer at the hole, `I`; the body runs under the delimiter at its final answer `O`, its value that answer */
  def shift[X <: Row, I, O, A](f: (A => M[X, O, O, I]) => M[X, O, O, O]): M[X, I, O, A]
  /** the delimiter: the body's value to its initial answer `S2` by `ret`, its final answer `O` the delimiter's
   * value outside, at any answer `Q` there */
  def reset[X <: Row, Q, S2, O, A](body: M[X, S2, O, A], ret: A => S2): M[X, Q, Q, O]

/** a context with effects and control both — named: the intersection `Effects & Control` of two classes with their
 * own bound on `M` makes the compiler fail (Scala 3.9.0, an assertion in `LazyRef`) */
trait Delimited extends Effects with Control

object Control:
  /** `reset[S](body)`: the body written at the answer `S`, its value; the delimiter answers what the body moved
   * the answer to, `O` */
  def reset[S](using c: Control)[O](body: c.Under[c.R, S] ?=> c.M[c.R, S, O, S]): c.M[c.R, c.S, c.S, O] =
    c.reset(body(using c.under), identity)

  /** `shift[A, O](k => body)`: the hole's value `A` and the answer the delimiter is moved to, `O`; the hole's
   * answer is the context's. The body is written at `O`, under the delimiter */
  def shift[A, O](using c: Control)(f: c.Under[c.R, O] ?=> (A => c.M[c.R, O, O, c.S]) => c.M[c.R, O, O, O]): c.M[c.R, c.S, O, A] =
    c.shift(f(using c.under))
