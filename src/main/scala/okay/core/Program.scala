package okay.core

/** A PROGRAM OVER THE ROW `R`, NOT YET BUILT: built in any context whose row is `R` — `on(c)`, a program there. So
 * it is built again where another context needs it — under an interceptor (`local`, `handleWith`) — and its type
 * names its row: `F[A] = Program[R, A]`, the carrier of code written over `F[_]` (cats, tofu), of a program in
 * okay's shape (`A ! R`, the row in the type), and readable from Scala 2. Built where it runs, so it runs on any
 * monad: `run(top)(p)` */
abstract class Program[R <: Row, A]:
  /** this program built in the context `c` */
  def on(c: Effects { type Effs = R }): c.Prog[A]

  /** built at once: a value, an operation — one node, nothing under it to build */
  protected def leaf: Boolean = false
  /** the left side of a bind built: a leaf at once; anything else when the loop gets to it */
  private[core] def left(c: Effects { type Effs = R }): c.Prog[A] = if leaf then on(c) else c.delay(on(c))

  /** a bind; one on a bind reassociated as it is made — `(p >>= f) >>= g` is `p >>= (x => f(x) >>= g)` — so the
   * left side of a bind is a leaf, built at once, no delay between. Up to `Program.Fused` binds deep: the functions
   * composed so call each other, a frame each; past it a bind on the bind, its left side built when the loop gets to
   * it — a chain of any depth built in constant stack */
  def flatMap[B](f: A => Program[R, B]): Program[R, B] = this match
    case b: Program.Bind[R, x, A] if b.depth < Program.Fused => Program.Bind[R, x, B](b.p, y => b.f(y).flatMap(f), b.depth + 1)
    case _ => Program.Bind(this, f, 0)
  def map[B](f: A => B): Program[R, B] = this match
    case b: Program.Bind[R, x, A] if b.depth < Program.Fused => Program.Bind[R, x, B](b.p, y => b.f(y).map(f), b.depth + 1)
    case _ =>
      val p = this
      new Program[R, B]:
        def on(c: Effects { type Effs = R }): c.Prog[B] = p.left(c).map(f)

object Program:
  /** how many binds reassociated into one, at most: the frames a call of the composed function takes */
  private[core] final val Fused = 64
  /** `p >>= f`, `f` composed of `depth` binds reassociated */
  private[core] final class Bind[R <: Row, X, A](val p: Program[R, X], val f: X => Program[R, A], val depth: Int) extends Program[R, A]:
    def on(c: Effects { type Effs = R }): c.Prog[A] = p.left(c).flatMap(x => f(x).on(c))
  /** the program constructor at the row `R`: `Program.Of[R]` is an `F[_]` */
  type Of[R <: Row] = [A] =>> Program[R, A]

  def pure[R <: Row, A](a: A): Program[R, A] = new Program[R, A]:
    override protected def leaf: Boolean = true
    def on(c: Effects { type Effs = R }): c.Prog[A] = c.pure(a)
  /** `defer(p)`: the program `p`, made when it is built — a tail call */
  def defer[R <: Row, A](p: => Program[R, A]): Program[R, A] = new Program[R, A]:
    def on(c: Effects { type Effs = R }): c.Prog[A] = c.delay(p.on(c))
  /** an operation of `E`, at its path in the row */
  def perform[E[_], R <: Row, X](op: E[X])(using in: In[E, R]): Program[R, X] = new Program[R, X]:
    override protected def leaf: Boolean = true
    def on(c: Effects { type Effs = R }): c.Prog[X] = c.perform(op, in)
  /** a program written in context, at the row `R` */
  def apply[R <: Row, A](p: (c: Effects { type Effs = R }) => c.Prog[A]): Program[R, A] = new Program[R, A]:
    def on(c: Effects { type Effs = R }): c.Prog[A] = p(c)

  /** `run(top)(p)`: a program with nothing left to handle, built at the top of a monad and run there */
  def run[A](top: Top)(p: Program[Pure, A]): A = top.run(p.on(top.at[A]))
