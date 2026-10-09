package okay.core

/** A PROGRAM OVER THE ROW `R`, NOT YET BUILT: built in any context whose row is `R` — `on(c)`, a program there. So
 * it is built again where another context needs it — under an interceptor (`local`, `handleWith`) — and its type
 * names its row: `F[A] = Program[R, A]`, the carrier of code written over `F[_]` (cats, tofu), of a program in
 * okay's shape (`A ! R`, the row in the type), and readable from Scala 2. Built where it runs, so it runs on any
 * monad: `run(top)(p)` */
abstract class Program[R <: Row, A]:
  /** this program built in the context `c` */
  def on(c: Effects { type Effs = R }): c.Prog[A]

  def flatMap[B](f: A => Program[R, B]): Program[R, B] =
    val p = this
    new Program[R, B]:
      // the left side built when the loop gets to it: a chain of binds of any depth built in constant stack
      def on(c: Effects { type Effs = R }): c.Prog[B] = c.delay(p.on(c)).flatMap(a => f(a).on(c))
  def map[B](f: A => B): Program[R, B] =
    val p = this
    new Program[R, B]:
      def on(c: Effects { type Effs = R }): c.Prog[B] = c.delay(p.on(c)).map(f)

object Program:
  /** the program constructor at the row `R`: `Program.Of[R]` is an `F[_]` */
  type Of[R <: Row] = [A] =>> Program[R, A]

  def pure[R <: Row, A](a: A): Program[R, A] = new Program[R, A]:
    def on(c: Effects { type Effs = R }): c.Prog[A] = c.pure(a)
  /** `defer(p)`: the program `p`, made when it is built — a tail call */
  def defer[R <: Row, A](p: => Program[R, A]): Program[R, A] = new Program[R, A]:
    def on(c: Effects { type Effs = R }): c.Prog[A] = c.delay(p.on(c))
  /** an operation of `E`, at its path in the row */
  def perform[E[_], R <: Row, X](op: E[X])(using in: In[E, R]): Program[R, X] = new Program[R, X]:
    def on(c: Effects { type Effs = R }): c.Prog[X] = c.perform(op, in)
  /** a program written in context, at the row `R` */
  def apply[R <: Row, A](p: (c: Effects { type Effs = R }) => c.Prog[A]): Program[R, A] = new Program[R, A]:
    def on(c: Effects { type Effs = R }): c.Prog[A] = p(c)

  /** `run(top)(p)`: a program with nothing left to handle, built at the top of a monad and run there */
  def run[A](top: Top)(p: Program[Pure, A]): A = top.run(p.on(top.at[A]))
