package okay.core

/** OKAY'S INTERFACE, as okay's root `Effects[M]` has it (okay/src/main/scala/Effects.scala): a program `M[R, A]`, its
 * row `R` in its type; an operation by its path in the row; a handler taking its effect off WHEREVER it is
 * (`Removed`) — the order of effects in a type says nothing, the order of handlers everything. The primitives, then
 * okay's words over them: `A ! R`, `effect`, `op.perform`, `p.handle(h)`, `p.value`. okay-core's instance is
 * `RowEffects.on(top)`, over `Program`, on the monad of `top` — the shape okay's facade can name beside its own */
trait RowEffects[M[_ <: Row, _]]:
  def pure[R <: Row, A](a: A): M[R, A]
  /** an operation of `E`, by its path in the row */
  def perform[E[_], R <: Row, X](op: E[X])(using In[E, R]): M[R, X]
  /** `thunk`'s program made when it is run, then `f`: stack safety for any shape of binds */
  def defer[R <: Row, A, B](thunk: () => M[R, A])(f: A => M[R, B]): M[R, B]
  def tailcall[R <: Row, A](thunk: => M[R, A]): M[R, A] = defer(() => thunk)(pure)
  extension [R <: Row, A](m: M[R, A])
    def flatMap[B](f: A => M[R, B]): M[R, B]
    def map[B](f: A => B): M[R, B] = m.flatMap(a => pure(f(a)))
  /** `E` handled wherever it is in `m`'s row: the program over the row without it */
  def handle[E[_], A, Ans, R <: Row](h: Program.Handler[E, A, Ans])(m: M[R, A])(using rm: Removed[E, R]): M[rm.Out, Ans]
  /** a program with nothing left to handle, to its value */
  def run[A](m: M[Pure, A]): A

  // -- okay's words, each an alias over the primitives

  /** `A ! R`: a program of value `A` over the row `R` */
  infix type ![A, R <: Row] = M[R, A]
  /** `effect(op)`: an operation, its row the program's it is bound into — taken from the expected type by `flatMap`
   * and `map`, or given by `at[R]` */
  def effect[E[_], X](op: E[X]): Op[E, X] = Op(op)
  extension [E[_], X](op: E[X])
    /** `op.perform`: the same, written after the operation */
    def perform: Op[E, X] = Op(op)
  extension [R <: Row, A](p: M[R, A])
    /** `p.handle(h)`: `handle(h)(p)` */
    def handle[E[_], Ans](h: Program.Handler[E, A, Ans])(using rm: Removed[E, R]): M[rm.Out, Ans] = this.handle[E, A, Ans, R](h)(p)
  extension [A](p: M[Pure, A])
    /** `p.value`: `run(p)` */
    def value: A = run(p)

  /** an operation not yet at a row: its path found where it is bound */
  final class Op[E[_], X](val op: E[X]):
    def flatMap[R <: Row, B](f: X => M[R, B])(using In[E, R]): M[R, B] = perform[E, R, X](op).flatMap(f)
    def map[R <: Row, B](f: X => B)(using In[E, R]): M[R, B] = perform[E, R, X](op).map(f)
    def at[R <: Row](using In[E, R]): M[R, X] = perform[E, R, X](op)

object RowEffects:
  /** okay's other way to write a row: `Pure + Ask + Say`, the effects added one by one — `Say +: Ask +: Pure`; the
   * order of a row says nothing. `import okay.core.RowEffects.+` (the facade's `+` joins effects, not rows) */
  infix type +[R <: Row, E[_]] = E +: R

  /** okay's interface over `Program`, run on the monad of `top` — `okay.zero.top`, `okay.cont.top` */
  def on(top: Top): RowEffects[Program] = new RowEffects[Program]:
    def pure[R <: Row, A](a: A): Program[R, A] = Program.pure(a)
    def perform[E[_], R <: Row, X](op: E[X])(using in: In[E, R]): Program[R, X] = Program.perform(op)
    def defer[R <: Row, A, B](thunk: () => Program[R, A])(f: A => Program[R, B]): Program[R, B] = Program.defer(thunk()).flatMap(f)
    override def tailcall[R <: Row, A](thunk: => Program[R, A]): Program[R, A] = Program.defer(thunk)
    extension [R <: Row, A](m: Program[R, A])
      def flatMap[B](f: A => Program[R, B]): Program[R, B] = m.flatMap(f)
      override def map[B](f: A => B): Program[R, B] = m.map(f)
    def handle[E[_], A, Ans, R <: Row](h: Program.Handler[E, A, Ans])(m: Program[R, A])(using rm: Removed[E, R]): Program[rm.Out, Ans] =
      Program.handle(h)(m)
    def run[A](m: Program[Pure, A]): A = Program.run(top)(m)
