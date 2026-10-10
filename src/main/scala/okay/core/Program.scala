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

  /** THE PROGRAMS OUTSIDE A HANDLER, for its clauses: of a type of their own, `P`, made and bound by plain methods —
   * what Scala 2 can call, and what a clause needs no context for. `perform`: an operation of the row outside, `R` */
  abstract class Ops[R <: Row]:
    type P[A]
    def pure[A](a: A): P[A]
    def flatMap[A, B](p: P[A])(f: A => P[B]): P[B]
    def map[A, B](p: P[A])(f: A => B): P[B]
    def perform[E[_], X](op: E[X])(using in: In[E, R]): P[X]
  /** a context's programs as `Ops` */
  def ops(c: Effects): Ops[c.Effs] { type P[A] = c.Prog[A] } = new Ops[c.Effs]:
    type P[A] = c.Prog[A]
    def pure[A](a: A): c.Prog[A] = c.pure(a)
    def flatMap[A, B](p: c.Prog[A])(f: A => c.Prog[B]): c.Prog[B] = c.flatMap(p)(f)
    def map[A, B](p: c.Prog[A])(f: A => B): c.Prog[B] = c.map(p)(f)
    def perform[E[_], X](op: E[X])(using in: In[E, c.Effs]): c.Prog[X] = c.perform(op, in)

  /** A HANDLER OF `E` FOR PROGRAMS: the body's value to `Ans`; each operation with the rest `k`, its clause over the
   * programs outside, `ops` — the row outside `R`, whatever it is. Written in Scala 3 or in Scala 2 alike */
  trait Handler[E[_], A, Ans]:
    def ret(a: A): Ans
    def apply[R <: Row, X](ops: Ops[R])(op: E[X], k: X => ops.P[Ans]): ops.P[Ans]

  /** A HANDLER OF `E` FOR PROGRAMS OVER THE ROW `O` OUTSIDE IT: as `Handler`, its clauses able to perform the effects
   * of `O` too — `ops.perform`, by their paths in `O` */
  trait HandlerAt[O <: Row, E[_], A, Ans]:
    def ret(a: A): Ans
    def apply[X](ops: Ops[O])(op: E[X], k: X => ops.P[Ans]): ops.P[Ans]

  /** `handleAt(h)(p)`: `E` handled wherever it is in `p`'s row, the row without it `h`'s own, `O` */
  def handleAt[E[_], R <: Row, O <: Row, A, Ans](h: HandlerAt[O, E, A, Ans])(p: Program[R, A])(using rm: Removed.Aux[E, R, O]): Program[O, Ans] =
    new Program[O, Ans]:
      def on(c: Effects { type Effs = O }): c.Prog[Ans] =
        c.handle(new okay.core.Handler[c.Prog, E, A, Ans]:
          def ret(a: A): Ans = h.ret(a)
          def apply[X](op: E[X], k: X => c.Prog[Ans]): c.Prog[Ans] = h(ops(c))(op, k))(hc ?=> p.on(View(hc, rm.tr)))

  /** `handle(h)(p)`: `E` handled WHEREVER it is in `p`'s row — okay's `handle` with `Removed`: `p` built in a view of
   * the handler's context, its paths translated; the program over the row without `E` */
  def handle[E[_], R <: Row, A, Ans](h: Handler[E, A, Ans])(p: Program[R, A])(using rm: Removed[E, R]): Program[rm.Out, Ans] =
    new Program[rm.Out, Ans]:
      def on(c: Effects { type Effs = rm.Out }): c.Prog[Ans] =
        c.handle(new okay.core.Handler[c.Prog, E, A, Ans]:
          def ret(a: A): Ans = h.ret(a)
          def apply[X](op: E[X], k: X => c.Prog[Ans]): c.Prog[Ans] = h(ops(c))(op, k))(hc ?=> p.on(View(hc, rm.tr)))

  /** A HANDLER IN CONTEXT AS ONE OF PROGRAMS: `h`, written in a context — given its body, a program of that context
   * — over `p`, its effect `E` taken off wherever it is in `p`'s row: `p` built in a view of the handler's context
   * (`View`), its paths translated. So an effect is written once, in context; its `Program` is this */
  def handled[E[_], R <: Row, A, Ans](p: Program[R, A])(using rm: Removed[E, R])(
      h: (c: Effects { type Effs = rm.Out }) => c.Body[E, Ans, A] => c.Prog[Ans]): Program[rm.Out, Ans] =
    new Program[rm.Out, Ans]:
      def on(c: Effects { type Effs = rm.Out }): c.Prog[Ans] = h(c)(hc ?=> p.on(View(hc, rm.tr)))

  /** AN INTERCEPTOR IN CONTEXT AS ONE OF PROGRAMS: `h` over `fa`, the row the same — `fa` built again in the
   * interceptor's context */
  def within[R <: Row, A, B](fa: Program[R, A])(
      h: (c: Effects { type Effs = R }) => (c.Reset[B] ?=> c.Within[B, B, A]) => c.Prog[B]): Program[R, B] =
    new Program[R, B]:
      def on(c: Effects { type Effs = R }): c.Prog[B] = h(c)(r ?=> fa.on(r))

  /** `run(top)(p)`: a program with nothing left to handle, built at the top of a monad and run there */
  def run[A](top: Top)(p: Program[Pure, A]): A = top.run(p.on(top.at[A]))
