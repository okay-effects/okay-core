package okay.core

/** OKAY'S INTERFACE, as okay's root `Effects[M]` has it (okay/src/main/scala/Effects.scala): a program `M[R, A]`, its
 * row `R` in its type; an operation by its path in the row; a handler taking its effect off WHEREVER it is
 * (`Removed`) — the order of effects in a type says nothing, the order of handlers everything. The primitives, then
 * okay's words over them: `A ! R`, `effect`, `op.perform`, `p.handle(h)`, `p.value`. The instance is the kernel's
 * program itself, `Eff` — no `Program` built at a row under it any more */
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
  /** `E` handled wherever it is in `m`'s row, the row named: the program over the row without it, `O` */
  def handleAt[E[_], A, Ans, R <: Row, O <: Row](h: Handler[E, A, Ans])(m: M[R, A])(using rm: Removed[E, R, O]): M[O, Ans]
  /** a program with nothing left to handle, to its value */
  def run[A](m: M[Pure, A]): A

  // -- okay's words, each an alias over the primitives — the facade's, over this encoding

  /** `A ! Es`: a program of `A` NEEDING the effects `Es`, `Reader % Int + Writer % String` — in any order, among any
   * others: fitted to the row of the place it runs at (`Needs`), its operations' paths found there. The same
   * program fits every place that has its effects */
  infix type ![A, Es[_]] = (c: Needs[Es]) ?=> M[c.R, A]
  /** a body under a handler of `E`: at the head of the row outside, `T` */
  type Under[E[_], T <: Row, A] = Place[E +: T] ?=> M[E +: T, A]
  /** `effect(op)`: an operation, its row the program's it is bound into — found where it is bound (`flatMap`,
   * `map`), or given, `at[R]` */
  def effect[E[_], X](op: E[X]): Operation[E, X] = new Operation(op)
  extension [E[_], X](op: E[X])
    /** `op.perform`: the same, written after the operation */
    def perform: Operation[E, X] = new Operation(op)
  /** `handle(h)(body)`: the body at the handler's place, `E` at the head of the row outside, `T` — its value through
   * `h`, its operations of `E` to `h`'s clauses; `T` the row where the result is used, so the handlers make the row,
   * the outermost last */
  def handle[E[_], A, Ans, T <: Row](h: Handler[E, A, Ans])(body: Under[E, T, A])(using rm: Removed[E, E +: T, T]): M[T, Ans] =
    handleAt[E, A, Ans, E +: T, T](h)(body(using Place.head))
  extension [R <: Row, A](p: M[R, A])
    /** `p.handle(h)`: `handle(h)(p)` at the row `R` named, `E` wherever it is in it */
    def handle[E[_], Ans, O <: Row](h: Handler[E, A, Ans])(using rm: Removed[E, R, O]): M[O, Ans] = handleAt[E, A, Ans, R, O](h)(p)
  extension [A](p: M[Pure, A])
    /** `p.value`: `run(p)` */
    def value: A = run(p)
  /** `run(p)`: a program at the top, the empty row, to its value */
  def run[A](p: Place[Pure] ?=> M[Pure, A]): A = run(p(using Place.top))

  /** an operation not yet at a row: its path found where it is bound — in a program needing effects, among them
   * (`Path`, from the place first) */
  final class Operation[E[_], X](val op: E[X]):
    def flatMap[R <: Row, B](f: X => M[R, B])(using p: Path[E, R]): M[R, B] = perform[E, R, X](op)(using p.in).flatMap(f)
    def map[R <: Row, B](f: X => B)(using p: Path[E, R]): M[R, B] = perform[E, R, X](op)(using p.in).map(f)
    def at[R <: Row](using p: Path[E, R]): M[R, X] = perform[E, R, X](op)(using p.in)

object RowEffects:
  /** okay's interface over the kernel's program itself */
  given eff: RowEffects[Eff] with
    def pure[R <: Row, A](a: A): Eff[R, A] = okay.core.pure(a)
    def perform[E[_], R <: Row, X](op: E[X])(using in: In[E, R]): Eff[R, X] = okay.one.Cont.perform[E, R, X](op)
    def defer[R <: Row, A, B](thunk: () => Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] = okay.core.defer(thunk()).flatMap(f)
    override def tailcall[R <: Row, A](thunk: => Eff[R, A]): Eff[R, A] = okay.core.defer(thunk)
    extension [R <: Row, A](m: Eff[R, A])
      def flatMap[B](f: A => Eff[R, B]): Eff[R, B] = m.flatMap(f)
      override def map[B](f: A => B): Eff[R, B] = m.map(f)
    def handleAt[E[_], A, Ans, R <: Row, O <: Row](h: Handler[E, A, Ans])(m: Eff[R, A])(using rm: Removed[E, R, O]): Eff[O, Ans] =
      okay.one.Cont.handle(m)(h.at[O])
    def run[A](m: Eff[Pure, A]): A = m.run
