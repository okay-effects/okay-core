package okay.core

import okay.one.Cont0
import okay.one.handler.{Answering as OneAnswering, Deep, Handler as OneHandler, Translating}

/** AN OPERATION, not yet a program: of the row where it is bound — `for n <- ask[Int]; _ <- tell(n) yield n`, the
 * row the expected type's, so a program needing effects names no row; or named, `at[R]`, `perform[R]` */
final class Op[E[_], X](val op: E[X]) extends AnyVal:
  def at[R <: Row](using p: Path[E, R]): Eff[R, X] = Cont0.perform[E, R, X](op)(using p.in)
  def perform[R <: Row](using Path[E, R]): Eff[R, X] = at[R]
  def flatMap[R <: Row, B](f: X => Eff[R, B])(using Path[E, R]): Eff[R, B] = at[R].flatMap(f)
  def map[R <: Row, B](f: X => B)(using Path[E, R]): Eff[R, B] = at[R].map(f)
object Op:
  /** an operation where a program is expected: performed there, in the row expected */
  given toProgram[E[_], X, R <: Row](using Path[E, R]): Conversion[Op[E, X], Eff[R, X]] = _.at[R]

/** `perform(op)`: an operation of `E`, a program of the row it is bound in */
def perform[E[_], X](op: E[X]): Op[E, X] = Op(op)
/** `pure(a)`: the value, a program over any row */
def pure[R <: Row, A](a: A): Eff[R, A] = Cont0.pure(a)
def unit[R <: Row]: Eff[R, Unit] = Cont0.pure(())
/** `defer(p)`: `p` built when the run gets to it — a tail call, in constant stack */
def defer[R <: Row, A](p: => Eff[R, A]): Eff[R, A] = Cont0.defer(p)

/** A HANDLER of the effect `E`, over any row outside it: the body's value `A` to the answer `Ans`; each operation with
 * the rest of the body under the handler, `k` — deep, data: dropped, resumed, resumed again, kept. Its clauses are
 * programs over the row outside, `T`, whatever it is — so they perform nothing of it; a handler made of other
 * effects is the kernel's `Deep` over a row that has them (`handleIn`) */
trait Handler[E[_], A, Ans]:
  def ret(a: A): Ans
  def apply[T <: Row, X](op: E[X], k: X => Eff[T, Ans]): Eff[T, Ans]
  /** the kernel's form of it, over the row `T` */
  def at[T <: Row]: OneHandler[E, T, A, Ans] = new Deep[E, T, A, Ans]:
    def ret(a: A): Eff[T, Ans] = Cont0.pure(Handler.this.ret(a))
    def apply[X](op: E[X], k: X => Eff[T, Ans]): Eff[T, Ans] = Handler.this.apply[T, X](op, k)

/** an operation's value, by the operation alone */
trait Value[E[_]]:
  def value[X](op: E[X]): X

/** a handler ANSWERING IN PLACE: each clause `k(value(op))`, the value of the operation alone — the run goes on with
 * the rest of the body at once, nothing captured (the kernel's `Answering`) */
trait Answering[E[_], A, Ans] extends Handler[E, A, Ans] with Value[E]:
  final def apply[T <: Row, X](op: E[X], k: X => Eff[T, Ans]): Eff[T, Ans] = k(value(op))
  override def at[T <: Row]: OneHandler[E, T, A, Ans] = new OneAnswering[E, T, A, Ans]:
    def ret(a: A): Eff[T, Ans] = Cont0.pure(Answering.this.ret(a))
    def value[X](op: E[X]): X = Answering.this.value(op)

/** `handle(h)(body)`: the body at the handler's place — `E` at the head of the row outside, `T` — its value through
 * `h`, its operations of `E` to `h`'s clauses. `T` is the row where the result is used: so the handlers make the row,
 * the outermost last */
def handle[E[_], A, Ans, T <: Row](h: Handler[E, A, Ans])(body: Under[E, T, A]): Eff[T, Ans] = handleIn(h.at[T])(body)

/** the same with a handler in the kernel's form over the row outside, `T` — one made of the effects of `T`: its
 * clauses perform them by their paths, `(using In[Log, T])` */
def handleIn[E[_], T <: Row, A, Ans](h: OneHandler[E, T, A, Ans])(body: Under[E, T, A]): Eff[T, Ans] =
  // the body built when the run enters the handler: a handler installed by a recursive call costs no stack
  defer(body(using Place.head).handle(h))

/** `translate(f)(body)`: `E`'s operations each a program over the row outside, run where it is performed, the body
 * resumed with its value — no `k`, no delimiter (the kernel's `Translating`) */
def translate[E[_], T <: Row, A](f: [X] => E[X] => Eff[T, X])(body: Under[E, T, A]): Eff[T, A] =
  handleIn(new Translating[E, T, A, A]:
    def ret(a: A): Eff[T, A] = Cont0.pure(a)
    def apply[X](op: E[X]): Eff[T, X] = f(op))(body)

/** `intercept(h)(body)`: `E`, already in the row outside, handled again for the body by `h` — its clauses' own
 * operations of `E` go to the handler outside (`local`, `handleWith`) */
def intercept[E[_], T <: Row, A](h: OneHandler[E, T, A, A])(body: Under[E, T, A])(using Path[E, T]): Eff[T, A] =
  handleIn(h)(body)

/** `run(p)`: a program with nothing left to handle — its value */
def run[A](p: Place[Pure] ?=> Eff[Pure, A]): A = p(using Place.top).run
