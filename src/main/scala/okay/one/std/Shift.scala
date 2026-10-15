package okay.one.std

import okay.one.*
import okay.one.Cont.{perform, pure, widen}
import okay.one.handler.Deep
import okay.one.machine.Machine
import okay.one.row.*

/** DELIMITED CONTROL AS AN EFFECT of the row, its answer `S` fixed, `T` the row outside its `reset`: under any handlers,
 * in any order. `shift0`'s body runs outside the `reset`, `shift`'s under it (Danvy–Filinski's: a shift in its body
 * captures up to the same `reset`); each given `k`, the rest up to the `reset` — data, a program over `T`.
 * `shift0(k => pure(k))` gives `k` itself, the answer holding it. The answer moved is `Control[R]`'s, not this */
enum Shift[S, T <: Row, X]:
  case Up0[S, T <: Row, X](body: (X => S ! T) => S ! T) extends Shift[S, T, X]
  case Up[S, T <: Row, X](body: (X => S ! Shifts[S, T] +: T) => S ! Shifts[S, T] +: T) extends Shift[S, T, X]

/** the effect of the shifts of answer `S`, outside the `reset` the row `T` */
type Shifts[S, T <: Row] = [x] =>> Shift[S, T, x]

object Shift:
  /** shift0: `body` given `k`, run outside the `reset` */
  def shift0[S, T <: Row, X, R <: Row](body: (X => S ! T) => S ! T)(using In[Shifts[S, T], R]): X ! R =
    perform[Shifts[S, T], R, X](Up0(body))

  /** shift: `body` given `k`, run under the `reset` — a shift in it to the same one */
  def shift[S, T <: Row, X, R <: Row](body: (X => S ! Shifts[S, T] +: T) => S ! Shifts[S, T] +: T)
      (using In[Shifts[S, T], R]): X ! R = perform[Shifts[S, T], R, X](Up(body))

  /** the `reset`: its handler, `Deep` */
  def reset[S, T <: Row]: Deep[Shifts[S, T], T, S, S] = new Deep[Shifts[S, T], T, S, S]:
    def ret(a: S) = pure(a)
    def apply[X](op: Shift[S, T, X], k: X => S ! T) = op match
      case Up0(body) => body(k)
      case Up(body) =>
        // k seen under the body's reset: the view's two parts made once, not at each k
        val rm = Removed.Absent[Shifts[S, T], T]()
        val end = Machine.Frame.end[T, S, Nothing]
        body((x: X) => Cont.View[Shifts[S, T], T, T, S, Nothing, S, Nothing, Nothing](rm, end, k(x))).handle(this)
