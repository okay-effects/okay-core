package okay.one.std

import okay.one.*
import okay.one.Cont0.{perform, pure}
import okay.one.handler.*
import okay.one.row.*

/** EMIT: a body that yields `W`. `collect` keeps every element, the body resumed at once; `generate` is LAZY — each
 * element given with the rest of the body, `k` kept as data: the consumer's next step, run when it pulls */
enum Emit[W, X]:
  case Yield[W](w: W) extends Emit[W, Unit]

object Emit:
  def emit[W, R <: Row](w: W)(using In[Emit % W, R]): Unit ! R = perform[Emit % W, R, Unit](Yield(w))

  /** every element, in order, and the value: each path its own, a state */
  def collect[W, T <: Row, A]: Stateful[Emit % W, T, List[W], A, (List[W], A)] =
    new Stateful[Emit % W, T, List[W], A, (List[W], A)]:
      def initial = Nil
      def ret(s: List[W], a: A) = pure((s.reverse, a))
      def value[X](s: List[W], op: Emit[W, X]): X = op match { case _: Yield[w] => () }
      def next[X](s: List[W], op: Emit[W, X]) = op match { case y: Yield[w] => y.w :: s }

  /** the body a lazy generator of the consumer's own type `G`: each element with the rest — a program over `T` of
   * `G`, by `next`; the end, `done`. Nothing of the rest runs until it is run */
  def generate[W, G, T <: Row](next: (W, G ! T) => G, done: G): Deep[Emit % W, T, Unit, G] =
    new Deep[Emit % W, T, Unit, G]:
      def ret(u: Unit) = pure(done)
      def apply[X](op: Emit[W, X], k: X => G ! T) = op match { case y: Yield[w] => pure(next(y.w, k(()))) }
