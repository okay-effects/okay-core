package okay.std

import okay.core.*
import okay.one.handler.{Deep, Stateful}

/** EMIT: a body that yields. Two handlers: `collect` keeps every element, a state of the kernel's — each path its
 * own; `generate` is LAZY — each `yield` gives the rest of the body as the next step, `k` data, a program over the
 * row outside, run when the consumer asks */
enum Emit[W, X]:
  case Yield[W](w: W) extends Emit[W, Unit]

def yield_[W](w: W): Op[Emit % W, Unit] = perform(Emit.Yield(w))
/** `collect(body)`: every element yielded, in order, and the value */
def collect[W, A, T <: Row](body: Under[Emit % W, T, A]): Eff[T, (List[W], A)] =
  handleIn(new Stateful[Emit % W, T, List[W], A, (List[W], A)]:
    def initial: List[W] = Nil
    def ret(s: List[W], a: A): Eff[T, (List[W], A)] = pure((s.reverse, a))
    def value[X](s: List[W], op: Emit[W, X]): X = op match { case Emit.Yield(_) => () }
    def next[X](s: List[W], op: Emit[W, X]): List[W] = op match { case Emit.Yield(w) => w :: s })(body)

/** A SOURCE: a program yielding `W`, under `Emit % W` alone — a stream's body, run by `stream` */
type Source[W] = Under[Emit % W, Pure, Unit]

/** `generate(next, done)(body)`: the body as a lazy generator — nothing runs until the consumer pulls — of the
 * consumer's own type `G`: each element with the rest, a program over the row outside of `G`, by `next`; the end,
 * `done`. Any type will do. At the top, `stream` */
def generate[W, G, T <: Row](next: (W, Eff[T, G]) => G, done: G)(body: Under[Emit % W, T, Unit]): Eff[T, G] =
  handleIn(new Deep[Emit % W, T, Unit, G]:
    def ret(u: Unit): Eff[T, G] = pure(done)
    def apply[X](op: Emit[W, X], k: X => Eff[T, G]): Eff[T, G] = op match
      case Emit.Yield(w) => pure(next(w, k(()))))(body)
