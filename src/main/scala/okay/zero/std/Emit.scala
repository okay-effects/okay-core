package okay.zero.std

import okay.zero.*

/** EMIT: a body that yields. `collect` keeps every element in a buffer of its own, the body resumed at once;
 * `generate` is LAZY — each `yield` captures the rest of the body as the next step, run when the consumer asks */
enum Emit[W, +A]:
  case Yield[W](w: W) extends Emit[W, Unit]

def yield_[W](w: W)(using c: Ctx, has: Emit % W :? c.R): c.P[Unit] = perform[Emit % W, Unit](Emit.Yield(w))

/** `collect[W, A](body)`: every element yielded, in order, and the value — a buffer per RUN, made when stepped */
def collect[W, A](using c: Ctx)(body: c.Body[Emit % W, (List[W], A), A]): c.P[(List[W], A)] =
  delay:
    handle(new Answering[c.P, Emit % W, A, (List[W], A)]:
      private val out = List.newBuilder[W]
      def ret(a: A): (List[W], A) = (out.result(), a)
      def value[X](op: Emit[W, X]): X = op match
        case Emit.Yield(w) => out += w; ())(body)

/** `generate(next, done)(body)`: the body as a lazy generator, of the consumer's own type `G` — `next(w, rest)` an
 * element and the rest, a program here; `done` the end. Nothing runs until the consumer runs the rest, under the
 * handlers in force where it runs it. `G` is the consumer's NOMINAL type: a program of this level in an answer of
 * this level is a recursive type, which only a class can be — `enum Gen { case Next(w: Int, rest: ...); case Done }` */
def generate[W, G](using c: Ctx)(next: (W, c.P[G]) => G, done: G)(body: c.Body[Emit % W, G, Unit]): c.P[G] =
  handle(new Handler[c.P, Emit % W, Unit, G]:
    def ret(u: Unit): G = done
    def apply[X](op: Emit[W, X], k: X => c.P[G]): c.P[G] = op match
      case Emit.Yield(w) => pure(next(w, k(()))))(body)
