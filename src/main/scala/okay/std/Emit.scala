package okay.std

import okay.core.*

/** EMIT: a body that yields. Two handlers: `collect` keeps every element in a buffer of its own, the body resumed
 * at once; `generate` is LAZY — each `yield` captures the rest of the body as the next step, a program over what
 * the handler leaves, run when the consumer asks */
enum Emit[W, +A]:
  case Yield[W](w: W) extends Emit[W, Unit]

def yield_[W](w: W)(using c: Effects, has: Emit % W :? c.Effs): c.Prog[Unit] = perform[Emit % W, Unit](Emit.Yield(w))
/** `collect[W, A](body)`: every element yielded, in order, and the value. The elements go to a buffer, one per
 * RUN — made when the program is stepped, as `state`'s cell — and each `yield` resumes the body at once: nothing
 * waits for the body to end (a clause `k(()).map(w :: _)` left one frame an element, unwound at the end). A
 * resumption shares the buffer: a body resumed twice collects both paths' elements, in the order they ran */
def collect[W, A](using c: Effects)(body: c.Body[Emit % W, (List[W], A), A])
    : c.Prog[(List[W], A)] =
  type Ans = (List[W], A)
  c.delay:
    Effects.handle(new Answering[c.Prog, Emit % W, A, Ans]:
      private val out = List.newBuilder[W]
      def ret(a: A): Ans = (out.result(), a)
      def value[X](op: Emit[W, X]): X = op match
        case Emit.Yield(w) => out += w; ())(body)

/** a lazy generator over the row `R`: the next element and the rest, a program over `R` at the generator's own
 * answer; or done */
enum Gen[M[_ <: Row, _, _, _], W, R <: Row]:
  case Next(w: W, rest: M[R, Gen[M, W, R], Gen[M, W, R], Gen[M, W, R]])
  case Done()
/** `generate[W, G](next, done)(body)`: the body as a lazy generator — nothing runs until the consumer pulls — of
 * the consumer's own type `G`: each element with the rest, a program here of `G`, by `next`; the end, `done`. Any
 * type will do; `Gen` is one, at a context whose answer it is */
def generate[W, G](using c: Effects)(next: (W, c.Prog[G]) => G, done: G)(body: c.Body[Emit % W, G, Unit]): c.Prog[G] =
  c.handle(new Handler[c.Prog, Emit % W, Unit, G]:
    def ret(u: Unit): G = done
    def apply[X](op: Emit[W, X], k: X => c.Prog[G]): c.Prog[G] = op match
      case Emit.Yield(w) => c.pure(next(w, k(()))))(body)
