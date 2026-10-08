package okay.std

import okay.core.*

/** EMIT: a body that yields. Two handlers: `collect` keeps every element in a buffer of its own, the body resumed
 * at once; `generate` is LAZY — each `yield` captures the rest of the body as the next step, a program over what
 * the handler leaves, run when the consumer asks */
enum Emit[W, +A]:
  case Yield[W](w: W) extends Emit[W, Unit]

def yield_[W](w: W)(using c: Effects, has: Emit % W :? c.R): c.M[c.R, c.S, c.S, Unit] = perform[Emit % W, Unit](Emit.Yield(w))
/** `collect[W, A](body)`: every element yielded, in order, and the value. The elements go to a buffer, one per
 * RUN — made when the program is stepped, as `state`'s cell — and each `yield` resumes the body at once: nothing
 * waits for the body to end (a clause `k(()).map(w :: _)` left one frame an element, unwound at the end). A
 * resumption shares the buffer: a body resumed twice collects both paths' elements, in the order they ran */
def collect[W, A](using c: Effects)(body: c.Handled[Emit % W, (List[W], A)] ?=> c.Inner[Emit % W +: c.R, (List[W], A), (List[W], A), A])
    : c.M[c.R, c.S, c.S, (List[W], A)] =
  type Ans = (List[W], A)
  c.delay:
    Effects.handle(new Answering[c.M, Emit % W, c.R, c.S, A, Ans]:
      private val out = List.newBuilder[W]
      def ret(a: A): Ans = (out.result(), a)
      def value[X](op: Emit[W, X]): X = op match
        case Emit.Yield(w) => out += w; ())(body)

/** a lazy generator over the row `R`: the next element and the rest, a program over `R` at the generator's own
 * answer; or done */
enum Gen[M[_ <: Row, _, _, _], W, R <: Row]:
  case Next(w: W, rest: M[R, Gen[M, W, R], Gen[M, W, R], Gen[M, W, R]])
  case Done()
/** `generate[W](body)`: the body as a lazy generator — nothing runs until the consumer pulls. The generator is a
 * program at its own answer, `Gen`, so it is written in a context at that answer; at the top, `run` pulls its
 * next element */
def generate[W](using c: Effects)(body: c.Handled[Emit % W, Gen[c.M, W, c.R]] ?=> c.Inner[Emit % W +: c.R, Gen[c.M, W, c.R], Gen[c.M, W, c.R], Unit])
         (using c.S =:= Gen[c.M, W, c.R]): c.M[c.R, c.S, c.S, Gen[c.M, W, c.R]] =
  type G = Gen[c.M, W, c.R]
  val same = summon[c.S =:= G]
  c.handle(new Handler[c.M, Emit % W, c.R, c.S, Unit, G]:
    def ret(a: Unit): G = Gen.Done()
    def apply[X](op: Emit[W, X], k: X => c.M[c.R, c.S, c.S, G]): c.M[c.R, c.S, c.S, G] = op match
      case Emit.Yield(w) => c.pure(Gen.Next(w, same.substituteCo[[Q] =>> c.M[c.R, Q, Q, G]](k(())))))(body)
