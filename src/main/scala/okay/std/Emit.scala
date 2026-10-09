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

/** A GENERATOR WHATEVER THE MONAD: the next element and the rest, or done. The rest is a program of some monad,
 * but closed over its top's run (`Top.run`), so the type names no monad: `rest()` runs it, each call a run of its
 * own — a program is a value, run as many times as asked */
enum Stream[W]:
  case Next(w: W, rest: () => Stream[W])
  case Done()

  /** the first `n` elements, each pulled by a run of the rest, none past the `n`th — a loop, in constant stack */
  def take(n: Int): List[W] =
    val out = List.newBuilder[W]
    var s = this
    var i = 0
    while i < n do
      s match
        case Next(w, rest) => out += w; i += 1; if i < n then s = rest()
        case Done() => i = n
    out.result()

  /** every element: for a stream that ends */
  def toList: List[W] = take(Int.MaxValue)

/** `stream(t)(body)`: the body as a `Stream` of its elements, on the monad of the top `t` — `okay.zero.top`,
 * `okay.cont.top`, a context's `c.top`. Nothing runs past the first element until the rest is pulled. The body's
 * effects are `Emit` only; under effects still to be handled, `generate` */
def stream[W](t: Top)(body: Yields[W]): Stream[W] =
  val c = t.at[Stream[W]]
  t.run(generate[W, Stream[W]](using c)((w, rest) => Stream.Next(w, () => t.run(rest)), Stream.Done())(body))
/** a stream's body: a program yielding `W`, under `Emit % W` alone */
type Yields[W] = (c: Effects { type Effs = Emit % W +: Pure }) ?=> c.Prog[Unit]

/** a lazy generator over the row `R`: the next element and the rest, a program over `R` at the generator's own
 * answer; or done */
enum Gen[M[_, _, _], W]:
  case Next(w: W, rest: M[Gen[M, W], Gen[M, W], Gen[M, W]])
  case Done()
/** `generate[W, G](next, done)(body)`: the body as a lazy generator — nothing runs until the consumer pulls — of
 * the consumer's own type `G`: each element with the rest, a program here of `G`, by `next`; the end, `done`. Any
 * type will do; `Gen` is one, at a context whose answer it is */
def generate[W, G](using c: Effects)(next: (W, c.Prog[G]) => G, done: G)(body: c.Body[Emit % W, G, Unit]): c.Prog[G] =
  c.handle(new Handler[c.Prog, Emit % W, Unit, G]:
    def ret(u: Unit): G = done
    def apply[X](op: Emit[W, X], k: X => c.Prog[G]): c.Prog[G] = op match
      case Emit.Yield(w) => c.pure(next(w, k(()))))(body)
