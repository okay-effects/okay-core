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

/** A GENERATOR WHATEVER THE MONAD: an element and the rest, or done. The rest of a generator's element (`Next`) is
 * a program of the monad of `top`, its type `top`'s — so the type `Stream` names no monad, and one object an
 * element, no closure. The rest of a made one (`Later`: `map`, `flatMap`, `Stream.of`) is a function. `pull()` runs
 * the rest, each call a run of its own (a program is a value, run as many times as asked). Every operation lazy, in
 * constant stack */
enum Stream[W]:
  case Next(w: W, top: Top, rest: top.M[Stream[W], Stream[W], Stream[W], Stream[W]])
  case Later(w: W, rest: () => Stream[W])
  case Done()

  /** the stream after this element; done, done */
  def pull(): Stream[W] = this match
    case n: Next[W] => n.top.run(n.rest)
    case Later(_, rest) => rest()
    case Done() => this

  /** this element: for a stream not done */
  private def head: W = this match
    case Next(w, _, _) => w
    case Later(w, _) => w
    case Done() => throw java.util.NoSuchElementException("Stream.Done")

  /** `f` of each element, as pulled */
  def map[B](f: W => B): Stream[B] = this match
    case Done() => Done()
    case _ => Later(f(head), () => pull().map(f))

  /** the stream of each element, one after another, as pulled; the empty ones skipped in a loop */
  def flatMap[B](f: W => Stream[B]): Stream[B] =
    var s = this
    var out: Stream[B] | Null = null
    while out == null do
      s match
        case Done() => out = Done()
        case at => f(at.head) match
          case Done() => s = at.pull()
          case inner => out = Stream.concat(inner, () => at.pull().flatMap(f))
    out.nn

  /** the elements `p` keeps, as pulled; those dropped skipped in a loop */
  def filter(p: W => Boolean): Stream[W] =
    var s = this
    while s match
        case Done() => false
        case at => !p(at.head)
    do s = s.pull()
    s match
      case Done() => s
      case at => Later(at.head, () => at.pull().filter(p))
  def withFilter(p: W => Boolean): Stream[W] = filter(p)

  /** this stream's elements, then `that`'s */
  def ++(that: => Stream[W]): Stream[W] = Stream.concat(this, () => that)

  /** the first `n` elements, each pulled by a run of the rest, none past the `n`th — a loop, in constant stack */
  def take(n: Int): List[W] =
    val out = List.newBuilder[W]
    var s = this
    var i = 0
    while i < n do
      s match
        case Done() => i = n
        case at => out += at.head; i += 1; if i < n then s = at.pull()
    out.result()

  /** every element: for a stream that ends */
  def toList: List[W] = take(Int.MaxValue)

  /** the elements as an `Iterator`, each pulled when asked for, none past the last asked */
  def iterator: Iterator[W] = new Iterator[W]:
    private var s = Stream.this
    private var pulled = true
    def hasNext: Boolean =
      if !pulled then { s = s.pull(); pulled = true }
      s match
        case Done() => false
        case _ => true
    def next(): W =
      hasNext
      val w = s.head
      pulled = false
      w

  /** `f` of every element, in order: for a stream that ends — a loop, in constant stack */
  def foreach(f: W => Unit): Unit =
    var s = this
    while s match
        case Done() => false
        case at => f(at.head); s = at.pull(); true
    do ()

object Stream:
  /** the elements given, then done */
  def of[W](ws: W*): Stream[W] = from(ws.toList)
  private def from[W](ws: List[W]): Stream[W] = ws match
    case w :: more => Later(w, () => from(more))
    case Nil => Done()

  /** `s`'s elements, then `that()`'s */
  private def concat[W](s: Stream[W], that: () => Stream[W]): Stream[W] = s match
    case Done() => that()
    case at => Later(at.head, () => concat(at.pull(), that))

/** A GENERATOR NOT STARTED: its top and what each element becomes. `map` and `filter` here are FUSED — composed
 * into what the generator's handler does with an element, no object an element of their own — and `start` runs
 * it to its first element, a `Stream`. A value: each start a run of its own, from the beginning */
abstract class Source[W]:
  val top: Top
  /** the generator run at its top, each element and the rest through `out`: a program there answering `G` */
  def run[G](out: (W, top.M[G, G, G, G]) => top.M[G, G, G, G], done: G): G
  /** the generator run to its end at its top, `f` of each element where it is yielded — answered in place, no
   * rest captured: a whole stream's fastest pass */
  def push(f: W => Unit): Unit

  /** started: the first element and the rest, or done */
  def start: Stream[W] =
    val c = top.at[Stream[W]]
    run[Stream[W]]((w, rest) => c.pure(Stream.Next(w, top, rest)), Stream.Done())

  /** `f` of each element, in the generator's handler */
  def map[B](f: W => B): Source[B] =
    val src = this
    new Source[B]:
      val top: src.top.type = src.top
      def run[G](out: (B, top.M[G, G, G, G]) => top.M[G, G, G, G], done: G): G =
        src.run[G]((w, rest) => out(f(w), rest), done)
      def push(g: B => Unit): Unit = src.push(w => g(f(w)))

  /** the elements `p` keeps, in the generator's handler: a dropped one's rest run in its place */
  def filter(p: W => Boolean): Source[W] =
    val src = this
    new Source[W]:
      val top: src.top.type = src.top
      def run[G](out: (W, top.M[G, G, G, G]) => top.M[G, G, G, G], done: G): G =
        src.run[G]((w, rest) => if p(w) then out(w, rest) else rest, done)
      def push(g: W => Unit): Unit = src.push(w => if p(w) then g(w))
  def withFilter(p: W => Boolean): Source[W] = filter(p)

  def flatMap[B](f: W => Stream[B]): Stream[B] = start.flatMap(f)
  /** pulled, each element a run of the rest: for any stream, to stop anywhere */
  def take(n: Int): List[W] = start.take(n)
  def iterator: Iterator[W] = start.iterator
  /** pushed, for a stream that ends: the whole of it, nothing captured */
  def foreach(f: W => Unit): Unit = push(f)
  def fold[B](z: B)(op: (B, W) => B): B =
    var acc = z
    push(w => acc = op(acc, w))
    acc
  def toList: List[W] =
    val out = List.newBuilder[W]
    push(out += _)
    out.result()

/** `stream(t)(body)`: the body as a `Source` of its elements, on the monad of the top `t` — `okay.zero.top`,
 * `okay.cont.top`, a context's `c.top`; `start` makes it a `Stream`. Nothing runs past an element until the rest
 * is pulled. The body's effects are `Emit` and those it handles itself; under effects still to be handled,
 * `generate` */
def stream[W](t: Top)(body: Yields[W]): Source[W] = new Source[W]:
  val top: t.type = t
  def run[G](out: (W, t.M[G, G, G, G]) => t.M[G, G, G, G], done: G): G =
    val c = t.at[G]
    t.run(c.handle(new Handler[c.Prog, Emit % W, Unit, G]:
      def ret(u: Unit): G = done
      def apply[X](op: Emit[W, X], k: X => c.Prog[G]): c.Prog[G] = op match
        case Emit.Yield(w) => out(w, k(())))(body))
  def push(f: W => Unit): Unit =
    val c = t.at[Unit]
    t.run(c.handle(new Answering[c.Prog, Emit % W, Unit, Unit]:
      def ret(u: Unit): Unit = ()
      def value[X](op: Emit[W, X]): X = op match
        case Emit.Yield(w) => f(w))(body))
/** a stream's body: a program yielding `W`, under `Emit % W` alone */
type Yields[W] = (c: Effects { type Effs = Emit % W +: Pure }) ?=> c.Prog[Unit]

/** `generate[W, G](next, done)(body)`: the body as a lazy generator — nothing runs until the consumer pulls — of
 * the consumer's own type `G`: each element with the rest, a program here of `G`, by `next`; the end, `done`. Any
 * type will do. At the top, `stream` */
def generate[W, G](using c: Effects)(next: (W, c.Prog[G]) => G, done: G)(body: c.Body[Emit % W, G, Unit]): c.Prog[G] =
  c.handle(new Handler[c.Prog, Emit % W, Unit, G]:
    def ret(u: Unit): G = done
    def apply[X](op: Emit[W, X], k: X => c.Prog[G]): c.Prog[G] = op match
      case Emit.Yield(w) => c.pure(next(w, k(()))))(body)
