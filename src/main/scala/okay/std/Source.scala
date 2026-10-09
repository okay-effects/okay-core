package okay.std

import okay.core.*

/** A SOURCE OF ELEMENTS, NOT STARTED: a value — each consumption a run of its own, from the beginning. Pushed
 * (`foreach`, `fold`, `toList`: the whole of it), or pulled (`take`, `iterator`: as far as asked, the rest
 * cancelled). A generator's (`source`, `Async.source`) fuses `map` and `filter` into what its handler does with an
 * element; any other wraps them */
abstract class Source[W]:
  /** `f` of every element, in order, the source run to its end */
  def push(f: W => Unit): Unit
  /** the elements, each pulled when asked for; `close()` ends the run where it is */
  def iterator: Source.Iter[W]

  /** the first `n` elements; the rest never run */
  def take(n: Int): List[W] =
    val it = iterator
    try it.take(n).toList finally it.close()
  def map[B](f: W => B): Source[B] = Source.Mapped(this, f)
  def filter(p: W => Boolean): Source[W] = Source.Filtered(this, p)
  def withFilter(p: W => Boolean): Source[W] = filter(p)
  /** each element's source, one after another */
  def flatMap[B](f: W => Source[B]): Source[B] = Source.Bound(this, f)

  def foreach(f: W => Unit): Unit = push(f)
  def fold[B](z: B)(op: (B, W) => B): B =
    var acc = z
    push(w => acc = op(acc, w))
    acc
  def toList: List[W] =
    val out = List.newBuilder[W]
    push(out += _)
    out.result()

object Source:
  /** an iterator that can be closed: its run ended where it is */
  trait Iter[W] extends Iterator[W], AutoCloseable:
    def close(): Unit = ()

  /** the elements given */
  def of[W](ws: W*): Source[W] = new Source[W]:
    def push(f: W => Unit): Unit = ws.foreach(f)
    def iterator: Iter[W] = new Iter[W]:
      private val it = ws.iterator
      def hasNext: Boolean = it.hasNext
      def next(): W = it.next()

  private final class Mapped[W, B](src: Source[W], f: W => B) extends Source[B]:
    def push(g: B => Unit): Unit = src.push(w => g(f(w)))
    def iterator: Iter[B] = new Iter[B]:
      private val it = src.iterator
      def hasNext: Boolean = it.hasNext
      def next(): B = f(it.next())
      override def close(): Unit = it.close()

  private final class Filtered[W](src: Source[W], p: W => Boolean) extends Source[W]:
    def push(g: W => Unit): Unit = src.push(w => if p(w) then g(w))
    def iterator: Iter[W] = new Iter[W]:
      private val under = src.iterator
      private val it = under.filter(p)
      def hasNext: Boolean = it.hasNext
      def next(): W = it.next()
      override def close(): Unit = under.close()

  private final class Bound[W, B](src: Source[W], f: W => Source[B]) extends Source[B]:
    def push(g: B => Unit): Unit = src.push(w => f(w).push(g))
    def iterator: Iter[B] = new Iter[B]:
      private val outer = src.iterator
      private var inner: Iter[B] | Null = null
      def hasNext: Boolean =
        var more = true
        while more && (inner == null || !inner.nn.hasNext) do
          if inner != null then inner.nn.close()
          inner = null
          if outer.hasNext then inner = f(outer.next()).iterator else more = false
        more
      def next(): B =
        if !hasNext then throw java.util.NoSuchElementException("Source")
        inner.nn.next()
      override def close(): Unit =
        if inner != null then inner.nn.close()
        outer.close()

/** A GENERATOR ON A MONAD'S TOP, NOT STARTED: `map` and `filter` fused — composed into what the generator's handler
 * does with an element, no object an element of their own. `start` runs it to its first element, a `Stream` */
abstract class Generator[W] extends Source[W]:
  val top: Top
  /** the generator run at its top, each element and the rest through `out`: a program there answering `G` */
  def run[G](out: (W, top.M[G, G, G, G]) => top.M[G, G, G, G], done: G): G

  /** started: the first element and the rest, or done */
  def start: Stream[W] =
    val c = top.at[Stream[W]]
    run[Stream[W]]((w, rest) => c.pure(Stream.Next(w, top, rest)), Stream.Done())

  def iterator: Source.Iter[W] = new Source.Iter[W]:
    private val it = start.iterator
    def hasNext: Boolean = it.hasNext
    def next(): W = it.next()

  /** the first `n` elements, the rest never run: each element's rest run in place by the handler, or dropped */
  override def take(n: Int): List[W] =
    val out = List.newBuilder[W]
    if n > 0 then
      val c = top.at[Unit]
      var i = 0
      run[Unit]((w, rest) => { out += w; i += 1; if i < n then rest else c.pure(()) }, ())
    out.result()

  override def map[B](f: W => B): Generator[B] =
    val src = this
    new Generator[B]:
      val top: src.top.type = src.top
      def run[G](out: (B, top.M[G, G, G, G]) => top.M[G, G, G, G], done: G): G =
        src.run[G]((w, rest) => out(f(w), rest), done)
      def push(g: B => Unit): Unit = src.push(w => g(f(w)))

  /** the elements `p` keeps: a dropped one's rest run in its place */
  override def filter(p: W => Boolean): Generator[W] =
    val src = this
    new Generator[W]:
      val top: src.top.type = src.top
      def run[G](out: (W, top.M[G, G, G, G]) => top.M[G, G, G, G], done: G): G =
        src.run[G]((w, rest) => if p(w) then out(w, rest) else rest, done)
      def push(g: W => Unit): Unit = src.push(w => if p(w) then g(w))
  override def withFilter(p: W => Boolean): Generator[W] = filter(p)

/** `source(t)(body)`: the body as a `Generator` of its elements, on the monad of the top `t` — `okay.zero.top`,
 * `okay.cont.top`, a context's `c.top`. The body's effects are `Emit` and those it handles itself; under effects
 * still to be handled, `generate` */
def source[W](t: Top)(body: Yields[W]): Generator[W] = new Generator[W]:
  val top: t.type = t
  def run[G](out: (W, t.M[G, G, G, G]) => t.M[G, G, G, G], done: G): G =
    val c = t.at[G]
    t.run(c.handle(new Handler[c.Prog, Emit % W, Unit, G]:
      def ret(u: Unit): G = done
      def apply[X](op: Emit[W, X], k: X => c.Prog[G]): c.Prog[G] = op match
        case Emit.Yield(w) => out(w, k(())))(body))
  /** answered in place, nothing captured: a whole run's fastest pass */
  def push(f: W => Unit): Unit =
    val c = t.at[Unit]
    t.run(c.handle(new Answering[c.Prog, Emit % W, Unit, Unit]:
      def ret(u: Unit): Unit = ()
      def value[X](op: Emit[W, X]): X = op match
        case Emit.Yield(w) => f(w))(body))

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

