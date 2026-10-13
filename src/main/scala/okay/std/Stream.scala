package okay.std

import okay.core.*
import okay.one.handler.{Answering as OneAnswering, Deep}

/** A STREAM OF ELEMENTS, NOT STARTED: a value — each consumption a run of its own, from the beginning. Pushed
 * (`foreach`, `fold`, `toList`: the whole of it), or pulled (`take`, `iterator`: as far as asked, the rest
 * cancelled). A generator's (`stream`, `Async.stream`: a `Source` run) fuses `map` and `filter` into what its
 * handler does with an element; any other wraps them */
abstract class Stream[W]:
  /** `f` of every element, in order, the stream run to its end */
  def push(f: W => Unit): Unit
  /** the elements, each pulled when asked for; `close()` ends the run where it is */
  def iterator: Stream.Iter[W]

  /** the first `n` elements; the rest never run */
  def take(n: Int): List[W] =
    val it = iterator
    try it.take(n).toList finally it.close()
  def map[B](f: W => B): Stream[B] = Stream.Mapped(this, f)
  def filter(p: W => Boolean): Stream[W] = Stream.Filtered(this, p)
  def withFilter(p: W => Boolean): Stream[W] = filter(p)
  /** each element's stream, one after another */
  def flatMap[B](f: W => Stream[B]): Stream[B] = Stream.Bound(this, f)

  def foreach(f: W => Unit): Unit = push(f)
  def fold[B](z: B)(op: (B, W) => B): B =
    var acc = z
    push(w => acc = op(acc, w))
    acc
  def toList: List[W] =
    val out = List.newBuilder[W]
    push(out += _)
    out.result()

object Stream:
  /** an iterator that can be closed: its run ended where it is */
  trait Iter[W] extends Iterator[W], AutoCloseable:
    def close(): Unit = ()

  /** the elements given */
  def of[W](ws: W*): Stream[W] = new Stream[W]:
    def push(f: W => Unit): Unit = ws.foreach(f)
    def iterator: Iter[W] = new Iter[W]:
      private val it = ws.iterator
      def hasNext: Boolean = it.hasNext
      def next(): W = it.next()

  private final class Mapped[W, B](src: Stream[W], f: W => B) extends Stream[B]:
    def push(g: B => Unit): Unit = src.push(w => g(f(w)))
    def iterator: Iter[B] = new Iter[B]:
      private val it = src.iterator
      def hasNext: Boolean = it.hasNext
      def next(): B = f(it.next())
      override def close(): Unit = it.close()

  private final class Filtered[W](src: Stream[W], p: W => Boolean) extends Stream[W]:
    def push(g: W => Unit): Unit = src.push(w => if p(w) then g(w))
    def iterator: Iter[W] = new Iter[W]:
      private val under = src.iterator
      private val it = under.filter(p)
      def hasNext: Boolean = it.hasNext
      def next(): W = it.next()
      override def close(): Unit = under.close()

  private final class Bound[W, B](src: Stream[W], f: W => Stream[B]) extends Stream[B]:
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
        if !hasNext then throw java.util.NoSuchElementException("Stream")
        inner.nn.next()
      override def close(): Unit =
        if inner != null then inner.nn.close()
        outer.close()

/** A GENERATOR, NOT STARTED: `map` and `filter` fused — composed into what the generator's handler does with an
 * element, no object an element of their own. `start` runs it to its first element, a `Step` */
abstract class Generator[W] extends Stream[W]:
  /** the generator run, each element and the rest through `out` — a program of `G` over nothing, run in place */
  def run[G](out: (W, Eff[Pure, G]) => Eff[Pure, G], done: G): G

  /** started: the first element and the rest, or done */
  def start: Step[W] = run[Step[W]]((w, rest) => pure(Step.Next(w, rest)), Step.Done())

  def iterator: Stream.Iter[W] = new Stream.Iter[W]:
    private val it = start.iterator
    def hasNext: Boolean = it.hasNext
    def next(): W = it.next()

  /** the first `n` elements, the rest never run: each element's rest run in place by the handler, or dropped */
  override def take(n: Int): List[W] =
    val out = List.newBuilder[W]
    if n > 0 then
      var i = 0
      run[Unit]((w, rest) => { out += w; i += 1; if i < n then rest else pure(()) }, ())
    out.result()

  override def map[B](f: W => B): Generator[B] =
    val src = this
    new Generator[B]:
      def run[G](out: (B, Eff[Pure, G]) => Eff[Pure, G], done: G): G = src.run[G]((w, rest) => out(f(w), rest), done)
      def push(g: B => Unit): Unit = src.push(w => g(f(w)))

  /** the elements `p` keeps: a dropped one's rest run in its place */
  override def filter(p: W => Boolean): Generator[W] =
    val src = this
    new Generator[W]:
      def run[G](out: (W, Eff[Pure, G]) => Eff[Pure, G], done: G): G =
        src.run[G]((w, rest) => if p(w) then out(w, rest) else rest, done)
      def push(g: W => Unit): Unit = src.push(w => if p(w) then g(w))
  override def withFilter(p: W => Boolean): Generator[W] = filter(p)

/** `stream(body)`: the source `body` as a `Generator` of its elements. The body's effects are `Emit` and those it
 * handles itself; under effects still to be handled, `generate` */
def stream[W](body: Source[W]): Generator[W] = new Generator[W]:
  def run[G](out: (W, Eff[Pure, G]) => Eff[Pure, G], done: G): G =
    handleIn(new Deep[Emit % W, Pure, Unit, G]:
      def ret(u: Unit): Eff[Pure, G] = pure(done)
      def apply[X](op: Emit[W, X], k: X => Eff[Pure, G]): Eff[Pure, G] = op match
        case Emit.Yield(w) => out(w, k(())))(body).run
  /** answered in place, nothing captured: a whole run's fastest pass */
  def push(f: W => Unit): Unit =
    handleIn(new OneAnswering[Emit % W, Pure, Unit, Unit]:
      def ret(u: Unit): Eff[Pure, Unit] = pure(())
      def value[X](op: Emit[W, X]): X = op match
        case Emit.Yield(w) => f(w))(body).run

/** WHERE A STARTED STREAM STANDS: an element and the rest, or done (`Generator.start`). The rest of a generator's
 * element (`Next`) is a program of the kernel over nothing, its answer this type — one object an element, no
 * closure. The rest of a made one (`Later`: `map`, `flatMap`, `Step.of`) is a function. `pull()` runs the rest,
 * each call a run of its own (a program is a value, run as many times as asked). Every operation lazy, in constant
 * stack */
enum Step[W]:
  case Next(w: W, rest: Eff[Pure, Step[W]])
  case Later(w: W, rest: () => Step[W])
  case Done()

  /** the stream after this element; done, done */
  def pull(): Step[W] = this match
    case Next(_, rest) => rest.run
    case Later(_, rest) => rest()
    case Done() => this

  /** this element: for a stream not done */
  private def head: W = this match
    case Next(w, _) => w
    case Later(w, _) => w
    case Done() => throw java.util.NoSuchElementException("Step.Done")

  /** `f` of each element, as pulled */
  def map[B](f: W => B): Step[B] = this match
    case Done() => Done()
    case _ => Later(f(head), () => pull().map(f))

  /** the stream of each element, one after another, as pulled; the empty ones skipped in a loop */
  def flatMap[B](f: W => Step[B]): Step[B] =
    var s = this
    var out: Step[B] | Null = null
    while out == null do
      s match
        case Done() => out = Done()
        case at => f(at.head) match
          case Done() => s = at.pull()
          case inner => out = Step.concat(inner, () => at.pull().flatMap(f))
    out.nn

  /** the elements `p` keeps, as pulled; those dropped skipped in a loop */
  def filter(p: W => Boolean): Step[W] =
    var s = this
    while s match
        case Done() => false
        case at => !p(at.head)
    do s = s.pull()
    s match
      case Done() => s
      case at => Later(at.head, () => at.pull().filter(p))
  def withFilter(p: W => Boolean): Step[W] = filter(p)

  /** this stream's elements, then `that`'s */
  def ++(that: => Step[W]): Step[W] = Step.concat(this, () => that)

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
    private var s = Step.this
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

object Step:
  /** the elements given, then done */
  def of[W](ws: W*): Step[W] = from(ws.toList)
  private def from[W](ws: List[W]): Step[W] = ws match
    case w :: more => Later(w, () => from(more))
    case Nil => Done()

  /** `s`'s elements, then `that()`'s */
  private def concat[W](s: Step[W], that: () => Step[W]): Step[W] = s match
    case Done() => that()
    case at => Later(at.head, () => concat(at.pull(), that))

