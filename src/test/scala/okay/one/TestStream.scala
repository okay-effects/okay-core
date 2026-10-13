package okay.one

import okay.one.Cont0.{pure, rows}
import okay.one.handler.*
import okay.one.row.*
import okay.one.std.*
import scala.annotation.tailrec

/** okay-core's TestStream on `generate`: a body made a lazy generator, `k` kept as the next step */
class TestStream extends munit.FunSuite:
  /** the consumer's type: an element with the rest, or the end */
  enum Gen:
    case Done
    case Next(w: Int, rest: Gen ! Pure)

  type E = Emit % Int
  def stream(body: Unit ! E +: Pure): Gen ! Pure = body.handle(Emit.generate[Int, Gen, Pure](Gen.Next(_, _), Gen.Done))

  def take(g: Gen ! Pure, n: Int): List[Int] =
    @tailrec def go(g: Gen ! Pure, n: Int, acc: List[Int]): List[Int] =
      if n == 0 then acc.reverse
      else g.run match
        case Gen.Done => acc.reverse
        case Gen.Next(w, rest) => go(rest, n - 1, w :: acc)
    go(g, n, Nil)

  def from[R <: Row](i: Int)(using In[E, R]): Unit ! R = Emit.emit[Int, R](i).flatMap(_ => Cont0.defer(from[R](i + 1)))

  test("the elements in order, an infinite body pulled as far as asked") {
    assertEquals(take(stream(from(0)), 5), List(0, 1, 2, 3, 4))
  }

  test("a body that ends, the list ends") {
    assertEquals(take(stream(Emit.emit[Int, E +: Pure](1).flatMap(_ => Emit.emit[Int, E +: Pure](2))), 10), List(1, 2))
  }

  test("lazy: the body runs as far as pulled; a rest pulled again, run again, the same elements") {
    var ran = 0
    def counted(i: Int): Unit ! E +: Pure = Cont0.defer { ran += 1; Emit.emit[Int, E +: Pure](i).flatMap(_ => counted(i + 1)) }
    val first = stream(counted(0)).run
    assertEquals(ran, 1)
    first match
      case Gen.Next(w, rest) =>
        assertEquals(w :: take(rest, 2), List(0, 1, 2))
        assertEquals(ran, 3)
        assertEquals(w :: take(rest, 2), List(0, 1, 2))
        assertEquals(ran, 5)
      case Gen.Done => fail("empty")
  }

  test("other effects handled in the body: a reader") {
    type R = Reader % Int +: E +: Pure
    val reading = Reader.ask[Int, R].flatMap(n => Emit.emit[Int, R](n))
      .flatMap(_ => Reader.ask[Int, R].flatMap(n => Emit.emit[Int, R](n * 2)))
    assertEquals(take(stream(reading.handle(Reader(5))), 10), List(5, 10))
  }

  /** a counter in the body under `h`, its state the stream's */
  def counter[R <: Row](using In[State % Int, R], In[E, R]): Unit ! R =
    State.get[Int, R].flatMap(n => State.put[Int, R](n + 1).flatMap(_ => Emit.emit[Int, R](n))).flatMap(_ => Cont0.defer(counter[R]))

  test("a cell handled in the body lives in the stream: one every pull shares") {
    type R = State % Int +: E +: Pure
    def cell[T <: Row, A](s0: Int) = new Answering[State % Int, T, A, A]:
      var s = s0
      def ret(a: A) = pure(a)
      def value[X](op: State[Int, X]): X = op match
        case _: State.Get[s] => s
        case p: State.Put[s] => s = p.s
    val src = stream(Cont0.defer(counter[R].handle(cell(0))))
    src.run match
      case Gen.Next(w, rest) =>
        assertEquals(w :: take(rest, 3), List(0, 1, 2, 3))
        // the head pulled once; the rest goes on from the cell
        assertEquals(w :: take(rest, 3), List(0, 4, 5, 6))
      case Gen.Done => fail("empty")
    // a source started again: a run of its own, a cell of its own
    assertEquals(take(src, 4), List(0, 1, 2, 3))
  }

  test("a state handled in the body: each rest its state as it was taken — the register, not a cell") {
    type R = State % Int +: E +: Pure
    val src = stream(counter[R].handle(State(0)).map(_ => ()))
    src.run match
      case Gen.Next(w, rest) =>
        assertEquals(w :: take(rest, 3), List(0, 1, 2, 3))
        assertEquals(w :: take(rest, 3), List(0, 1, 2, 3))
      case Gen.Done => fail("empty")
  }

  test("100 000 elements pulled, in constant stack; collect keeps them all") {
    assertEquals(take(stream(from(0)), 100000).last, 99999)
    def upTo(i: Int, n: Int): Unit ! E +: Pure = if i == n then pure(()) else Emit.emit[Int, E +: Pure](i).flatMap(_ => upTo(i + 1, n))
    assertEquals(upTo(0, 100000).handle(Emit.collect[Int, Pure, Unit]).run._1.length, 100000)
  }
