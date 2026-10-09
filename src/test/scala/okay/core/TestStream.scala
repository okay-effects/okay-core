package okay.core

import okay.std.*

/** `stream` on a monad's top: one type, `Stream`, whatever the monad */
abstract class TestStreamOn(top: Top, stackSafe: Boolean = true) extends munit.FunSuite:
  override def munitTestTransforms: List[TestTransform] = super.munitTestTransforms :+ TestTransform("stack", t =>
    if stackSafe then t else t.withBody(() => BackendSuite.onLargeStack(t.body())))
  def from(i: Int)(using c: Effects, e: Emit % Int :? c.Effs): c.Prog[Unit] = yield_(i).flatMap(_ => from(i + 1))

  test("stream: the elements in order, an infinite body pulled as far as asked"):
    assertEquals(stream[Int](top)(from(0)).take(5), List(0, 1, 2, 3, 4))

  test("stream: a body that ends, the list ends"):
    assertEquals(stream[Int](top)(yield_(1).flatMap(_ => yield_(2))).toList, List(1, 2))

  test("stream: lazy — the body runs as far as pulled; a rest pulled again, run again, the same elements"):
    var ran = 0
    def counted(i: Int)(using c: Effects, e: Emit % Int :? c.Effs): c.Prog[Unit] =
      c.delay { ran += 1; yield_(i).flatMap(_ => counted(i + 1)) }
    val s = stream[Int](top)(counted(0)).start
    assertEquals(ran, 1)
    assertEquals(s.take(3), List(0, 1, 2))
    assertEquals(ran, 3)
    assertEquals(s.take(3), List(0, 1, 2))
    assertEquals(ran, 5)

  test("stream: an enum — its rest run by its own top, matched"):
    stream[Int](top)(from(5)).start match
      case n @ Stream.Next(w, _, _) =>
        assertEquals(w, 5)
        assertEquals(n.top.run(n.rest).take(2), List(6, 7))
        assertEquals(n.pull().take(2), List(6, 7))
      case Stream.Later(_, _) | Stream.Done() => fail("a generator's element")

  test("stream: an iterator, the collections' own operations, none pulled past the last asked"):
    var ran = 0
    def counted(i: Int)(using c: Effects, e: Emit % Int :? c.Effs): c.Prog[Unit] =
      c.delay { ran += 1; yield_(i).flatMap(_ => counted(i + 1)) }
    assertEquals(stream[Int](top)(counted(0)).iterator.map(_ * 2).filter(_ % 4 == 0).take(3).toList, List(0, 4, 8))
    assertEquals(ran, 5)
    assertEquals(stream[Int](top)(yield_(1).flatMap(_ => yield_(2))).iterator.sum, 3)

  test("stream: foreach, every element in order"):
    val out = List.newBuilder[Int]
    stream[Int](top)(yield_(1).flatMap(_ => yield_(2)).flatMap(_ => yield_(3))).foreach(out += _)
    assertEquals(out.result(), List(1, 2, 3))

  test("stream: map and flatMap, lazy, for-comprehensions"):
    var ran = 0
    def counted(i: Int)(using c: Effects, e: Emit % Int :? c.Effs): c.Prog[Unit] =
      c.delay { ran += 1; yield_(i).flatMap(_ => counted(i + 1)) }
    val pairs = for
      n <- stream[Int](top)(counted(1))
      if n % 2 == 1
      ch <- Stream.of('a', 'b')
    yield s"$n$ch"
    assertEquals(pairs.take(5), List("1a", "1b", "3a", "3b", "5a"))
    assertEquals(ran, 5)
    assertEquals(stream[Int](top)(from(0)).map(_ * 10).take(3), List(0, 10, 20))
    assertEquals((Stream.of(1, 2) ++ stream[Int](top)(from(3)).start).take(4), List(1, 2, 3, 4))

  test("stream: flatMap and filter skip 100 000 empties in constant stack"):
    assertEquals(stream[Int](top)(from(0)).flatMap(n => if n < 100000 then Stream.of() else Stream.of(n)).take(1), List(100000))
    assertEquals(stream[Int](top)(from(0)).filter(_ >= 100000).take(1), List(100000))

  test("stream: other effects handled in the body — a reader"):
    def reading(using c: Effects, r: Reader % Int :? c.Effs, e: Emit % Int :? c.Effs): c.Prog[Unit] =
      ask[Int].flatMap(yield_(_)).flatMap(_ => ask[Int].flatMap(n => yield_(n * 2)))
    assertEquals(stream[Int](top)(reader(5)(reading)).toList, List(5, 10))

  test("stream: a state handled in the body lives in the stream, one cell every pull shares"):
    def counter(using c: Effects, s: State % Int :? c.Effs, e: Emit % Int :? c.Effs): c.Prog[Unit] =
      get[Int].flatMap(n => put(n + 1).flatMap(_ => yield_(n))).flatMap(_ => counter)
    val src = stream[Int](top)(state(0)(counter).map(_ => ()))
    val s = src.start
    assertEquals(s.take(4), List(0, 1, 2, 3))
    // the head was pulled once; the rest goes on from the cell, as a resumption run twice does
    assertEquals(s.take(4), List(0, 4, 5, 6))
    // a source started again: a run of its own, a cell of its own
    assertEquals(src.take(4), List(0, 1, 2, 3))

  test("stream: a source runs nothing until started; map and filter fused, each start from the beginning"):
    var ran = 0
    def counted(i: Int)(using c: Effects, e: Emit % Int :? c.Effs): c.Prog[Unit] =
      c.delay { ran += 1; yield_(i).flatMap(_ => counted(i + 1)) }
    val src = stream[Int](top)(counted(0)).map(_ * 3).filter(_ % 2 == 0).map(_ + 1)
    assertEquals(ran, 0)
    assertEquals(src.take(3), List(1, 7, 13))
    assertEquals(ran, 5)
    assertEquals(src.take(3), List(1, 7, 13))
    assertEquals(src.start match { case _: Stream.Next[Int] => true; case _ => false }, true)

  test("stream: pushed — foreach, fold, toList the whole of a finite one, map and filter fused"):
    def upTo(i: Int, n: Int)(using c: Effects, e: Emit % Int :? c.Effs): c.Prog[Unit] =
      if i > n then c.pure(()) else yield_(i).flatMap(_ => upTo(i + 1, n))
    val src = stream[Int](top)(upTo(1, 100000))
    assertEquals(src.fold(0L)(_ + _), 5000050000L)
    assertEquals(src.filter(_ % 2 == 0).map(_ / 2).toList.take(3), List(1, 2, 3))
    val out = List.newBuilder[Int]
    stream[Int](top)(upTo(1, 3)).map(_ * 10).foreach(out += _)
    assertEquals(out.result(), List(10, 20, 30))

  test("stream: 100 000 elements, a run each"):
    assertEquals(stream[Int](top)(from(0)).take(100000).last, 99999)

class TestStream extends TestStreamOn(okay.cont.top)
class TestStreamOnCont0 extends TestStreamOn(okay.zero.top)
class TestStreamOnRef extends TestStreamOn(okay.zero.Ref.top)
/** the textbook CPS, not stack safe: on a thread of a large stack */
class TestStreamOnCps0 extends TestStreamOn(Cps0.top, stackSafe = false)
