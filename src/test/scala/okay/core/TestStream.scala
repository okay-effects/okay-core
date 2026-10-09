package okay.core

import okay.std.*

/** `stream` on a monad's top: one type, `Stream`, whatever the monad */
abstract class TestStreamOn(top: Top) extends munit.FunSuite:
  def from(i: Int)(using c: Effects, e: Emit % Int :? c.Effs): c.Prog[Unit] = yield_(i).flatMap(_ => from(i + 1))

  test("stream: the elements in order, an infinite body pulled as far as asked"):
    assertEquals(stream[Int](top)(from(0)).take(5), List(0, 1, 2, 3, 4))

  test("stream: a body that ends, the list ends"):
    assertEquals(stream[Int](top)(yield_(1).flatMap(_ => yield_(2))).toList, List(1, 2))

  test("stream: lazy — the body runs as far as pulled; a rest pulled again, run again, the same elements"):
    var ran = 0
    def counted(i: Int)(using c: Effects, e: Emit % Int :? c.Effs): c.Prog[Unit] =
      c.delay { ran += 1; yield_(i).flatMap(_ => counted(i + 1)) }
    val s = stream[Int](top)(counted(0))
    assertEquals(ran, 1)
    assertEquals(s.take(3), List(0, 1, 2))
    assertEquals(ran, 3)
    assertEquals(s.take(3), List(0, 1, 2))
    assertEquals(ran, 5)

  test("stream: an enum — its rest run by its own top, matched"):
    stream[Int](top)(from(5)) match
      case n @ Stream.Next(w, _, _) =>
        assertEquals(w, 5)
        assertEquals(n.top.run(n.rest).take(2), List(6, 7))
        assertEquals(n.pull().take(2), List(6, 7))
      case Stream.Done() => fail("an element")

  test("stream: 100 000 elements, a run each"):
    assertEquals(stream[Int](top)(from(0)).take(100000).last, 99999)

class TestStream extends TestStreamOn(okay.cont.top)
class TestStreamOnCont0 extends TestStreamOn(okay.zero.top)
class TestStreamOnRef extends TestStreamOn(okay.zero.Ref.top)
class TestStreamOnCps0 extends TestStreamOn(Cps0.top)
