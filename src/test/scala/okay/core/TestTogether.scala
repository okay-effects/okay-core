package okay.core

import okay.cont.{Cont, Free, At}

import Effects.handle
import Control.{reset, shift}
import okay.std.*

/** CONTROL AND EFFECTS TOGETHER, beyond master's: a reset passes every operation on, a shift's body and its k
 * perform, a handler's body resets with the answer modified */
abstract class TestTogetherOn(on: Backend) extends munit.FunSuite:
  def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A = on.run(body)

  test("k resumed twice, each path telling: the writer outside sees both paths, in order"):
    val r = run(writer[String, Int](reset[Int](shift[Int, Int](k => k(1).flatMap(a => k(2).map(b => a + b))).flatMap(x => tell(s"x=$x").map(_ => x)))))
    assertEquals(r, (List("x=1", "x=2"), 3))

  test("a cell's state through a k resumed twice: the second resumption sees the first's"):
    val r = run(state[Int](0)(reset[Int](shift[Int, Int](k => k(1).flatMap(_ => k(2))).flatMap(x => modify[Int](_ + x)))))
    assertEquals(r, (3, 3))

  test("a reset inside a handler's body, the answer modified there: control under an effect"):
    assertEquals(run(reader[String](4)(reset[Int](shift[Int, String](k => k(1).map(n => s"<$n>")).flatMap(x => ask[Int].map(_ * x))))), "<4>")

  test("control written for any context, used alone and under a handler"):
    def twice(using c: Effects): c.Prog[Int] = reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1))
    assertEquals(run[Int](twice), 12)
    assertEquals(run(reader[Int](3)(twice.flatMap(x => ask[Int].map(_ + x)))), 15)

  test("100 000 operations passed on by one reset, in constant stack"):
    def asks(n: Int, acc: Int)(using c: Effects, r: Reader % Int :? c.Effs): c.Prog[Int] =
      if n == 0 then pure(acc) else ask[Int].flatMap(x => asks(n - 1, acc + x))
    assertEquals(run(reader[Int](1)(reset[Int](asks(100000, 0)))), 100000)

/** on `Cont` */
class TestTogether extends TestTogetherOn(Backend.cont)
