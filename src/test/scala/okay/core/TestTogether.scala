package okay.core

import okay.std.*
import okay.core.Control.{reset, shift}

/** CONTROL AND EFFECTS TOGETHER: a reset passes every operation on, a shift's body and its k perform */
class TestTogether extends munit.FunSuite:

  test("k resumed twice, each path telling: the writer outside sees both paths, in order"):
    type T = Writer % String +: Pure
    type Q = Shifts[Int, T] +: T
    val r = run(writer[String, Int, Pure](reset[Int, T](shift[Int, T, Int](k => k(1).flatMap(a => k(2).map(b => a + b))).at[Q]
      .flatMap(x => tell(s"x=$x").at[Q].map(_ => x)))))
    assertEquals(r, (List("x=1", "x=2"), 3))

  test("a state outside the reset, through a k resumed twice: threaded — the second shot sees the first's last, a cell's too"):
    type T = State % Int +: Pure
    type Q = Shifts[Int, T] +: T
    given Place[Q] = Place.at
    def body: Eff[Q, Int] = shift[Int, T, Int](k => k(1).flatMap(_ => k(2))).at[Q].flatMap(x => modify[Int](_ + x))
    assertEquals(run(cell(0)(reset[Int, T](body))), (3, 3))
    assertEquals(run(state(0)(reset[Int, T](body))), (3, 3))

  test("control written for any row, used alone and under a handler"):
    def twice[T <: Row]: Eff[T, Int] = reset[Int, T](shift[Int, T, Int](k => k(10).flatMap(k)).at[Shifts[Int, T] +: T].map(_ + 1))
    assertEquals(run(twice[Pure]), 12)
    assertEquals(run(reader(3)(twice[Reader % Int +: Pure].flatMap(x => ask[Int].at[Reader % Int +: Pure].map(_ + x)))), 15)

  test("100 000 operations passed on by one reset, in constant stack"):
    def asks(n: Int, acc: Int): Int ! Reader % Int =
      if n == 0 then pure(acc) else ask[Int].flatMap(x => asks(n - 1, acc + x))
    assertEquals(run(reader(1)(reset[Int, Reader % Int +: Pure](asks(100000, 0)))), 100000)
