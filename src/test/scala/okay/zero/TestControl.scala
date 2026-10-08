package okay.zero

import Std.*

/** shift and reset with the answers modified, on the kernel beside the effects: a reset passes every operation on
 * to the handlers outside it, a handler is no shift's delimiter — and each serves the other */
class TestControl extends munit.FunSuite:

  test("reset(1 + shift(k => k(k(10)))) is 12"):
    assertEquals(run[Int](reset(shift[Int, Int, Int](k => k(10).flatMap(k)).map(_ + 1))), 12)

  test("the answer type modified: an Int at the hole, a String the reset's"):
    assertEquals(run[String](reset(shift[Int, Int, String](k => k(5).map(_.toString)).map(_ * 2))), "10")

  test("a body dropping k aborts the reset: the rest never runs"):
    var ran = false
    assertEquals(run[Int](reset(shift[Int, Int, Int](_ => pure(-1)).map(x => { ran = true; x }))), -1)
    assert(!ran)

  test("multi-shot: k resumed twice, the answers joined"):
    assertEquals(run[List[Int]](reset(shift[Int, List[Int], List[Int]](k => k(1).flatMap(a => k(2).map(b => a ++ b))).map(x => List(x, x * 10)))),
      List(1, 10, 2, 20))

  test("a shift captures up to the nearest reset only"):
    assertEquals(run[Int](resetAt[Int](shift[Int, Int, Int](k => k(1).map(_ * 100))).flatMap(a => reset(shift[Int, Int, Int](k => k(2).map(_ + 7)).map(_ + a)))), 109)

  test("an operation inside a reset reaches its handler outside: the reset passes it on, the rest resumed in it"):
    assertEquals(run[Int](asking(5)(reset(shift[Int, Int, Int](k => k(1)).flatMap(x => number.map(_ + x))))), 6)

  test("a shift's body runs outside the reset: its operations, and k's, go to the handlers outside"):
    // the body asks (5), resumes k with it; the rest asks again (5) inside the reset
    assertEquals(run[Int](asking(5)(reset(shift[Int, Int, Int](k => number.flatMap(k)).flatMap(x => number.map(_ + x))))), 10)

  test("k resumed twice, each path saying: the handler outside sees both paths, in order"):
    val p = run[(List[String], Int)](saying(reset(shift[Int, Int, Int](k => k(1).flatMap(a => k(2).map(b => a + b))).flatMap(x => say(s"x=$x").map(_ => x)))))
    assertEquals(p, (List("x=1", "x=2"), 3))

  test("functional state through a k resumed twice: the second resumption sees the first's state"):
    val p = run[(Int, Int)](stating(0)(reset(shift[Int, Int, Int](k => k(1).flatMap(_ => k(2))).flatMap(x => get.flatMap(s => put(s + x)).flatMap(_ => get)))))
    assertEquals(p, (3, 3))

  test("a reset inside a handler's body, the answer modified there: control under an effect"):
    assertEquals(run[String](asking(4)(reset(shift[Int, Int, String](k => k(1).map(n => s"<$n>")).flatMap(x => number.map(_ * x))))), "<4>")

  test("100 000 resets and shifts, and 100 000 operations passed on by one reset, in constant stack"):
    def many[R <: Row, J, T](n: Int, acc: Int)(using Ctx[R, J]): Eff[R, J, T, T, Int] =
      if n == 0 then pure(acc) else resetAt[T](shift[Int, Int, Int](k => k(1)).map(_ + 1)).flatMap(x => many(n - 1, acc + x))
    assertEquals(run[Int](many(100000, 0)), 200000)
    def asks[R <: Row, J, T](n: Int, acc: Int)(using Ctx[R, J], Ask :? R): Eff[R, J, T, T, Int] =
      if n == 0 then pure(acc) else number.flatMap(x => asks(n - 1, acc + x))
    assertEquals(run[Int](asking(1)(reset(asks(100000, 0)))), 100000)
