package okay.zero

import Std.*

/** CONTROL AND EFFECTS TOGETHER: a reset passes every operation on to the handlers outside it, a handler is no
 * shift's delimiter — and each serves the other: a shift's body and its k perform, a handler's body resets */
class TestTogether extends munit.FunSuite:

  test("a shift's body is under the reset, in its context: it may perform, the reset passing it on"):
    assertEquals(run[Int](asking(3)(reset(shift[Int, Int, Int](k => number.flatMap(k)).map(_ * 10)))), 30)

  test("an operation under a handler, under a reset: the capture re-delimited, the handler still answers"):
    assertEquals(run[(List[String], Int)](saying(reset(shift[Int, Int, Int](k => k(1).flatMap(k)).flatMap(x => say(x.toString).map(_ => x + 1))))),
      (List("1", "2"), 3))

  test("a shift under a handler, no reset between: k the rest up to the handler, its ret and clauses with it; two shots"):
    type Ans = (List[String], Int)
    val r = run[Ans](saying(shift[Int, Ans, Ans](k => k(1).flatMap((l1, a1) => k(2).map((l2, a2) => (l1 ++ l2, a1 + a2))))
      .flatMap(x => say(x.toString).map(_ => x * 10))))
    assertEquals(r, (List("1", "2"), 30))

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

  test("control written for any row, used under a handler: the same program, the row the handler's"):
    def twice[R <: Row, J, T](using Ctx[R, J]): Eff[R, J, T, T, Int] = reset(shift[Int, Int, Int](k => k(10).flatMap(k)).map(_ + 1))
    assertEquals(run[Int](twice), 12)
    assertEquals(run[Int](asking(3)(twice.flatMap(x => number.map(_ + x)))), 15)
