package okay.zero

import Cont0.*

/** the monad alone: shift0, $, the answers modified, the loop */
class TestCont0 extends munit.FunSuite:
  type P = Cont0[Int, Int, Int]
  type L = Cont0[P, P, Int]

  test("reset0(1 + shift0(k => k(k(10)))) is 12"):
    assertEquals(shift0[Int, Int, Int, P, Int](k => k(10).flatMap(k)).map(_ + 1).reset0.value, 12)

  test("the answer type modified: the hole's program of an Int, the body's of a String"):
    type Q = Cont0[String, String, String]
    assertEquals(shift0[String, String, Int, Q, Int](k => k(5).map(_.toString)).map(_ * 2).reset0.value, "10")

  test("a body dropping k aborts the delimiter"):
    var ran = false
    assertEquals(shift0[Int, Int, Int, P, Int](_ => pure(-1)).map(x => { ran = true; x }).reset0.value, -1)
    assert(!ran)

  test("the body runs outside the delimiter: a shift0 in it reaches the next one out"):
    val inner: L = shift0[Int, Int, Int, L, Int](k1 => shift0[Int, Int, Int, P, Int](k2 => k1(1).flatMap(k2).flatMap(k2))).map(_ + 10).reset0
    assertEquals(inner.map(_ * 2).reset0.value, 44)
    val abort: L = shift0[Int, Int, Int, L, Int](_ => shift0[Int, Int, Int, P, Int](_ => pure(-1))).map(_ + 10).reset0
    assertEquals(abort.map(_ * 2).reset0.value, -1)

  test("shift is shift0 with its body under reset0"):
    val p: P = shift0[L, L, Int, P, Int](k => shift0[P, P, Int, P, Int](k2 => k2(1).map(_ * 100).reset0).flatMap(k).reset0).map(_ + 1).reset0
    assertEquals(p.value, 200)

  test("100 000 binds to the left and to the right, in constant stack"):
    assertEquals((1 to 100000).foldLeft(pure[Int, Int](0))((acc, _) => acc.map(_ + 1)).value, 100000)
    def right(n: Int): P = if n == 0 then pure(0) else pure[Int, Int](1).flatMap(x => right(n - 1).map(_ + x))
    assertEquals(right(100000).value, 100000)

  test("100 000 shift0 in a row, and k under 100 000 binds applied 3 times, in constant stack"):
    def shifts(i: Int, acc: Int): L =
      if i == 0 then pure(acc) else shift0[Int, Int, Int, P, Int](k => k(1)).flatMap(x => shifts(i - 1, acc + x + 1))
    assertEquals(shifts(100000, 0).reset0.value, 200000)
    type Lg = Cont0[Long, Long, Long]
    def calls(k: Int => Lg, i: Int, acc: Long): Lg = if i == 0 then pure(acc) else k(i).flatMap(r => calls(k, i - 1, acc + r * 10))
    var p: Cont0[Lg, Lg, Int] = shift0[Long, Long, Long, Lg, Int](k => calls(k, 3, 0))
    for _ <- 1 to 100000 do p = p.map(_ + 1)
    assertEquals(p.map(_.toLong).reset0.value, (100003L + 100002L + 100001L) * 10)

  test("k applied runs nothing of the rest: a generator's next step made, not taken"):
    var ran = false
    val p: P = shift0[Int, Int, Int, P, Int](k => { k(1); pure(if ran then 1 else 0) }).flatMap(x => { ran = true; pure(x) }).reset0
    assertEquals(p.value, 0)
