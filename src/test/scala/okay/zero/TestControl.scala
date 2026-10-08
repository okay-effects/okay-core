package okay.zero

/** CONTROL alone: shift and reset with the answers modified, no effect anywhere — in any row */
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

  test("a shift's body runs under the reset: a shift in it captures up to the same reset — Danvy–Filinski's"):
    assertEquals(run[Int](reset(shift[Int, Int, Int](k => shift[Int, Int, Int](k2 => k2(1).map(_ * 100)).flatMap(k)).map(_ + 1))), 200)

  test("a shift at the top: the top is a delimiter"):
    assertEquals(run[Int](shift[Int, Int, Int](k => k(1).flatMap(k)).map(_ * 3)), 9)

  test("k applied runs nothing of the rest: the rest runs when stepped"):
    var ran = false
    assertEquals(run[Int](reset(shift[Int, Int, Int](k => { k(1); pure(if ran then 1 else 0) }).flatMap(x => { ran = true; pure(x) }))), 0)

  test("shift0: the body outside the reset — a shift0 in it reaches the next reset out, k1 and k2 both contexts"):
    // reset(reset(shift0 k1. let v = k1 1 in shift0 k2. k2(k2 v)) + 10) * 2): k1 = _ + 10, k2 = _ * 2 — 44
    // the inner body's shift0 is to the outer reset: its context named, the nearer reset's being in scope
    def inner[R <: Row, J, T](using outer: Reset[R, J, T]): Eff[R, Level[R, J, T], Int, Int, Int] =
      resetAt[Int](shift0[Int, Int, Int](k1 => k1(1).flatMap(v => shift0[Int, Int, Int].apply(using outer)(k2 => k2(v).flatMap(k2)))).map(_ + 10))
    assertEquals(run[Int](reset(inner.map(_ * 2))), 44)

  test("shift0 two deep, a body dropping both: both resets aborted"):
    def inner[R <: Row, J, T](using outer: Reset[R, J, T]): Eff[R, Level[R, J, T], Int, Int, Int] =
      resetAt[Int](shift0[Int, Int, Int](_ => shift0[Int, Int, Int].apply(using outer)(_ => pure(-1))).map(_ + 10))
    assertEquals(run[Int](reset(inner.map(_ * 2))), -1)

  test("a shift captures up to the nearest reset only"):
    assertEquals(run[Int](resetAt[Int](shift[Int, Int, Int](k => k(1).map(_ * 100))).flatMap(a => reset(shift[Int, Int, Int](k => k(2).map(_ + 7)).map(_ + a)))), 109)
