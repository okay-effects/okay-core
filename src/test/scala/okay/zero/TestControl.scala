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

  test("a shift captures up to the nearest reset only"):
    assertEquals(run[Int](resetAt[Int](shift[Int, Int, Int](k => k(1).map(_ * 100))).flatMap(a => reset(shift[Int, Int, Int](k => k(2).map(_ + 7)).map(_ + a)))), 109)
