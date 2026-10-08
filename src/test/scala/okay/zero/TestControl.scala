package okay.zero

/** CONTROL alone, beyond master's: Danvy–Filinski's shift in a shift's body, k lazy, shift0's hierarchy */
class TestControl extends munit.FunSuite:

  test("a body dropping k aborts the reset: the rest never runs"):
    var ran = false
    assertEquals(run[Int](reset[Int](shift[Int, Int](_ => pure(-1)).map(x => { ran = true; x }))), -1)
    assert(!ran)

  test("multi-shot: k resumed twice, the answers joined"):
    assertEquals(run[List[Int]](reset[List[Int]](shift[Int, List[Int]](k => k(1).flatMap(a => k(2).map(b => a ++ b))).map(x => List(x, x * 10)))),
      List(1, 10, 2, 20))

  test("a shift's body runs under the reset: a shift in it captures up to the same reset — Danvy–Filinski's"):
    assertEquals(run[Int](reset[Int](shift[Int, Int](k => shift[Int, Int](k2 => k2(1).map(_ * 100)).flatMap(k)).map(_ + 1))), 200)

  test("a shift captures up to the nearest reset only"):
    assertEquals(run[Int](reset[Int](shift[Int, Int](k => k(1).map(_ * 100))).flatMap(a => reset[Int](shift[Int, Int](k => k(2).map(_ + 7)).map(_ + a)))), 109)

  test("k applied runs nothing of the rest: the rest runs when stepped"):
    var ran = false
    assertEquals(run[Int](reset[Int](shift[Int, Int](k => { k(1); pure(if ran then 1 else 0) }).flatMap(x => { ran = true; pure(x) }))), 0)

  test("shift0: the body outside the reset — a shift0 in it reaches the next reset out"):
    // reset(reset(shift0 k1. let v = k1 1 in shift0 k2. k2(k2 v)) + 10) * 2): k1 = _ + 10, k2 = _ * 2 — 44. The outer
    // reset's context named for the inner body's shift0: the nearer reset's is in scope
    val r = run[Int](reset[Int] {
      val outer = summon[InReset[Empty, Int, Int, Int]]
      reset[Int](shift0[Int, Int](k1 => k1(1).flatMap(v => shift0[Int, Int].apply(using outer)(k2 => k2(v).flatMap(k2)))).map(_ + 10)).map(_ * 2)
    })
    assertEquals(r, 44)

  test("shift0 two deep, a body dropping both: both resets aborted"):
    val r = run[Int](reset[Int] {
      val outer = summon[InReset[Empty, Int, Int, Int]]
      reset[Int](shift0[Int, Int](_ => shift0[Int, Int].apply(using outer)(_ => pure(-1))).map(_ + 10)).map(_ * 2)
    })
    assertEquals(r, -1)

  test("100 000 resets and shifts in constant stack"):
    def many(n: Int, acc: Int)(using c: Ctx): c.P[Int] =
      if n == 0 then pure(acc) else reset[Int](shift[Int, Int](k => k(1)).map(_ + 1)).flatMap(x => many(n - 1, acc + x))
    assertEquals(run[Int](many(100000, 0)), 200000)
