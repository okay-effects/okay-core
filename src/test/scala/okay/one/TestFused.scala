package okay.one

import okay.one.Cont.*
import okay.one.Handlers.*
import okay.one.row.*
import okay.one.std.{Shift, Shifts}

/** maps fused where the program is made (`Cont.Fused`): one map, the functions composed, bounded — the values, the
 * stack, a throw and a shift as the maps one by one gave them */
class TestFused extends munit.FunSuite:
  test("100 000 maps of a value: fused by 32, the stack constant"):
    var p: Int ! Pure = pure(0)
    for _ <- 1 to 100000 do p = p.map(_ + 1)
    assertEquals(p.run, 100000)

  test("a map of a map: one map, its functions composed, at most MaxFused"):
    val p = defer(pure[Pure, Int](1)).map(_ + 1).map(_ * 2)
    p match
      case Map(_, f: Cont.Fused[?, ?, ?]) => assertEquals(f.n, 2)
      case other => fail(s"not fused: $other")
    var q: Int ! Pure = defer(pure(0))
    for _ <- 1 to 40 do q = q.map(_ + 1)
    q match
      case Map(Map(_, f: Cont.Fused[?, ?, ?]), g) =>
        assertEquals(f.n, Cont.MaxFused); assertEquals(Cont.fused(g), 40 - Cont.MaxFused)
      case other => fail(s"not two maps: $other")
    assertEquals(q.run, 40)

  test("a throw in the middle of a fused chain: out of the run, the maps after it not run"):
    var after = false
    val p = defer(pure[Pure, Int](1)).map(_ + 1).map(x => if x == 2 then throw IllegalStateException("two") else x)
      .map { x => after = true; x }
    intercept[IllegalStateException](p.run)
    assert(!after)

  test("a shift under fused maps: k the maps, each resumption all of them"):
    type R = Pure
    type SR = Shifts[Int, R] +: R
    var p: Int ! SR = Shift.shift[Int, R, Int, SR](k => k(1).flatMap(a => k(10).map(b => a + b)))
    for _ <- 1 to 50 do p = p.map(_ + 1)
    assertEquals(p.handle(Shift.reset[Int, R]).run, (1 + 50) + (10 + 50))
