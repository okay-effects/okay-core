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

  test("a bind of a map: one bind, the map's function before the rest"):
    val p = defer(pure[Pure, Int](2)).map(_ * 10).flatMap(x => pure(x + 1))
    p match
      case Bind(_, _: Cont.Fused[?, ?, ?]) => ()
      case other => fail(s"not one bind: $other")
    assertEquals(p.run, 21)

  test("an operation mapped twice: its MapK the functions fused; k resumed twice by choose, each its answer"):
    type CR = Choose +: Pure
    val p = perform[Choose, CR, Boolean](Choose.Flip).map(b => if b then 1 else 2).map(_ * 10)
    p match
      case InjectHere(_, mk: Cont.MapK[?, ?, ?, ?, ?]) => assert(mk.f.isInstanceOf[Cont.Fused[?, ?, ?]])
      case other => fail(s"not one MapK: $other")
    assertEquals(p.handle(choose).run, List(10, 20))

