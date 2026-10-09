package okay.zero

// the rows and handlers, okay.core's: imported, not exported — Scala 2's TASTy reader fails on an export of a sealed trait
import okay.core.{Row, +:, Pure, In, :?, %, Sub, Req, Paths, Handler, Answering, Value}

import okay.core.{Effects, pure}
import okay.core.Control.{reset, shift}
import okay.std.*

/** WHAT IS Cont0's OWN, through master's interface: shift0, a program kept as a value of `Eff` */
class TestZero extends munit.FunSuite:

  test("shift0: the body outside the reset — a shift0 in it reaches the next reset out"):
    // reset(reset(shift0 k1. let v = k1 1 in shift0 k2. k2(k2 v)) + 10) * 2): k1 = _ + 10, k2 = _ * 2 — 44. The outer
    // reset's context named for the inner body's shift0: the nearer reset's is in scope
    val r = run[Int](reset[Int] {
      val outer = summon[At[Pure, Level[Pure, Int, Int], Int]]
      reset[Int](shift0[Int, Int](k1 => k1(1).flatMap(v => shift0[Int, Int].apply(using outer)(k2 => k2(v).flatMap(k2)))).map(_ + 10)).map(_ * 2)
    })
    assertEquals(r, 44)

  test("shift0 two deep, a body dropping both: both resets aborted"):
    val r = run[Int](reset[Int] {
      val outer = summon[At[Pure, Level[Pure, Int, Int], Int]]
      reset[Int](shift0[Int, Int](_ => shift0[Int, Int].apply(using outer)(_ => pure(-1))).map(_ + 10)).map(_ * 2)
    })
    assertEquals(r, -1)

  test("a program left its handler: the handler it runs under answers it"):
    var e: Option[Eff[Reader % Int +: Pure, Level[Pure, Int, Int], Int, Int, Int]] = None
    run(reader[Int](1) { e = Some(ask[Int].map(_ * 10)); pure(0) })
    assertEquals(run(reader[Int](2)(e.get)), 20)

