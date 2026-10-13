package okay.one

import okay.one.Cont0.{perform, pure, rows}
import okay.one.Handlers.*
import okay.one.row.*
import okay.one.std.{Shift, Shifts}
import okay.one.std.Shift.{reset, shift, shift0}

/** `Shift % S`, control as an effect: okay-core's `TestControl`, the shifts up to a `reset`, a `Deep` handler */
class TestShift extends munit.FunSuite:
  type Sh[S] = Shifts[S, Pure]
  type R[S] = Sh[S] +: Pure

  test("a body dropping k aborts the reset: the rest never runs") {
    var ran = false
    val p = shift[Int, Pure, Int, R[Int]](_ => pure(-1)).map(x => { ran = true; x })
    assertEquals(p.handle(reset).run, -1)
    assert(!ran)
  }

  test("multi-shot: k resumed twice, the answers joined") {
    type S = List[Int]
    val p = shift[S, Pure, Int, R[S]](k => k(1).flatMap(a => k(2).map(b => a ++ b))).map(x => List(x, x * 10))
    assertEquals(p.handle(reset).run, List(1, 10, 2, 20))
  }

  test("a shift's body runs under the reset: a shift in it captures up to the same reset — Danvy–Filinski's") {
    val p = shift[Int, Pure, Int, R[Int]](k =>
      shift[Int, Pure, Int, R[Int]](k2 => k2(1).map(_ * 100)).flatMap(k)).map(_ + 1)
    assertEquals(p.handle(reset).run, 200)
  }

  test("a shift captures up to the nearest reset only") {
    val p = shift[Int, Pure, Int, R[Int]](k => k(1).map(_ * 100)).handle(reset).flatMap(a =>
      shift[Int, Pure, Int, R[Int]](k => k(2).map(_ + 7)).map(_ + a).handle(reset))
    assertEquals(p.run, 109)
  }

  test("k applied runs nothing of the rest: the rest runs when stepped") {
    var ran = false
    val p = shift[Int, Pure, Int, R[Int]](k => { k(1); pure(if ran then 1 else 0) }).flatMap(x => { ran = true; pure(x) })
    assertEquals(p.handle(reset).run, 0)
  }

  test("100 000 resets and shifts in constant stack") {
    def many(n: Int, acc: Int): Int ! Pure =
      if n == 0 then pure(acc)
      else shift[Int, Pure, Int, R[Int]](k => k(1)).map(_ + 1).handle(reset).flatMap(x => Cont0.defer(many(n - 1, acc + x)))
    assertEquals(many(100000, 0).run, 200000)
  }

  test("a shift under a handler, the reset outside: k holds the handler, the asks in k its") {
    type T = Ask +: Pure
    type Q = Ask +: Shifts[Int, T] +: T
    // reset(reader(3) { x <- shift(k => k(1) + ask + 100 ...); ask + x }): the ask in the body the outer reader's, 7
    type B = Shifts[Int, T] +: T
    val inner = shift[Int, T, Int, Q](k => k(1).flatMap(a => perform[Ask, B, Int](Ask.Get).flatMap(e => k(a + e))).map(_ + 100))
      .flatMap(x => perform[Ask, Q, Int](Ask.Get).map(_ + x))
    val p = inner.handle(reader(3)).handle(reset).handle(reader(7))
    // k(y) = 3 + y under the inner reader: k(1) = 4, ask (outer) 7, k(11) = 14, + 100 = 114
    assertEquals(p.run, 114)
  }

  test("shift0(k => pure(k)): k itself the answer, resumed after the reset has answered") {
    val p = shift0[Kept, Pure, Int, Sh[Kept] +: Pure](k => pure(Kept.Rest(k))).map(x => Kept.Value(x * 2))
    p.handle(reset[Kept, Pure]).run match
      case Kept.Rest(k) => assertEquals(k(21).run, Kept.Value(42))
      case v => fail(s"no k: $v")
  }

/** an answer holding `k` */
enum Kept:
  case Value(v: Int)
  case Rest(k: Int => Kept ! Pure)
