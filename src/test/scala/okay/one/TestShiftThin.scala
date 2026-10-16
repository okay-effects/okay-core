package okay.one

import okay.one.Cont.*
import okay.one.Handlers.*
import okay.one.row.*
import okay.one.std.{Shift, Shifts}

/** a shift's `k` in its body put back as ONE level (control-thin, `UnderViewed`): its value, an operation of the row
 * outside found past it, a shift inside it, a throw in it — the answers a view and a resume gave */
class TestShiftThin extends munit.FunSuite:
  type R = Ask +: Pure
  type SR = Shifts[Int, R] +: R
  def ask: Int ! SR = perform[Ask, SR, Int](Ask.Get)
  def shift(body: (Int => Int ! SR) => Int ! SR): Int ! SR = Shift.shift[Int, R, Int, SR](body)
  def twice(k: Int => Int ! SR, a: Int, b: Int): Int ! SR = k(a).flatMap(x => k(b).map(y => x + y))

  test("k twice in the body: each its answer, summed") {
    val p = shift(k => twice(k, 1, 2)).map(_ * 10)
    assertEquals(p.handle(Shift.reset[Int, R]).handle(reader(5)).run, 30)
  }

  test("an operation of the row outside, inside k: found past the merged level") {
    val p = shift(k => twice(k, 1, 2)).flatMap(x => ask.map(n => x * n))
    assertEquals(p.handle(Shift.reset[Int, R]).handle(reader(5)).run, 15)
  }

  test("a shift inside k: the merged level itself taken into a k, put back") {
    val p = shift(k => twice(k, 1, 10)).flatMap(x => shift(k2 => twice(k2, x, x + 1)))
    assertEquals(p.handle(Shift.reset[Int, R]).handle(reader(5)).run, 24)
  }

  test("a throw inside k: out of the run") {
    val p = shift(k => twice(k, 1, 2)).map(x => if x == 2 then throw IllegalStateException("two") else x)
    intercept[IllegalStateException](p.handle(Shift.reset[Int, R]).handle(reader(5)).run)
  }

  test("k resumed after its body's other operations: the reader's value through the level each time") {
    val p = shift(k => ask.flatMap(n => twice(k, n, n + 1))).flatMap(x => ask.map(_ + x))
    assertEquals(p.handle(Shift.reset[Int, R]).handle(reader(5)).run, (5 + 5) + (5 + 6))
  }
