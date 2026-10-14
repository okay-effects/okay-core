package okay.one

import Cont.*
import okay.one.row.*

class TestCont0 extends munit.FunSuite:
  /** `n` `$`s, each in the rest its outer `$`'s shift0 resumed: an operation, then the next — each a row deeper, the
   * function one over any row */
  def nest[R <: Row](n: Int): Int ! R =
    if n == 0 then Return(0)
    else Dollar[R, Int ! R, Nothing, Nothing, Int, Int](
      Bind(Shift0[Delim[R], R, Nothing, Nothing, Int, Int ! R, Unit](Nearest.Here(), k => k(())),
        (_: Unit) => nest[Delim[R]](n - 1).cont[Int ! R]),
      a => Return(a + 1))

  test("stack safe: $s nested in the resumed rests") {
    assertEquals(nest[Pure](100000).run, 100000)
  }

  test("answer type modification: the shift0 turns the answer String into Int => String (printf)") {
    type S = Int => String
    // `$ ("Hello " + shift0(k => x => k(x)))`: the body's rest answers a String, the body a function — each a program
    // over the row outside the `$`, Pure: run as any program of effects
    val body: Cont[Reset, String ! Pure, S ! Pure, Int] =
      Shift0[Reset, Pure, Nothing, Nothing, String, S ! Pure, Int](Nearest.Here(), k => Return((x: Int) => k(x).run))
    val p: S ! Pure = Dollar[Pure, String ! Pure, Nothing, Nothing, S, String](
      Bind(body, (n: Int) => Return[Reset, String ! Pure, String](n.toString)),
      (s: String) => Return[Pure, Nothing, String]("Hello " + s))
    assertEquals(p.run(42), "Hello 42")
  }

  test("the same by shift0 and dollar: the path to the $ found, its row outside inferred") {
    type S = Int => String
    val p: S ! Pure = dollar(shift0[Pure, Nothing, Nothing, String, S ! Pure, Int, Reset](k =>
      Return((x: Int) => k(x).run)).map(_.toString))((s: String) => Return("Hello " + s))
    assertEquals(p.run(42), "Hello 42")
  }
