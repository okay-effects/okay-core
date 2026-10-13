package okay.one

import Cont0.*
import okay.one.row.Pure

class TestCont0 extends munit.FunSuite:
  /** `n` handlers, each installed in the rest its outer handler resumed: an operation, then the next */
  def nest[R](n: Int): Cont[R, Int] =
    type L = Cont[R, Int]
    if n == 0 then Return(0)
    else Dollar[Pure, L, R, R, Int, Int](
      Bind(Shift0[Pure, R, R, Int, L, Unit](k => k(())), (_: Unit) => nest[L](n - 1)),
      a => Return(a + 1))

  test("stack safe: handlers nested in the resumed rests") {
    assertEquals(nest[Int](100000).run(identity), 100000)
  }

  test("answer type modification: the shift0 turns the answer String into Int => String (printf)") {
    type S = Int => String
    // `$ ("Hello " + shift0(k => x => k(x)))`: the body's rest answers `Cont[String, String]`, the body `Cont[S, S]`
    val body: Cont0[Control[Pure], Cont[String, String], Cont[S, S], Int] =
      Shift0[Pure, String, String, String, Cont[S, S], Int](k => Return((x: Int) => k(x).run(identity)))
    val p: Cont[S, S] = Dollar(Bind(body, (n: Int) => Return[Control[Pure], Cont[String, String], String](n.toString)),
      (s: String) => Return[Control[Pure], String, String]("Hello " + s))
    assertEquals(p.run(identity)(42), "Hello 42")
  }

