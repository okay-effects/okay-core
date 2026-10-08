package okay.zero.master

import okay.zero.*
import okay.zero.std.*

/** master's TestControlCps's programs on okay.zero — their answers master's, there agreed between Cont and CPS */
class TestControlCps extends munit.FunSuite:

  def twice(using c: Ctx): c.P[Int] = reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1))
  test("reset(1 + shift(k => k(k(10)))) is 12"):
    assertEquals(run[Int](twice), 12)

  def modified(using c: Ctx): c.P[String] = reset[Int](shift[Int, String](k => k(5).map(_.toString)).map(_ * 2))
  test("the answer type modified: an Int hole, a String answer"):
    assertEquals(run[String](modified), "10")

  def atTop(using c: Ctx { type T = Int }): Eff[c.R, c.J, Int, Int, Int] = shift[Int, Int](k => k(1).flatMap(k)).map(_ * 3)
  test("a shift at the top: the top is the delimiter"):
    assertEquals(run[Int](atTop), 9)

  def shots(using c: Ctx): c.P[List[Int]] =
    reset[List[Int]](shift[Int, List[Int]](k => k(1).flatMap(a => k(2).map(b => a ++ b))).map(x => List(x, x * 10)))
  test("multi-shot: k resumed twice, the answers joined"):
    assertEquals(run[List[Int]](shots), List(1, 10, 2, 20))

  def nested(using c: Ctx): c.P[Int] =
    reset[Int](shift[Int, Int](k => k(1).map(_ * 100)).flatMap(a => reset[Int](shift[Int, Int](k => k(2).map(_ + 7)).map(_ + a))))
  test("a shift captures up to the nearest reset only"):
    assertEquals(run[Int](nested), 1000)

  def stateful(using c: Ctx): c.P[(String, Int)] =
    pstate(7)(st => st.get[Int].flatMap(n => st.put[Int]("x" * n)).flatMap(_ => st.get[String]).map(_.length))
  test("pstate, library code over control: the state's type changed"):
    assertEquals(run[(String, Int)](stateful), ("xxxxxxx", 7))
