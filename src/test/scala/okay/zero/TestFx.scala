package okay.zero

import okay.core.{Effects, Control, Cps}
import okay.core.Control.{reset, shift}
import okay.std.*

/** PROGRAMS WRITTEN ONCE, over master's `Effects` and `Control` and `okay.std`, run on each monad: `Cont`, the CPS
 * reference (control only) and `Cont0` (`Fx`) — the same answers */
class TestFx extends munit.FunSuite:

  def all[A](onCont: A, onZero: A, expected: A)(using munit.Location): Unit =
    assertEquals(onCont, expected, "on Cont")
    assertEquals(onZero, expected, "on Cont0")

  def twice(using c: Control): c.P[Int] =
    reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1))
  test("reset(1 + shift(k => k(k(10)))): 12 on each"):
    all(Effects.run(twice), Fx.run(twice), 12)
    assertEquals(Cps.run(twice), 12)

  def modified(using c: Control): c.P[String] =
    reset[Int](shift[Int, String](k => k(5).map(_.toString)).map(_ * 2))
  test("the answer type modified"):
    all(Effects.run(modified), Fx.run(modified), "10")

  def counting(n: Int)(using c: Effects, s: State % Int :? c.R): c.P[Int] =
    if n == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => counting(n - 1))
  test("state, 10 000 operations answered in place"):
    all(Effects.run(state(0)(counting(10000))), Fx.run(state(0)(counting(10000))), (10000, 10000))

  def telling(using c: Effects, w: Writer % String :? c.R, r: Reader % Int :? c.R): c.P[Int] =
    ask[Int].flatMap(x => tell(s"x=$x").map(_ => x * 2))
  test("reader and writer: a general clause, an operation across a handler"):
    all(Effects.run(reader(21)(writer[String, Int](telling))), Fx.run(reader(21)(writer[String, Int](telling))), (List("x=21"), 42))

  def raising(using c: Effects, t: Throws % String :? c.R): c.P[Int] =
    get0.flatMap(_ => raise[String, Int]("no"))
  def get0(using c: Effects): c.P[Int] = c.pure(0)
  test("throws: the clause drops the rest"):
    all(Effects.run(throws[String, Int](raising)), Fx.run(throws[String, Int](raising)), Left("no"))

  def paths(using c: Effects, ch: Choose :? c.R): c.P[Int] =
    among(Seq(1, 2)).flatMap(a => among(Seq(10, 20)).map(_ + a))
  test("choose: every path, multi-shot"):
    all(Effects.run(choose[Int](paths)), Fx.run(choose[Int](paths)), Vector(11, 21, 12, 22))

  def stateful(using c: Control): c.P[(String, Int)] =
    pstate(7)(st => st.get[Int].flatMap(n => st.put[Int]("x" * n)).flatMap(_ => st.get[String]).map(_.length))
  test("pstate: the state's type changed, by the answer type"):
    all(Effects.run(stateful), Fx.run(stateful), ("xxxxxxx", 7))
    assertEquals(Cps.run(stateful), ("xxxxxxx", 7))

  def shiftUnderReader(using c: Effects, r: Reader % Int :? c.R): c.P[Int] =
    reset[Int](shift[Int, Int](k => k(1).flatMap(k)).flatMap(x => ask[Int].map(_ + x)))
  test("control under an effect: a reset in a handler's body, ask in k"):
    all(Effects.run(reader(5)(shiftUnderReader)), Fx.run(reader(5)(shiftUnderReader)), 11)

  // WHERE THEY DIFFER, by design: a shift directly under a general handler. On Cont and on the reference `k` is the
  // rest under the handler's `ret` only, an operation in it the shift body's handler's — a raise in k aborts the
  // body; on Cont0's machine `k` is the rest WITH the handler, a raise in k that k's Left
  def kRaising(using c: Effects { type S = Either[String, Int] }, t: Throws % String :? c.R): c.M[c.R, Either[String, Int], Either[String, Int], Int] =
    shift[Int, Either[String, Int]](k =>
      k(1).flatMap(r1 => k(2).map(r2 => Right(List(r1, r2).map(_.fold(_.length, identity)).sum))))
      .flatMap(x => if x == 1 then raise[String, Int]("one") else c.pure(x * 10))
  test("a raise in k directly under throws: Cont and the reference abort the body; Cont0's k gets its Left"):
    def p(using c: Effects): c.P[Either[String, Int]] = okay.std.throws[String, Int](kRaising)
    assertEquals(Effects.run(p), Left("one"))
    assertEquals(FxRef.run(p), Left("one"))
    assertEquals(Fx.run(p), Right(3 + 20))
