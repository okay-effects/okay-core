package okay.zero

// the rows and handlers, okay.core's: imported, not exported — Scala 2's TASTy reader fails on an export of a sealed trait
import okay.core.{Row, +:, Pure, In, :?, %, Sub, Req, Paths, Handler, Answering, Value}

import okay.core.{Effects, Control, Cps}
import okay.core.Control.{reset, shift}
import okay.std.*

/** PROGRAMS WRITTEN ONCE, over master's `Effects` and `Control` and `okay.std`, run on each monad: `Cont`, the CPS
 * reference (control only) and `Cont0` (`At`) — the same answers */
class TestMonads extends munit.FunSuite:

  def all[A](onCont: A, onZero: A, expected: A)(using munit.Location): Unit =
    assertEquals(onCont, expected, "on Cont")
    assertEquals(onZero, expected, "on Cont0")

  def twice(using c: Control): c.Prog[Int] =
    reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1))
  test("reset(1 + shift(k => k(k(10)))): 12 on each"):
    all(okay.cont.run(twice), run(twice), 12)
    assertEquals(Cps.run(twice), 12)

  def modified(using c: Control): c.Prog[String] =
    reset[Int](shift[Int, String](k => k(5).map(_.toString)).map(_ * 2))
  test("the answer type modified"):
    all(okay.cont.run(modified), run(modified), "10")

  def counting(n: Int)(using c: Effects, s: State % Int :? c.Effs): c.Prog[Int] =
    if n == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => counting(n - 1))
  test("state, 10 000 operations answered in place"):
    all(okay.cont.run(state(0)(counting(10000))), run(state(0)(counting(10000))), (10000, 10000))

  def telling(using c: Effects, w: Writer % String :? c.Effs, r: Reader % Int :? c.Effs): c.Prog[Int] =
    ask[Int].flatMap(x => tell(s"x=$x").map(_ => x * 2))
  test("reader and writer: a general clause, an operation across a handler"):
    all(okay.cont.run(reader(21)(writer[String, Int](telling))), run(reader(21)(writer[String, Int](telling))), (List("x=21"), 42))

  def raising(using c: Effects, t: Throws % String :? c.Effs): c.Prog[Int] =
    get0.flatMap(_ => raise[String, Int]("no"))
  def get0(using c: Effects): c.Prog[Int] = c.pure(0)
  test("throws: the clause drops the rest"):
    all(okay.cont.run(throws[String, Int](raising)), run(throws[String, Int](raising)), Left("no"))

  def paths(using c: Effects, ch: Choose :? c.Effs): c.Prog[Int] =
    among(Seq(1, 2)).flatMap(a => among(Seq(10, 20)).map(_ + a))
  test("choose: every path, multi-shot"):
    all(okay.cont.run(choose[Int](paths)), run(choose[Int](paths)), Vector(11, 21, 12, 22))

  def stateful(using c: Control): c.Prog[(String, Int)] =
    pstate(7)(st => st.get[Int].flatMap(n => st.put[Int]("x" * n)).flatMap(_ => st.get[String]).map(_.length))
  test("pstate: the state's type changed, by the answer type"):
    all(okay.cont.run(stateful), run(stateful), ("xxxxxxx", 7))
    assertEquals(Cps.run(stateful), ("xxxxxxx", 7))

  def shiftUnderReader(using c: Effects, r: Reader % Int :? c.Effs): c.Prog[Int] =
    reset[Int](shift[Int, Int](k => k(1).flatMap(k)).flatMap(x => ask[Int].map(_ + x)))
  test("control under an effect: a reset in a handler's body, ask in k"):
    all(okay.cont.run(reader(5)(shiftUnderReader)), run(reader(5)(shiftUnderReader)), 11)

  // WHERE THEY DIFFER, by design: a shift directly under a general handler. On Cont and on the reference `k` is the
  // rest under the handler's `ret` only, an operation in it the shift body's handler's — a raise in k aborts the
  // body; on Cont0's machine `k` is the rest WITH the handler, a raise in k that k's Left
  def kRaising(using c: Effects { type S = Either[String, Int] }, t: Throws % String :? c.Effs): c.M[Either[String, Int], Either[String, Int], Int] =
    shift[Int, Either[String, Int]](k =>
      k(1).flatMap(r1 => k(2).map(r2 => Right(List(r1, r2).map(_.fold(_.length, identity)).sum))))
      .flatMap(x => if x == 1 then raise[String, Int]("one") else c.pure(x * 10))
  test("a raise in k directly under throws: Cont and the reference abort the body; Cont0's k gets its Left"):
    def p(using c: Effects): c.Prog[Either[String, Int]] = okay.std.throws[String, Int](kRaising)
    assertEquals(okay.cont.run(p), Left("one"))
    assertEquals(Spec.run(p), Left("one"))
    assertEquals(okay.core.Cps0.run(p), Left("one"))
    assertEquals(run(p), Right(3 + 20))

  // a raw shift moving the answer, directly under a handler: on Cont0 no proof it keeps it — the machine's fallback,
  // the delimiter's definition (`bail`); the same answer on each
  def moved(using c: Effects { type S = Int }): c.Prog[Int] =
    c.shift[String, Int, Int](k => k(1).map(_.length), null)
      .flatMap(x => c.shift[Int, String, Int](k2 => k2(x + 1).map(n => "x" * n), null))
  test("a shift moving the answer and one moving it back, directly under a handler: the same on each"):
    def p(using c: Effects): c.Prog[Int] = reader[Int](5)(moved.flatMap(x => ask[Int].map(_ + x)))
    assertEquals(okay.cont.run(p), 7)
    assertEquals(run(p), 7)
    assertEquals(Spec.run(p), 7)
    assertEquals(okay.core.Cps0.run(p), 7)
