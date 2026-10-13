package okay.core

import okay.std.*
import okay.core.Control.{reset, shift}

/** `A ! E1 + E2`: a program needing effects, fitted to the place it runs at — the row the handlers' */
class TestContext extends munit.FunSuite:

  def inc: Int ! Reader % Int = ask[Int].map(_ + 1)
  def twice: Int ! Reader % Int = inc.flatMap(a => inc.map(_ + a))

  test("A ! E: a program over any row with E, under its handler alone or under others"):
    assertEquals(run(reader(5)(twice)), 12)
    assertEquals(run(writer[String, Int, Pure](reader(5)(twice))), (Nil, 12))
    assertEquals(run(reader(5)(writer[String, Int, Reader % Int +: Pure](twice))), (Nil, 12))

  test("A ! E: an effect not declared is not in reach"):
    assert(compileErrors("""def both: Int ! Reader % Int = ask[Int].flatMap(a => tell("x").map(_ => a))""").nonEmpty)

  def both: Int ! Reader % Int + Writer % String = ask[Int].flatMap(a => tell(a.toString).map(_ => a + 1))
  def both2: Int ! Reader % Int + Writer % String = both.flatMap(a => both.map(_ + a))

  test("A ! E1 + E2: a program over several effects, under their handlers in any order, among others"):
    assertEquals(run(writer[String, Int, Pure](reader(5)(both2))), (List("5", "5"), 12))
    assertEquals(run(reader(5)(writer[String, Int, Reader % Int +: Pure](both2))), (List("5", "5"), 12))
    assertEquals(run(state(0)(reader(5)(writer[String, Int, Reader % Int +: State % Int +: Pure](both2))))._2, (List("5", "5"), 12))
    // a program of one of the effects, inside
    def mixed: Int ! Reader % Int + Writer % String = twice.flatMap(a => both.map(_ + a))
    assertEquals(run(writer[String, Int, Pure](reader(5)(mixed))), (List("5"), 18))

  test("A ! E1 + E2: the effects needed joined, in any order, the place's row any"):
    def plus: Int ! Writer % String + Reader % Int = ask[Int].flatMap(a => tell(a.toString).map(_ => a + 1))
    def plus3: Int ! State % Int + Writer % String + Reader % Int = plus.flatMap(a => get[Int].map(_ + a))
    def same: Int ! Reader % Int + Writer % String = plus
    assertEquals(run(reader(5)(writer[String, Int, Reader % Int +: Pure](plus))), (List("5"), 6))
    assertEquals(run(writer[String, Int, Pure](reader(5)(same))), (List("5"), 6))
    assertEquals(run(state(10)(writer[String, Int, State % Int +: Pure](reader(5)(plus3))))._2, (List("5"), 16))

  test("A ! E1 + E2: effects joined by +, not a row — a row is the handlers' business"):
    assert(compileErrors("""def both: Int ! Reader % Int +: Writer % String +: Pure = ask[Int]""").nonEmpty)

  test("a program over a row named, Eff[R, A], called from one needing its effects — and the other way round"):
    def named[R <: Row: Has[Reader % Int]]: Eff[R, Int] = ask[Int].at[R].map(_ * 2)
    def needing: Int ! Reader % Int + Writer % String = named.flatMap(a => tell(a.toString).map(_ => a))
    assertEquals(run(writer[String, Int, Pure](reader(5)(needing))), (List("10"), 10))
    given Place[Reader % Int +: Pure] = Place.at
    val p: Eff[Reader % Int +: Pure, Int] = inc
    assertEquals(run(reader(1)(p)), 2)

  // -- control as an effect of the row: shift and reset under handlers, in any order

  test("reset(1 + shift(k => k(k(10)))) is 12"):
    assertEquals(run(reset[Int, Pure](shift[Int, Pure, Int](k => k(10).flatMap(k)).map(_ + 1))), 12)

  test("a shift's body is written under the reset, among the effects in force there: it may perform"):
    type T = Reader % Int +: Pure
    val r = run(reader(3)(reset[Int, T](shift[Int, T, Int](k => ask[Int].at[Shifts[Int, T] +: T].flatMap(k)).map(_ * 10))))
    assertEquals(r, 30)

  test("an operation under a handler, under a reset: k holds the handler, which still answers"):
    type T = Writer % String +: Pure
    val r = run(writer[String, Int, Pure](reset[Int, T](shift[Int, T, Int](k => k(1).flatMap(k))
      .flatMap(x => tell(x.toString).at[Shifts[Int, T] +: T].map(_ => x + 1)))))
    assertEquals(r, (List("1", "2"), 3))

  test("a shift under a handler, the reset outside: k is the rest up to the reset, the handler in it; two shots"):
    type T = Pure
    type Q = Writer % String +: Shifts[(List[String], Int), T] +: T
    val r = run(reset[(List[String], Int), T](writer[String, Int, Shifts[(List[String], Int), T] +: T](
      shift[(List[String], Int), T, Int](k => k(1).flatMap((l1, a1) => k(2).map((l2, a2) => (l1 ++ l2, a1 + a2)))).at[Q]
        .flatMap(x => tell(x.toString).at[Q].map(_ => x * 10)))))
    assertEquals(r, (List("1", "2"), 30))
