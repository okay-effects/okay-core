package okay.core

import okay.one.{Cont, Delim}
import okay.one.Cont.Shift0
import okay.one.row.Nearest
import okay.std.*
import okay.core.Control.{reset, shift, shift0}
import okay.one.std.{Reader as ReaderH, State as StateH}

/** CONTROL'S TWO FACES: `Shift % S`, shift and reset as an effect of the row — Danvy–Filinski's shift in a shift's
 * body, k lazy, k data; and `Control[R]`, the answer type modified — printf, nested delimiters */
class TestControl extends munit.FunSuite:
  type P = Pure
  type Sh[S] = Shifts[S, P] +: P

  test("a body dropping k aborts the reset: the rest never runs"):
    var ran = false
    assertEquals(run(reset[Int, P](shift[Int, P, Int](_ => pure(-1)).at[Sh[Int]].map(x => { ran = true; x }))), -1)
    assert(!ran)

  test("multi-shot: k resumed twice, the answers joined"):
    assertEquals(run(reset[List[Int], P](shift[List[Int], P, Int](k => k(1).flatMap(a => k(2).map(b => a ++ b))).at[Sh[List[Int]]]
      .map(x => List(x, x * 10)))), List(1, 10, 2, 20))

  test("a shift's body runs under the reset: a shift in it captures up to the same reset — Danvy–Filinski's"):
    assertEquals(run(reset[Int, P](shift[Int, P, Int](k => shift[Int, P, Int](k2 => k2(1).map(_ * 100)).at[Sh[Int]].flatMap(k)).at[Sh[Int]]
      .map(_ + 1))), 200)

  test("a shift captures up to the nearest reset only"):
    assertEquals(run(reset[Int, P](shift[Int, P, Int](k => k(1).map(_ * 100)).at[Sh[Int]]).flatMap(a =>
      reset[Int, P](shift[Int, P, Int](k => k(2).map(_ + 7)).at[Sh[Int]].map(_ + a)))), 109)

  test("k applied runs nothing of the rest: the rest runs when stepped"):
    var ran = false
    assertEquals(run(reset[Int, P](shift[Int, P, Int](k => { k(1); pure(if ran then 1 else 0) }).at[Sh[Int]].flatMap(x => { ran = true; pure(x) }))), 0)

  test("shift0: its body runs outside the reset, in its place"):
    // reset(shift0(k => k(1) + 1) * 10): k = _ * 10, the body outside the reset
    assertEquals(run(reset[Int, P](shift0[Int, P, Int](k => k(1).map(_ + 1)).at[Sh[Int]].map(_ * 10))), 11)

  test("100 000 resets and shifts in constant stack"):
    def many(n: Int, acc: Int): Eff[P, Int] =
      if n == 0 then pure(acc)
      else reset[Int, P](shift[Int, P, Int](k => k(1)).at[Sh[Int]].map(_ + 1)).flatMap(x => defer(many(n - 1, acc + x)))
    assertEquals(run(many(100000, 0)), 200000)

  test("k kept: shift0(k => pure(k)) gives k itself, resumed after the reset has answered"):
    enum Kept:
      case Value(v: Int)
      case Rest(k: Int => Eff[P, Kept])
    val p = shift0[Kept, P, Int](k => pure(Kept.Rest(k))).at[Shifts[Kept, P] +: P].map(x => Kept.Value(x * 2))
    run(reset[Kept, P](p)) match
      case Kept.Rest(k) => assertEquals(run(k(21)), Kept.Value(42))
      case v => fail(s"no k: $v")

  // -- the answer moved: under a `$`

  test("the answer type modified: printf — an Int hole, the $'s answer a function; an effect asked in it"):
    type R = Reader % Int +: Pure
    type K = Eff[R, String]
    type F = Int => K
    // `$ (ask >>= n => s"$n:" + shift0(k => pure(x => k(x))))`: the answer `String` turned `Int => K`
    val body: Cont[Delim[R], K, Eff[R, F], Int] =
      Shift0[Delim[R], R, Nothing, Nothing, String, Eff[R, F], Int](Nearest.Here(), k => Cont.Return((x: Int) => k(x)))
    val p: Eff[R, F] = Cont.dollar[R, K, Nothing, Nothing, F, String](
      ask[Int].at[Delim[R]].cont[Eff[R, F]].flatMap(n => body.map(x => s"$n:$x")))((s: String) => Cont.Return("got " + s))
    assertEquals(run(run(reader(1)(p))(42).handle(okay.one.std.Reader[Int, Pure, String](7))), "got 1:42")

  test("a handler over a $: its k taken past the $, the $ put back each time it is resumed"):
    type R = Choose +: Pure
    val p: Eff[R, Int] = Cont.dollar[R, Eff[R, Int], Nothing, Nothing, Int, Int](
      among(Seq(1, 2)).at[Delim[R]].cont[Eff[R, Int]].map(_ * 10))((a: Int) => Cont.Return(a + 1))
    assertEquals(run(choose(p)), Seq(11, 21))

  // -- the answer moved, the row not named: Atm

  test("Atm: ATM in a for of three lines, no row named — printf, the reader inside the $"):
    val body: Atm[String, Fixed[String], Hole[Int, String], Reader % Int] = for
      n <- ask[Int]
      x <- Atm.shift0[String, Int](k => pure((x: Int) => k(x)))
      m <- ask[Int]
    yield s"$n $x $m"
    val p: Eff[Pure, Int => Eff[Pure, String]] = Atm.dollar(handleAt(ReaderH(5))(body))(s => pure("<" + s + ">"))
    assertEquals(run(run(p)(42)), "<5 42 5>")

  test("Atm: a program needing effects bound in it, lifted; a state inside the $, k resumed twice from the state as it was"):
    def incr: Int ! State % Int = get[Int].flatMap(n => put(n + 1).map(_ => n))
    val body: Atm[String, Fixed[String], Fixed[String], State % Int] = for
      a <- incr
      x <- Atm.shift0[String, Int](k => k(1).flatMap(s => k(2).map(t => s + t)))
      b <- incr
    yield s"[$a $x $b]"
    val p: Eff[Pure, String] = Atm.dollar(handleAt(StateH(10))(body).map(_._2))(s => pure(s))
    assertEquals(run(p), "[10 1 11][10 2 11]")

  test("Atm: the reader outside the $, k passing it — the ask of the rest answered where k is run"):
    val body: Atm[String, Fixed[String], Hole[Int, String], Reader % Int] = for
      n <- ask[Int]
      x <- Atm.shift0[String, Int](k => pure((x: Int) => k(x)))
      m <- ask[Int]
    yield s"$n $x $m"
    type R = Reader % Int +: Pure
    val p: Eff[R, Int => Eff[R, String]] = Atm.dollar(body)(s => pure("<" + s + ">"))
    assertEquals(run(reader(5)(run(reader(1)(p))(42))), "<1 42 5>")
