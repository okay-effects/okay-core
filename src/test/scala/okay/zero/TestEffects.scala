package okay.zero

import Std.*

/** effects and handlers with rows, on the kernel: an operation a shift0 whose body is the request, a handler
 * `Done $ body` and the requests interpreted — the handler the one the program runs under */
class TestEffects extends munit.FunSuite:

  test("an operation answered by its handler"):
    assertEquals(run(asking(41)(number.map(_ + 1))), 42)

  def both[R <: Row, J, T](using Ctx[R, J], Ask :? R, Say :? R): Eff[R, J, T, T, Int] = number.flatMap(n => say(n.toString).map(_ => n + 1))
  test("two effects by name, the handlers in either order"):
    assertEquals(run(saying(asking(41)(both))), (List("41"), 42))
    assertEquals(run(asking(41)(saying(both))), (List("41"), 42))

  def chosen[R <: Row, J, T](using Ctx[R, J], Choose :? R, Say :? R): Eff[R, J, T, T, Int] =
    among(List(1, 2)).flatMap(x => say(s"x=$x").map(_ => x * 10))
  test("a say crosses choose; choose resumes say's handler with the rest, in each path"):
    assertEquals(run(saying(choosing(chosen))), (List("x=1", "x=2"), List(10, 20)))
    assertEquals(run(choosing(saying(chosen))), List((List("x=1"), 10), (List("x=2"), 20)))

  def stepped[R <: Row, J, T](using Ctx[R, J], State :? R, Choose :? R): Eff[R, J, T, T, Int] =
    among(List(1, 2)).flatMap(x => get.flatMap(s => put(s + x))).flatMap(_ => get)
  test("the order of handlers is the semantics: state per path under choose, one state through the paths over it"):
    assertEquals(run(choosing(stating(0)(stepped))), List((1, 1), (2, 2)))
    assertEquals(run(stating(0)(choosing(stepped))), (3, List(1, 3)))

  test("a clause that drops k aborts the body: the rest never runs"):
    var ran = false
    assertEquals(run(choosing(among(List.empty[Int]).map(x => { ran = true; x }))), Nil)
    assert(!ran)

  test("a clause runs outside its handler: an operation in it goes to the handler further out"):
    def loud[R <: Row, J, T, A](using Ctx[R, J], Say :? R)(body: Body[Ask, R, J, T, A, A]): Eff[R, J, T, T, A] =
      handle(new Handler[[X] =>> Eff[R, J, T, T, X], Ask, A, A]:
        def ret(a: A): A = a
        def apply[X](op: Ask[X], k: X => Eff[R, J, T, T, A]): Eff[R, J, T, T, A] = op match
          case Ask.Number => say("asked").flatMap(_ => k(7)))(body)
    assertEquals(run(saying(loud(number.flatMap(a => number.map(_ + a))))), (List("asked", "asked"), 14))

  test("two handlers of one effect: the nearer by default, the further by its path"):
    def outer[R <: Row, J, T](using c: Ctx[Ask +: Ask +: R, J]): Eff[Ask +: Ask +: R, J, T, T, Int] = perform(Ask.Number)(using c, In.There(In.Here()))
    assertEquals(run(asking(1)(asking(2)(number.flatMap(a => outer.map(b => a * 10 + b))))), 21)

  def loop[R <: Row, J, T](n: Int, acc: Int)(using Ctx[R, J], Ask :? R): Eff[R, J, T, T, Int] =
    if n == 0 then pure(acc) else number.flatMap(x => loop(n - 1, acc + x))
  test("100 000 operations under a handler, and crossing two, in constant stack"):
    assertEquals(run(asking(1)(loop(100000, 0))), 100000)
    assertEquals(run(asking(1)(saying(saying(loop(100000, 0))))), (Nil, (Nil, 100000)))

  def says[R <: Row, J, T](n: Int)(using Ctx[R, J], Say :? R): Eff[R, J, T, T, Int] =
    if n == 0 then pure(0) else say(n.toString).flatMap(_ => says(n - 1))
  test("100 000 says, each clause k(()).map(...) — the rest resumed under the maps pending outside"):
    assertEquals(run(saying(says(100000)))._1.length, 100000)

  test("a handler resuming twice under 10 000 binds: each path runs the whole rest"):
    def binds[R <: Row, J, T](using Ctx[R, J], Choose :? R): Eff[R, J, T, T, Int] =
      (1 to 10000).foldLeft(among[R, J, T, Int](List(1, 2)))((p, _) => p.flatMap(x => pure(x + 1)))
    assertEquals(run(choosing(binds)), List(10001, 10002))

  test("100 000 gets and puts of functional state, in constant stack"):
    def count[R <: Row, J, T](n: Int)(using Ctx[R, J], State :? R): Eff[R, J, T, T, Int] =
      if n == 0 then get else get.flatMap(s => put(s + 1)).flatMap(_ => count(n - 1))
    assertEquals(run(stating(0)(count(100000))), (100000, 100000))

  def deep[R <: Row, J, T](d: Int)(using Ctx[R, J], Ask :? R): Eff[R, J, T, T, Int] =
    if d == 0 then loop(1000, 0) else saying(deep(d - 1)).map(_._2)
  test("1 000 operations, each crossing 500 handlers"):
    assertEquals(run(asking(1)(deep(500))), 1000)

  test("a program left its handler: the handler it runs under answers it"):
    var e: Option[Eff[Ask +: Empty, Level[Empty, Int, Int], Int, Int, Int]] = None
    run(asking(1) { e = Some(number.map(_ * 10)); pure(0) })
    assertEquals(run(asking(2)(e.get)), 20)

  enum Emit[+A]:
    case Yield(w: Int) extends Emit[Unit]
  type J1 = Level[Empty, Gen, Gen]
  /** a lazy generator: the element and the rest, a program inside a reader — whichever it is run under */
  enum Gen:
    case Next(w: Int, rest: Eff[Ask +: Empty, J1, Gen, Gen, Gen])
    case Done
  def generating(using Ctx[Ask +: Empty, J1])(body: Body[Emit, Ask +: Empty, J1, Gen, Unit, Gen]): Eff[Ask +: Empty, J1, Gen, Gen, Gen] =
    handle(new Handler[[X] =>> Eff[Ask +: Empty, J1, Gen, Gen, X], Emit, Unit, Gen]:
      def ret(u: Unit): Gen = Gen.Done
      def apply[X](op: Emit[X], k: X => Eff[Ask +: Empty, J1, Gen, Gen, Gen]): Eff[Ask +: Empty, J1, Gen, Gen, Gen] = op match
        case Emit.Yield(w) => pure(Gen.Next(w, k(()))))(body)
  def yieldW[R <: Row, J, T](w: Int)(using Ctx[R, J], Emit :? R): Eff[R, J, T, T, Unit] = perform(Emit.Yield(w))

  test("handlers are dynamic: a generator's rest asks the reader it is resumed under"):
    run(asking(7)(generating(number.flatMap(yieldW).flatMap(_ => number.flatMap(yieldW))))) match
      case Gen.Next(first, rest) =>
        assertEquals(first, 7)
        run(asking(8)(rest)) match
          case Gen.Next(second, _) => assertEquals(second, 8)
          case Gen.Done => fail("a second element")
      case Gen.Done => fail("a first element")
