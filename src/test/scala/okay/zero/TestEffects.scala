package okay.zero

import Cont0.*

/** effects and handlers with rows, a library on the four nodes: an operation a shift0 whose body is the request, a
 * handler `Done $ body` and the requests interpreted — the handler the one the program runs under */
class TestEffects extends munit.FunSuite:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]
  enum Choose[+A]:
    case Among[A](as: List[A]) extends Choose[A]

  def number[R <: Row, S](using Ctx[R, S], Ask :? R): Cont0[S, S, Int] = perform(Ask.Number)
  def say[R <: Row, S](s: String)(using Ctx[R, S], Say :? R): Cont0[S, S, Unit] = perform(Say.Line(s))
  def among[R <: Row, S, A](as: List[A])(using Ctx[R, S], Choose :? R): Cont0[S, S, A] = perform(Choose.Among(as))

  def asking[R <: Row, S, A](using Ctx[R, S])(n: Int)(body: Ctx[Ask +: R, Inner[Ask, R, S, A]] ?=> Cont0[Inner[Ask, R, S, A], Inner[Ask, R, S, A], A]): Cont0[S, S, A] =
    handle(new Handler[[X] =>> Cont0[S, S, X], Ask, A, A]:
      def ret(a: A): A = a
      def apply[X](op: Ask[X], k: X => Cont0[S, S, A]): Cont0[S, S, A] = op match
        case Ask.Number => k(n))(body)
  def saying[R <: Row, S, A](using Ctx[R, S])(body: Ctx[Say +: R, Inner[Say, R, S, A]] ?=> Cont0[Inner[Say, R, S, A], Inner[Say, R, S, A], A]): Cont0[S, S, (List[String], A)] =
    handle(new Handler[[X] =>> Cont0[S, S, X], Say, A, (List[String], A)]:
      def ret(a: A): (List[String], A) = (Nil, a)
      def apply[X](op: Say[X], k: X => Cont0[S, S, (List[String], A)]): Cont0[S, S, (List[String], A)] = op match
        case Say.Line(s) => k(()).map((log, a) => (s :: log, a)))(body)
  def choosing[R <: Row, S, A](using Ctx[R, S])(body: Ctx[Choose +: R, Inner[Choose, R, S, A]] ?=> Cont0[Inner[Choose, R, S, A], Inner[Choose, R, S, A], A]): Cont0[S, S, List[A]] =
    handle(new Handler[[X] =>> Cont0[S, S, X], Choose, A, List[A]]:
      def ret(a: A): List[A] = List(a)
      def apply[X](op: Choose[X], k: X => Cont0[S, S, List[A]]): Cont0[S, S, List[A]] = op match
        case Choose.Among(as) => as.foldLeft(pure[S, List[A]](Nil))((acc, x) => acc.flatMap(l => k(x).map(l ++ _))))(body)

  test("an operation answered by its handler"):
    assertEquals(run(asking(41)(number.map(_ + 1))), 42)

  def both[R <: Row, S](using Ctx[R, S], Ask :? R, Say :? R): Cont0[S, S, Int] = number.flatMap(n => say(n.toString).map(_ => n + 1))
  test("two effects by name, the handlers in either order"):
    assertEquals(run(saying(asking(41)(both))), (List("41"), 42))
    assertEquals(run(asking(41)(saying(both))), (List("41"), 42))

  def chosen[R <: Row, S](using Ctx[R, S], Choose :? R, Say :? R): Cont0[S, S, Int] =
    among(List(1, 2)).flatMap(x => say(s"x=$x").map(_ => x * 10))
  test("a say crosses choose; choose resumes say's handler with the rest, in each path"):
    assertEquals(run(saying(choosing(chosen))), (List("x=1", "x=2"), List(10, 20)))
    assertEquals(run(choosing(saying(chosen))), List((List("x=1"), 10), (List("x=2"), 20)))

  def loop[R <: Row, S](n: Int, acc: Int)(using Ctx[R, S], Ask :? R): Cont0[S, S, Int] =
    if n == 0 then pure(acc) else number.flatMap(x => loop(n - 1, acc + x))
  test("100 000 operations under a handler, and crossing two, in constant stack"):
    assertEquals(run(asking(1)(loop(100000, 0))), 100000)
    assertEquals(run(asking(1)(saying(saying(loop(100000, 0))))), (Nil, (Nil, 100000)))

  def says[R <: Row, S](n: Int)(using Ctx[R, S], Say :? R): Cont0[S, S, Int] =
    if n == 0 then pure(0) else say(n.toString).flatMap(_ => says(n - 1))
  test("100 000 says, each clause k(()).map(...) — the rest resumed under the maps pending outside"):
    val (log, _) = run(saying(says(100000)))
    assertEquals(log.length, 100000)

  test("a handler resuming twice under 10 000 binds: each path runs the whole rest"):
    def binds[R <: Row, S](using Ctx[R, S], Choose :? R): Cont0[S, S, Int] =
      (1 to 10000).foldLeft(among(List(1, 2)))((p, _) => p.flatMap(x => pure(x + 1)))
    assertEquals(run(choosing(binds)), List(10001, 10002))

  def deep[R <: Row, S](d: Int)(using Ctx[R, S], Ask :? R): Cont0[S, S, Int] =
    if d == 0 then loop(1000, 0) else saying(deep(d - 1)).map(_._2)
  test("1 000 operations, each crossing 500 handlers"):
    assertEquals(run(asking(1)(deep(500))), 1000)

  test("a program left its handler: the handler it runs under answers it"):
    var e: Option[Cont0[Inner[Ask, Empty, Int, Int], Inner[Ask, Empty, Int, Int], Int]] = None
    run(asking(1) { e = Some(number.map(_ * 10)); pure(0) })
    assertEquals(run(asking(2)(e.get)), 20)

  enum Emit[+A]:
    case Yield(w: Int) extends Emit[Unit]
  type S1 = Inner[Ask, Empty, Gen, Gen]
  /** a lazy generator: the element and the rest, a program inside a reader — whichever it is run under */
  enum Gen:
    case Next(w: Int, rest: Cont0[S1, S1, Gen])
    case Done
  def generating(using Ctx[Ask +: Empty, S1])(body: Ctx[Emit +: Ask +: Empty, Inner[Emit, Ask +: Empty, S1, Unit]] ?=> Cont0[Inner[Emit, Ask +: Empty, S1, Unit], Inner[Emit, Ask +: Empty, S1, Unit], Unit]): Cont0[S1, S1, Gen] =
    handle(new Handler[[X] =>> Cont0[S1, S1, X], Emit, Unit, Gen]:
      def ret(u: Unit): Gen = Gen.Done
      def apply[X](op: Emit[X], k: X => Cont0[S1, S1, Gen]): Cont0[S1, S1, Gen] = op match
        case Emit.Yield(w) => pure(Gen.Next(w, k(()))))(body)
  def yieldW[R <: Row, S](w: Int)(using Ctx[R, S], Emit :? R): Cont0[S, S, Unit] = perform(Emit.Yield(w))

  test("handlers are dynamic: a generator's rest asks the reader it is resumed under"):
    run(asking(7)(generating(number.flatMap(yieldW).flatMap(_ => number.flatMap(yieldW))))) match
      case Gen.Next(first, rest) =>
        assertEquals(first, 7)
        run(asking(8)(rest)) match
          case Gen.Next(second, _) => assertEquals(second, 8)
          case Gen.Done => fail("a second element")
      case Gen.Done => fail("a first element")

  test("an effect with no handler in force cannot be performed"):
    assert(compileErrors("""run[Int](asking(1)(say("x").map(_ => 1)))""").nonEmpty)
    assert(compileErrors("""run[Int](asking(1)(number))""").isEmpty)
