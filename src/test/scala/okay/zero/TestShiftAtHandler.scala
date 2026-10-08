package okay.zero

import okay.zero.std.*

/** A SHIFT UP TO A HANDLER: its body under the handler again, k the rest under the handler — an answering handler's
 * and another's alike, every program here run under both: a reader answering in place and the same reader as a
 * general clause */
class TestShiftAtHandler extends munit.FunSuite:

  /** the environment `e`, the body's value times `scale`: in place, or by a general clause */
  def env(general: Boolean, e: Int, scale: Int)(using c: Ctx)(body: c.Body[Reader % Int, Int, Int]): c.P[Int] =
    if general then handle(new Handler[c.P, Reader % Int, Int, Int]:
      def ret(a: Int): Int = a * scale
      def apply[X](op: Reader[Int, X], k: X => c.P[Int]): c.P[Int] = op match
        case Reader.Ask() => k(e))(body)
    else handle(new Answering[c.P, Reader % Int, Int, Int]:
      def ret(a: Int): Int = a * scale
      def value[X](op: Reader[Int, X]): X = op match
        case Reader.Ask() => e)(body)

  def both[A](name: String)(p: Boolean => A)(using munit.Location): Unit =
    test(name):
      assertEquals(p(false), p(true))

  test("k resumed twice, the handler's ret in each resumption: k(a) = (3 + a) * 10, the body's value as it is"):
    assertEquals(run(env(false, 3, 10)(shift[Int, Int](k => k(1).flatMap(a => ask[Int].flatMap(e => k(a + e))).map(_ + 100))
      .flatMap(x => ask[Int].map(_ + x)))), 560)

  both("k resumed twice, the handler's ret in each resumption, asks in the body and in k"):
    g => run(env(g, 3, 10)(shift[Int, Int](k => k(1).flatMap(a => ask[Int].flatMap(e => k(a + e))).map(_ + 100))
      .flatMap(x => ask[Int].map(_ + x))))

  both("tells in the body, in k, after the shift: the writer outside sees them in order"):
    g => run(writer[String, Int](env(g, 2, 1)(shift[Int, Int](k => tell("b1").flatMap(_ => k(1)).flatMap(a => tell("b2").flatMap(_ => k(a))))
      .flatMap(x => tell(s"x=$x").flatMap(_ => ask[Int]).map(_ * x)))))

  both("a shift in k resumed: nested, each up to the handler"):
    g => run(writer[String, Int](env(g, 5, 2)(shift[Int, Int](k => k(1).flatMap(k))
      .flatMap(x => tell(s"x=$x").flatMap(_ => shift[Int, Int](k2 => k2(x).flatMap(a => k2(a + 1)))).flatMap(y => ask[Int].map(_ + y))))))

  both("k dropped: the body's value the handler's answer, no ret"):
    g => run(writer[String, Int](env(g, 7, 3)(tell("a").flatMap(_ => shift[Int, Int](_ => ask[Int].map(_ + 1))).flatMap(x => tell("never").map(_ => x)))))

  both("a shift under a handler under a reset: the reset outside untouched"):
    g => run[Int](reset[Int](env(g, 4, 1)(shift[Int, Int](k => k(1).flatMap(k)).flatMap(x => ask[Int].map(_ + x))).map(_ * 2)))

  both("shifts deep: 10 000 resumptions, in constant stack"):
    g =>
      def loop(n: Int)(using c: Ctx { type T = Int }, r: Reader % Int :? c.R): c.P[Int] =
        if n == 0 then ask[Int] else shift[Int, Int](k => k(1)).flatMap(x => loop(n - 1).map(_ + x))
      run(env(g, 1, 1)(loop(10000)))

  test("100 000 shifts under a reset, each k resumed nested in its body, in constant stack"):
    def loop(n: Int)(using c: Ctx { type T = Int }): c.P[Int] =
      if n == 0 then pure(0) else shift[Int, Int](k => k(1).map(_ + 1)).flatMap(x => loop(n - 1).map(_ + x))
    assertEquals(run[Int](reset[Int](loop(100000))), 200000)

  both("100 000 shifts under a handler, each k resumed nested in its body, in constant stack"):
    g =>
      def loop(n: Int)(using c: Ctx { type T = Int }, r: Reader % Int :? c.R): c.P[Int] =
        if n == 0 then ask[Int] else shift[Int, Int](k => k(1).map(_ + 1)).flatMap(x => loop(n - 1).map(_ + x))
      run(env(g, 1, 1)(loop(100000)))

  // k under a handler: the rest up to it AND the handler — Danvy–Filinski's k = λx. handle(E[x]): an operation in k
  // is k's own handler's, its clause with the rest of k only

  test("a raise in k: that k's Left, the shift's body goes on"):
    val r = run(throws[String, Int](shift[Int, Either[String, Int]](k =>
      k(1).flatMap(r1 => k(2).map(r2 => Right(List(r1, r2).map(_.fold(_.length, identity)).sum))))
      .flatMap(x => if x == 1 then raise[String, Int]("one") else pure(x * 10))))
    assertEquals(r, Right(3 + 20))

  test("a tell in k: in k's answer, gone if the body drops it"):
    val r = run(writer[String, Int](shift[Int, (List[String], Int)](k => k(1).map((_, a) => (Nil, a + 1)))
      .flatMap(x => tell(s"x=$x").map(_ => x))))
    assertEquals(r, (Nil, 2))

  test("a choice in k: k's own list of results"):
    val r = run(choose[Int](shift[Int, Seq[Int]](k => k(1).flatMap(a => k(10).map(b => Seq(a.sum, b.sum))))
      .flatMap(x => among(Vector(0, 1)).map(_ + x))))
    assertEquals(r, Seq(1 + 2, 10 + 11))

  test("a shift moving the answer: up to a reset only — directly under a handler, a compile error"):
    assert(compileErrors("""run(reader[Int](1)(shift[Int, String](_ => pure("no")).map(_ => 1)))""").contains("Same"))
    assertEquals(run(reader[Int](1)(reset[Int](shift[Int, String](k => k(5).map(_.toString))).map(_.length))), 1)
