package okay.zero

import okay.zero.std.*

/** A SHIFT UP TO A HANDLER: an answering handler's taken by the machine, another's by the reading — the same
 * answers, every program here run under both: a reader answering in place and the same reader as a general clause */
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
