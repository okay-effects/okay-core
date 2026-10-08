package okay.zero.master

import okay.zero.*
import okay.zero.std.*

/** master's TestEffects on okay.zero: the effects in context, the syntax and the answer never named */
class TestEffects extends munit.FunSuite:

  test("reader: ask answered in place"):
    assertEquals(run(reader[Int](41)(ask[Int].map(_ + 1))), 42)

  test("writer: the log in order, and the value"):
    assertEquals(run(writer[String, Int](tell("a").flatMap(_ => tell("b")).map(_ => 1))), (List("a", "b"), 1))

  test("reader and writer, the handlers in either order: a program needs only its capabilities"):
    def prog(using c: Ctx, r: Reader % Int :? c.R, w: Writer % String :? c.R): c.P[Int] =
      ask[Int].flatMap(n => tell(n.toString).map(_ => n + 1))
    assertEquals(run(writer[String, Int](reader[Int](41)(prog))), (List("41"), 42))
    assertEquals(run(reader[(List[String], Int)](41)(writer[String, Int](prog))), (List("41"), 42))

  test("state: get, put, modify; the last state and the value"):
    val r = run:
      state[String](1):
        for
          a <- get[Int]
          _ <- put(a + 1)
          b <- modify[Int](_ * 10)
        yield s"$a,$b"
    assertEquals(r, (20, "1,20"))

  test("100 000 operations answered in place, and forwarded through a handler, in constant stack"):
    def loop(i: Int)(using c: Ctx, s: State % Int :? c.R): c.P[Int] =
      if i == 0 then get[Int] else modify[Int](_ + 1).flatMap(_ => loop(i - 1))
    assertEquals(run(state[Int](0)(loop(100000))), (100000, 100000))
    def asks(i: Int, acc: Int)(using c: Ctx, r: Reader % Int :? c.R): c.P[Int] =
      if i == 0 then pure(acc) else ask[Int].flatMap(x => asks(i - 1, acc + x))
    assertEquals(run(reader[(List[String], Int)](1)(collect[String, Int](asks(100000, 0)))), (Nil, 100000))

  test("an operation through 16 handlers of other effects, and back, 10 000 times"):
    def loop(i: Int, acc: Int)(using c: Ctx, r: Reader % Int :? c.R): c.P[Int] =
      if i == 0 then pure(acc) else ask[Int].flatMap(x => loop(i - 1, acc + x))
    def nested(d: Int)(using c: Ctx, r: Reader % Int :? c.R): c.P[Int] =
      if d == 0 then loop(10000, 0) else reader[Int](0.5)(nested(d - 1))
    assertEquals(run(reader[Int](2)(nested(16))), 20000)

  test("state: the cell is per run — the same program run twice starts from s0 twice"):
    var p: Option[Eff[Empty, (Int, Int), (Int, Int), (Int, Int), (Int, Int)]] = None
    run[(Int, Int)] { p = Some(state[Int](0)(modify[Int](_ + 1))); pure((0, 0)) }
    assertEquals(run(p.get), (1, 1))
    assertEquals(run(p.get), (1, 1))

  test("two instances of one effect, State[Int] and State[String]: each goes to its own handler, by its path"):
    def body(using c: Ctx, i: State % Int :? c.R, s: State % String :? c.R): c.P[Unit] =
      modify[Int](_ + 1).flatMap(n => modify[String](_ + n)).map(_ => ())
    assertEquals(run(state[(Int, Unit)]("x")(state[Unit](41)(body))), ("x42", (42, ())))
    assertEquals(run(state[(String, Unit)](41)(state[Unit]("x")(body))), (42, ("x42", ())))

  test("throws: raise aborts the body, Left; no raise, Right"):
    assertEquals(run(throws[String, Int](raise[String, Int]("boom").map(_ + 1))), Left("boom"))
    assertEquals(run(throws[String, Int](pure(1).map(_ + 1))), Right(2))

  test("state and throws: the order of handlers is the semantics — state kept or lost on a failure"):
    def body(using c: Ctx, s: State % Int :? c.R, t: Throws % String :? c.R): c.P[Int] =
      put(5).flatMap(_ => raise[String, Int]("boom"))
    assertEquals(run(state[Either[String, Int]](0)(throws[String, Int](body))), (5, Left("boom")))
    assertEquals(run(throws[String, (Int, Int)](state[Int](0)(body))), Left("boom"))

  test("choose: every path, in order — multi-shot"):
    val r = run:
      choose[(Int, Int)]:
        for
          x <- among(Seq(1, 2, 3))
          y <- among(Seq(10, 20))
        yield (x, y)
    assertEquals(r, Seq((1, 10), (1, 20), (2, 10), (2, 20), (3, 10), (3, 20)))

  test("choose among 10 000: every path, in order"):
    assertEquals(run(choose[Int](among(0 until 10000).map(_ * 2))), (0 until 10000).map(_ * 2))

  test("8 queens by choose: 92 solutions"):
    def safe(col: Int, placed: List[Int]) = placed.zipWithIndex.forall((c, d) => c != col && math.abs(c - col) != d + 1)
    def place(row: Int, placed: List[Int])(using c: Ctx, ch: Choose :? c.R): c.P[List[Int]] =
      if row == 8 then pure(placed) else among((0 until 8).filter(safe(_, placed))).flatMap(col => place(row + 1, col :: placed))
    val all = run(choose[List[Int]](place(0, Nil)))
    assertEquals(all.length, 92)
    assertEquals(all.head, List(3, 1, 6, 2, 5, 7, 4, 0))

  test("choose with an empty choice prunes the path"):
    assertEquals(run(choose[Int](among(Seq(1, 2)).flatMap(x => among(if x == 1 then Seq.empty else Seq(x))))), Seq(2))

  test("choose and state: a resumption shares the cell — the second path sees the first's last state"):
    assertEquals(run(state[Seq[Int]](0)(choose[Int](among(Seq(1, 2)).flatMap(x => modify[Int](_ + x))))), (3, Seq(1, 3)))

  test("collect: every element yielded, and the value"):
    assertEquals(run(collect[Int, String](yield_(1).flatMap(_ => yield_(2)).map(_ => "done"))), (List(1, 2), "done"))

  test("collect: the buffer is per run — the same program run twice collects from empty twice"):
    var p: Option[Eff[Empty, (List[Int], Unit), (List[Int], Unit), (List[Int], Unit), (List[Int], Unit)]] = None
    run[(List[Int], Unit)] { p = Some(collect[Int, Unit](yield_(1).flatMap(_ => yield_(2)))); pure((Nil, ())) }
    assertEquals(run(p.get), (List(1, 2), ()))
    assertEquals(run(p.get), (List(1, 2), ()))

  test("collect and choose: a resumption shares the buffer — both paths' elements, in the order they ran"):
    assertEquals(run(collect[Int, Seq[Int]](choose[Int](among(Seq(1, 2)).flatMap(x => yield_(x).map(_ => x * 10))))), (List(1, 2), Seq(10, 20)))

  // the generators: a consumer's own type each — the rest a program of the level the generator runs at
  enum G1:
    case Next(w: Int, rest: Eff[Empty, G1, G1, G1, G1])
    case Done
  enum G2:
    case Next(w: Int, rest: Eff[Reader % Int +: Empty, Level[Empty, G2, G2], G2, G2, G2])
    case Done
  enum G3:
    case Next(w: Long, rest: Eff[Empty, G3, G3, G3, G3])
    case Done

  test("generate is lazy: an infinite body, three elements pulled, nothing past them runs"):
    var produced = 0
    def from(n: Int)(using c: Ctx, e: Emit % Int :? c.R): c.P[Unit] = yield_(n).flatMap(_ => { produced += 1; from(n + 1) })
    var gen: Option[Eff[Empty, G1, G1, G1, G1]] = None
    run[G1] { gen = Some(generate[Int, G1](G1.Next(_, _), G1.Done)(from(0))); pure(G1.Done) }
    def take(n: Int, g: Eff[Empty, G1, G1, G1, G1]): List[Int] = if n == 0 then Nil else run(g) match
      case G1.Done => Nil
      case G1.Next(w, rest) => w :: take(n - 1, rest)
    assertEquals(take(3, gen.get), List(0, 1, 2))
    assertEquals(produced, 2)

  test("generate over a remaining effect: the rest of the generator still asks, under the handler it is run in"):
    type F = Reader % Int +: Empty
    def all(g: Eff[F, Level[Empty, G2, G2], G2, G2, G2]): List[Int] = run(reader[G2](7)(g)) match
      case G2.Done => Nil
      case G2.Next(w, rest) => w :: all(rest)
    var gen: Option[Eff[F, Level[Empty, G2, G2], G2, G2, G2]] = None
    run[G2](reader[G2](7) { gen = Some(generate[Int, G2](G2.Next(_, _), G2.Done)(ask[Int].flatMap(n => yield_(n)).flatMap(_ => ask[Int].flatMap(n => yield_(n * 2))))); pure(G2.Done) })
    assertEquals(all(gen.get), List(7, 14))

  test("Fibonacci: collected and generated agree, the generator infinite, and 10 000 of them in constant stack"):
    def fibs(i: Int, a: Long, b: Long)(using c: Ctx, e: Emit % Long :? c.R): c.P[Unit] =
      if i == 0 then pure(()) else yield_(a).flatMap(_ => fibs(i - 1, b, a + b))
    def forever(a: Long, b: Long)(using c: Ctx, e: Emit % Long :? c.R): c.P[Unit] = yield_(a).flatMap(_ => forever(b, a + b))
    def take(n: Int, g: Eff[Empty, G3, G3, G3, G3]): List[Long] = if n == 0 then Nil else run(g) match
      case G3.Done => Nil
      case G3.Next(w, rest) => w :: take(n - 1, rest)
    val first = List(0L, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    assertEquals(run(collect[Long, Unit](fibs(10, 0, 1)))._1, first)
    var gen: Option[Eff[Empty, G3, G3, G3, G3]] = None
    run[G3] { gen = Some(generate[Long, G3](G3.Next(_, _), G3.Done)(forever(0, 1))); pure(G3.Done) }
    assertEquals(take(10, gen.get), first)
    assertEquals(run(collect[Long, Unit](fibs(10000, 0, 1)))._1.length, 10000)

  test("generate stays lazy over a forwarded effect: an infinite body asking at every step, three pulled"):
    type F = Reader % Int +: Empty
    var produced = 0
    def from(n: Int)(using c: Ctx, e: Emit % Int :? c.R, r: Reader % Int :? c.R): c.P[Unit] =
      ask[Int].flatMap(x => yield_(n * x)).flatMap(_ => { produced += 1; from(n + 1) })
    def take(i: Int, g: Eff[F, Level[Empty, G2, G2], G2, G2, G2]): List[Int] = if i == 0 then Nil else run(reader[G2](10)(g)) match
      case G2.Done => Nil
      case G2.Next(w, rest) => w :: take(i - 1, rest)
    var gen: Option[Eff[F, Level[Empty, G2, G2], G2, G2, G2]] = None
    run[G2](reader[G2](10) { gen = Some(generate[Int, G2](G2.Next(_, _), G2.Done)(from(1))); pure(G2.Done) })
    assertEquals(take(3, gen.get), List(10, 20, 30))
    assertEquals(produced, 2)

  test("handlers are dynamic: a generator's rest asks the reader it is run under, not the one it was built under"):
    // built under reader(7): its rest is still a program over Reader, and the consumer runs it under reader(8)
    run(reader[G2](7)(generate[Int, G2](G2.Next(_, _), G2.Done)(ask[Int].flatMap(yield_(_)).flatMap(_ => ask[Int].flatMap(yield_(_)))))) match
      case G2.Next(first, rest) =>
        assertEquals(first, 7)
        run(reader[G2](8)(rest)) match
          case G2.Next(second, _) => assertEquals(second, 8)
          case G2.Done => fail("a second element")
      case G2.Done => fail("a first element")

  test("an effect its context does not have cannot be performed"):
    assert(compileErrors("""run(reader[Unit](1)(tell("x")))""").nonEmpty)
