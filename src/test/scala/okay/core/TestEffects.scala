package okay.core

import okay.std.*

/** a generator's consumer: the next element and the rest, a program of nothing of the generator's own answer; or done */
enum Gen[W]:
  case Next(w: W, rest: Eff[Pure, Gen[W]])
  case Done()

/** the effects in a program's place: the row never named — the handlers make it, outside in */
class TestEffects extends munit.FunSuite:

  test("reader: ask answered in place"):
    assertEquals(run(reader(41)(ask[Int].map(_ + 1))), 42)

  test("writer: the log in order, and the value"):
    assertEquals(run(writer[String, Int, Pure](tell("a").flatMap(_ => tell("b")).map(_ => 1))), (List("a", "b"), 1))

  test("reader and writer, the handlers in either order: a program needs only its effects"):
    def prog: Int ! Reader % Int + Writer % String = ask[Int].flatMap(n => tell(n.toString).map(_ => n + 1))
    assertEquals(run(writer[String, Int, Pure](reader(41)(prog))), (List("41"), 42))
    assertEquals(run(reader(41)(writer[String, Int, Reader % Int +: Pure](prog))), (List("41"), 42))

  test("state: get, put, modify; the last state and the value"):
    val r = run:
      state(1):
        for
          a <- get[Int]
          _ <- put(a + 1)
          b <- modify[Int](_ * 10)
        yield s"$a,$b"
    assertEquals(r, (20, "1,20"))

  test("100 000 operations answered in place, and forwarded through a handler, in constant stack"):
    def loop(i: Int): Int ! State % Int =
      if i == 0 then get[Int].perform else modify[Int](_ + 1).flatMap(_ => loop(i - 1))
    assertEquals(run(state(0)(loop(100000))), (100000, 100000))
    def asks(i: Int, acc: Int): Int ! Reader % Int =
      if i == 0 then pure(acc) else ask[Int].flatMap(x => asks(i - 1, acc + x))
    assertEquals(run(reader(1)(collect[String, Int, Reader % Int +: Pure](asks(100000, 0)))), (Nil, 100000))

  test("an operation through 16 handlers of other effects, and back, 10 000 times"):
    def loop(i: Int, acc: Int): Int ! Reader % Int =
      if i == 0 then pure(acc) else ask[Int].flatMap(x => loop(i - 1, acc + x))
    def nested[T <: Row](d: Int)(using In[Reader % Int, T]): Eff[T, Int] =
      given Place[T] = Place.at
      if d == 0 then loop(10000, 0) else reader[Double, Int, T](0.5)(nested[Reader % Double +: T](d - 1))
    assertEquals(run(reader(2)(nested[Reader % Int +: Pure](16))), 20000)

  test("two instances of one effect, State[Int] and State[String]: each goes to its own handler, by its path"):
    def body: Unit ! State % Int + State % String =
      get[Int].flatMap(n => put(n + 1).flatMap(_ => get[String].flatMap(s => put(s + (n + 1))))).map(_ => ())
    assertEquals(run(state("x")(state(41)(body))), ("x42", (42, ())))
    assertEquals(run(state(41)(state("x")(body))), (42, ("x42", ())))

  test("throws: raise aborts the body, Left; no raise, Right"):
    assertEquals(run(throws[String, Int, Pure]((raise("boom").at[Throws % String +: Pure]: Eff[Throws % String +: Pure, Int]).map(_ + 1))), Left("boom"))
    assertEquals(run(throws[String, Int, Pure](pure(1).map(_ + 1))), Right(2))

  test("state and throws: the order of handlers is the semantics — state kept or lost on a failure"):
    def body: Int ! State % Int + Throws % String = put(5).flatMap(_ => raise("boom").perform)
    assertEquals(run(state(0)(throws[String, Int, State % Int +: Pure](body))), (5, Left("boom")))
    assertEquals(run(throws[String, (Int, Int), Pure](state(0)(body))), Left("boom"))

  test("choose: every path, in order — multi-shot"):
    val r = run:
      choose:
        for
          x <- among(Seq(1, 2, 3))
          y <- among(Seq(10, 20))
        yield (x, y)
    assertEquals(r, Seq((1, 10), (1, 20), (2, 10), (2, 20), (3, 10), (3, 20)))

  test("choose among 10 000: every path, in order"):
    assertEquals(run(choose(among(0 until 10000).map(_ * 2))), (0 until 10000).map(_ * 2))

  test("8 queens by choose: 92 solutions"):
    def safe(col: Int, placed: List[Int]) = placed.zipWithIndex.forall((c, d) => c != col && math.abs(c - col) != d + 1)
    def place(row: Int, placed: List[Int]): List[Int] ! Choose =
      if row == 8 then pure(placed) else among((0 until 8).filter(safe(_, placed))).flatMap(col => place(row + 1, col :: placed))
    val all = run(choose(place(0, Nil)))
    assertEquals(all.length, 92)
    assertEquals(all.head, List(3, 1, 6, 2, 5, 7, 4, 0))

  test("choose with an empty choice prunes the path"):
    assertEquals(run(choose(among(Seq(1, 2)).flatMap(x => among(if x == 1 then Seq.empty else Seq(x))))), Seq(2))

  test("choose under a state: the state threaded through the paths — the second sees the first's last"):
    assertEquals(run(state(0)(choose(among(Seq(1, 2)).flatMap(x => modify[Int](_ + x))))), (3, Seq(1, 3)))

  test("a state under choose: each path its own; a cell: one, every path's"):
    def counted: Int ! Choose + State % Int =
      among(Seq(1, 2, 3)).flatMap(x => get[Int].flatMap(s => put(s + x).flatMap(_ => get[Int].perform)))
    assertEquals(run(state(0)(choose(counted))), (6, Seq(1, 3, 6)))
    assertEquals(run(choose(state(0)(counted))), Seq((1, 1), (2, 2), (3, 3)))
    assertEquals(run(choose(cell(0)(counted))), Seq((1, 1), (3, 3), (6, 6)))

  test("collect: every element yielded, and the value"):
    assertEquals(run(collect[Int, String, Pure](yield_(1).flatMap(_ => yield_(2)).map(_ => "done"))), (List(1, 2), "done"))

  test("collect over choose: both paths' elements, in the order they ran"):
    assertEquals(run(collect[Int, Seq[Int], Pure](choose(among(Seq(1, 2)).flatMap(x => yield_(x).map(_ => x * 10))))), (List(1, 2), Seq(10, 20)))

  test("an effect its place does not have cannot be performed"):
    assert(compileErrors("""run(reader(1)(tell("x").at[Reader % Int +: Pure]))""").nonEmpty)

  test("translate: an effect's operations to a writer outside — in order, the body's value the answer"):
    def notes[A, T <: Row](body: Under[Note, T, A])(using In[Writer % String, T]): Eff[T, A] =
      translate[Note, T, A]([X] => (op: Note[X]) => op match
        case Note.Say(s) => tell(s"[$s]").at[T].map(_ => s.length))(body)
    def prog: Int ! Note = perform(Note.Say("ab")).flatMap(a => perform(Note.Say("c")).map(b => a * 10 + b))
    assertEquals(run(writer[String, Int, Pure](notes(prog))), (List("[ab]", "[c]"), 21))

  test("translate: an effect on a state installed inside, of a private type — the caller's own state untouched"):
    def counted[A, T <: Row](body: Under[Note, State % Count +: T, A]): Eff[T, (Int, A)] =
      state(Count(0)):
        translate[Note, State % Count +: T, A]([X] => (op: Note[X]) => op match
          case Note.Say(s) => modify[Count](n => Count(n.n + s.length)).map(_.n))(body)
      .map((n, a) => (n.n, a))
    def prog: (Int, Int) ! State % Int =
      counted(perform(Note.Say("ab")).flatMap(_ => perform(Note.Say("c")))).flatMap(r => modify[Int](_ + 1).map(_ => r))
    assertEquals(run(state(100)(prog)), (101, (3, 3)))

  test("translate: 100 000 operations in constant stack"):
    def says(i: Int, acc: Int): Int ! Note =
      if i == 0 then pure(acc) else perform(Note.Say("x")).flatMap(v => says(i - 1, acc + v))
    assertEquals(run(translate[Note, Pure, Int]([X] => (op: Note[X]) => op match
      case Note.Say(s) => pure(s.length))(says(100000, 0))), 100000)

  test("a cell is per run — the same program run twice starts from s0 twice; collect's log too"):
    val p: Eff[Pure, (Int, Int)] = cell(0)(modify[Int](_ + 1))
    assertEquals(run(p), (1, 1))
    assertEquals(run(p), (1, 1))
    val q: Eff[Pure, (List[Int], Unit)] = collect(yield_(1).flatMap(_ => yield_(2)))
    assertEquals(run(q), (List(1, 2), ()))
    assertEquals(run(q), (List(1, 2), ()))

  test("generate is lazy: an infinite body, three elements pulled, nothing past them runs"):
    var produced = 0
    def from(n: Int): Unit ! Emit % Int = yield_(n).flatMap(_ => { produced += 1; from(n + 1) })
    val gen: Eff[Pure, Gen[Int]] = generate[Int, Gen[Int], Pure](Gen.Next(_, _), Gen.Done())(from(0))
    def take(n: Int, g: Eff[Pure, Gen[Int]]): List[Int] = if n == 0 then Nil else run(g) match
      case Gen.Done() => Nil
      case Gen.Next(w, rest) => w :: take(n - 1, rest)
    assertEquals(take(3, gen), List(0, 1, 2))
    assertEquals(produced, 2)

  test("generate over a remaining effect: the rest of the generator still asks, under the handler it is run in"):
    type F = Reader % Int +: Pure
    enum G:
      case Next(w: Int, rest: Eff[F, G])
      case Done()
    def all(g: Eff[F, G]): List[Int] = run(reader(7)(g)) match
      case G.Done() => Nil
      case G.Next(w, rest) => w :: all(rest)
    val gen: Eff[F, G] =
      generate[Int, G, F](G.Next(_, _), G.Done())(ask[Int].flatMap(n => yield_(n)).flatMap(_ => ask[Int].flatMap(n => yield_(n * 2))))
    assertEquals(all(gen), List(7, 14))

  test("Fibonacci: collected and generated agree, the generator infinite, and 10 000 of them in constant stack"):
    def fibs(i: Int, a: Long, b: Long): Unit ! Emit % Long =
      if i == 0 then pure(()) else yield_(a).flatMap(_ => fibs(i - 1, b, a + b))
    def forever(a: Long, b: Long): Unit ! Emit % Long = yield_(a).flatMap(_ => forever(b, a + b))
    def take(n: Int, g: Eff[Pure, Gen[Long]]): List[Long] = if n == 0 then Nil else run(g) match
      case Gen.Done() => Nil
      case Gen.Next(w, rest) => w :: take(n - 1, rest)
    val first = List(0L, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    assertEquals(run(collect[Long, Unit, Pure](fibs(10, 0, 1)))._1, first)
    assertEquals(take(10, generate[Long, Gen[Long], Pure](Gen.Next(_, _), Gen.Done())(forever(0, 1))), first)
    assertEquals(run(collect[Long, Unit, Pure](fibs(10000, 0, 1)))._1.length, 10000)

  test("handlers are dynamic: a generator's rest asks the reader it is run under, not the one it was built under"):
    type F = Reader % Int +: Pure
    enum G:
      case Next(w: Int, rest: Eff[F, G])
      case Done()
    // built under reader(7): its rest is still a program over Reader — its type says it needs one — and the
    // consumer runs it under reader(8)
    val gen: Eff[Pure, G] =
      reader(7)(generate[Int, G, F](G.Next(_, _), G.Done())(ask[Int].flatMap(yield_(_)).flatMap(_ => ask[Int].flatMap(yield_(_)))))
    run(gen) match
      case G.Next(first, rest) =>
        assertEquals(first, 7)
        run(reader(8)(rest)) match
          case G.Next(second, _) => assertEquals(second, 8)
          case G.Done() => fail("a second element")
      case G.Done() => fail("a first element")

/** an effect of one's own, invariant — what `translate` takes */
enum Note[A]:
  case Say(s: String) extends Note[Int]
final case class Count(n: Int)
