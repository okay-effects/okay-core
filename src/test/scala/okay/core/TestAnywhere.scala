package okay.core

import okay.one.Cont
import okay.one.handler.Deep
import okay.core.std.*

/** a handler of an effect wherever it is in a row: okay's shape, over the kernel's program itself */
class TestAnywhere extends munit.FunSuite:

  // -- okay's TestEffectsRows, as it is written there
  enum Ask[A]:
    case Number() extends Ask[Int]
  enum Say[A]:
    case Line(s: String) extends Say[Unit]

  def reader[A](n: Int): Answering[Ask, A, A] = new Answering[Ask, A, A]:
    def ret(a: A): A = a
    def value[X](op: Ask[X]): X = op match
      case Ask.Number() => n
  def writer[A]: Handler[Say, A, (List[String], A)] = new Handler[Say, A, (List[String], A)]:
    def ret(a: A): (List[String], A) = (Nil, a)
    def apply[T <: Row, X](op: Say[X], k: X => Eff[T, (List[String], A)]): Eff[T, (List[String], A)] = op match
      case Say.Line(s) => k(()).map((log, a) => (s :: log, a))

  /** a program written once over the interface: its row declared, each operation by its path */
  def prog[M[_ <: Row, _]](using E: RowEffects[M]): M[Ask +: Say +: Pure, Int] =
    E.perform[Ask, Ask +: Say +: Pure, Int](Ask.Number()).flatMap(n =>
      E.perform[Say, Ask +: Say +: Pure, Unit](Say.Line(n.toString)).map(_ => n + 1))

  val E: RowEffects[Eff] = RowEffects.eff

  test("okay's interface over the kernel's program: handlers apply in any order, each takes its effect wherever it is"):
    assertEquals(E.run(E.handleAt(writer[Int])(E.handleAt(reader[Int](41))(prog[Eff]))), (List("41"), 42))
    assertEquals(E.run(E.handleAt(reader[(List[String], Int)](41))(E.handleAt(writer[Int])(prog[Eff]))), (List("41"), 42))

  test("okay's words: A ! R, an operation at the row expected, handle wherever the effect is, value"):
    import E.*
    // the row named: a program AT it (the interface's `A ! Es` is a program needing effects, fitted to a place)
    val counting: Eff[State % Int +: Say +: Pure, Int] =
      for
        n <- effect[State % Int, Int](State.Get())
        _ <- effect[State % Int, Unit](State.Put(n + 1))
        _ <- Say.Line(s"$n").perform
        m <- effect[State % Int, Int](State.Get())
      yield m
    assertEquals(Cont.handle(counting)(State[Int, Say +: Pure, Int](41)).handle(writer[(Int, Int)]).value, (List("41"), (42, 42)))
    assertEquals(Cont.handle(counting.handle(writer[Int]))(State[Int, Pure, (List[String], Int)](41)).value, (42, (List("41"), 42)))

  test("Pure + A + B and A +: B +: Pure are one row; a tail call costs no frame — 100 000 of them"):
    def loop[M[_ <: Row, _]](n: Int, acc: Int)(using E: RowEffects[M]): M[Ask +: Pure, Int] =
      if n == 0 then E.pure(acc)
      else E.perform[Ask, Ask +: Pure, Int](Ask.Number()).flatMap(x => E.tailcall(loop[M](n - 1, acc + x)))
    assertEquals(E.run(E.handleAt(reader[Int](1))(loop[Eff](100000, 0))), 100000)

  // -- the standard effects taken off wherever they are: the kernel's handlers, over a row named
  type Row3 = State % Int +: Writer % String +: Throws % String +: Pure
  def counting: Eff[Row3, Int] =
    get[Int].at[Row3].flatMap(n => put(n + 1).at[Row3].flatMap(_ => tell(s"$n").at[Row3]
      .flatMap(_ => if n > 100 then raise("big").at[Row3] else get[Int].at[Row3])))

  test("the standard handlers in any order: the order of handlers is the semantics, the row's says nothing"):
    import okay.one.std.{Reader as R, State as S, Writer as W, Throws as Th}
    type R2 = Writer % String +: Throws % String +: Pure
    type R1 = Throws % String +: Pure
    assertEquals(Cont.handle(Cont.handle(Cont.handle(counting)(S[Int, R2, Int](41)))(W.collect[String, R1, (Int, Int)]))(Th.either[String, Pure, (List[String], (Int, Int))]).run,
      Right((List("41"), (42, 42))))
    type R2b = State % Int +: Throws % String +: Pure
    assertEquals(Cont.handle(Cont.handle(Cont.handle(counting)(W.collect[String, R2b, Int]))(Th.either[String, State % Int +: Pure, (List[String], Int)]))(S[Int, Pure, Either[String, (List[String], Int)]](41)).run,
      (42, Right((List("41"), 42))))
    // a failure under the state keeps it, over the state loses it
    assertEquals(Cont.handle(Cont.handle(Cont.handle(counting)(Th.either[String, State % Int +: Writer % String +: Pure, Int]))(W.collect[String, State % Int +: Pure, Either[String, Int]]))(S[Int, Pure, (List[String], Either[String, Int])](200)).run,
      (201, (List("200"), Left("big"))))
    assertEquals(Cont.handle(Cont.handle(Cont.handle(counting)(S[Int, Writer % String +: Throws % String +: Pure, Int](200)))(Th.either[String, Writer % String +: Pure, (Int, Int)]))(W.collect[String, Pure, Either[String, (Int, Int)]]).run,
      (List("200"), Left("big")))

  test("a handler inside a body taken off by an outer one: paths relocated through both"):
    import okay.one.std.{Reader as R, State as S}
    type Inner = Reader % Int +: State % Int +: Pure
    val body: Eff[Inner, Int] = ask[Int].at[Inner].flatMap(a => get[Int].at[Inner].map(_ + a))
    // the state outside, handled first though it is second in the row; the reader inside, after it
    val p = Cont.handle(Cont.handle(body)(S[Int, Reader % Int +: Pure, Int](10)).map(_._2))(R[Int, Pure, Int](5))
    assertEquals(p.run, 15)

  test("local and handleWith inside a body whose row was another's: the interceptor's path relocated"):
    import okay.one.std.{Reader as R, Throws as Th}
    type R2 = Throws % String +: Reader % Int +: Pure
    val body: Eff[R2, Int] = local[Int, Int, R2](_ * 10)(ask[Int].perform)
      .flatMap(a => handleWith[String, Int, R2](raise("x").perform)(e => pure(e.length + a)))
    assertEquals(Cont.handle(Cont.handle(body)(R[Int, Throws % String +: Pure, Int](4)))(Th.either[String, Pure, Int]).run, Right(41))

  // -- one's own effect written once, over any row outside; taken off wherever it is
  enum Log[A]:
    case Line(s: String) extends Log[Unit]
  def logging[A, T <: Row]: Deep[Log, T, A, (List[String], A)] = new Deep[Log, T, A, (List[String], A)]:
    def ret(a: A): Eff[T, (List[String], A)] = pure((Nil, a))
    def apply[X](op: Log[X], k: X => Eff[T, (List[String], A)]): Eff[T, (List[String], A)] = op match
      case Log.Line(l) => k(()).map((ls, a) => (l :: ls, a))

  test("one's own effect written once, over any row outside: taken off wherever it is"):
    import okay.one.std.Reader as R
    type R5 = Reader % Int +: Log +: Pure
    val p: Eff[R5, Int] = ask[Int].at[R5].flatMap(n => perform(Log.Line(s"n=$n")).at[R5].map(_ => n * 2))
    assertEquals(Cont.handle(Cont.handle(p)(logging[Int, Reader % Int +: Pure]))(R[Int, Pure, (List[String], Int)](21)).run, (List("n=21"), 42))

  test("100 000 operations of an effect deep in the row, its handler taken off from there"):
    import okay.one.std.{Reader as R, State as S, Writer as W}
    type R4 = Writer % Int +: Reader % Int +: State % Int +: Pure
    def loop(i: Int): Eff[R4, Unit] =
      if i == 0 then pure(()) else get[Int].at[R4].flatMap(s => put(s + 1).at[R4]).flatMap(_ => loop(i - 1))
    val r = Cont.handle(Cont.handle(Cont.handle(loop(100000))(S[Int, Writer % Int +: Reader % Int +: Pure, Unit](0)))(W.collect[Int, Reader % Int +: Pure, (Int, Unit)]))(R[Int, Pure, (List[Int], (Int, Unit))](0)).run
    assertEquals(r._1, Nil)
    assertEquals(r._2._1, 100000)
