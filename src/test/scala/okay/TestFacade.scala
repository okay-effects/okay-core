package okay

/** what a user writes with `import okay.*` alone: `!`, `+`, `%`, the standard effects, `run` */
class TestFacade extends munit.FunSuite:
  def greet: Int ! Reader % String + Writer % String =
    ask[String].flatMap(name => tell(s"hello, $name").map(_ => name.length))

  def counted: Int ! State % Int + Reader % Int + Throws % String =
    ask[Int].flatMap(n => if n < 0 then raise("negative") else modify[Int](_ + n))

  test("a program as its type says it, run"):
    assertEquals(run(writer[String, Int, Pure](reader("Ann")(greet))), (List("hello, Ann"), 3))

  test("three effects joined, handled in any order"):
    assertEquals(run(throws[String, (Int, Int), Pure](state(1)(reader(41)(counted)))), Right((42, 42)))
    assertEquals(run(state(1)(throws[String, Int, State % Int +: Pure](reader(-1)(counted)))), (1, Left("negative")))

  // -- okay's words: one's own effect by `effect` and `.perform`, a signature polymorphic in the rest
  enum Pick[A]:
    case Of(xs: List[Int]) extends Pick[Int]

  def pair: (Int, Int) ! Pick = effect(Pick.Of(List(1, 2))).flatMap(a => Pick.Of(List(10, 20)).perform.map(b => (a, b)))

  /** every value of the body, one per pick: a handler of one's own */
  def every[A, T <: Row](body: Under[Pick, T, A]): Eff[T, List[A]] =
    handle(new Handler[Pick, A, List[A]]:
      def ret(a: A): List[A] = List(a)
      def apply[R <: Row, X](op: Pick[X], k: X => Eff[R, List[A]]): Eff[R, List[A]] = op match
        case Pick.Of(xs) => xs.foldRight(pure[R, List[A]](Nil))((x, acc) => k(x).flatMap(l => acc.map(l ++ _))))(body)

  // okay-agent's Search: polymorphic in the rest of the program's effects, `F`
  def samples[A, F[_]](n: Int)(gen: A ! Choose + F): List[A] ! Choose + F =
    if n == 0 then pure(Nil) else gen.flatMap(a => samples(n - 1)(gen).map(a :: _))

  def roll: Int ! Choose + Reader % Int = ask[Int].flatMap(base => among(Seq(base, base + 1)))

  test("okay's words: effect(op) and op.perform, a handler of one's own"):
    assertEquals(run(every(pair)), List((1, 10), (1, 20), (2, 10), (2, 20)))

  test("a signature polymorphic in the rest, A ! Choose + F, used with F any effect"):
    assertEquals(run(reader(5)(choose(samples(2)(roll)))), Seq(List(5, 5), List(5, 6), List(6, 5), List(6, 6)))
    assertEquals(run(choose(reader(5)(samples[Int, Reader % Int](1)(roll)))), Seq(List(5), List(6)))

  // okay-std's runners: an effect off the front, the rest `F` left — okay's signatures, okay-core's handlers
  def runReader[R, A, F[_]](r: R)(p: A ! Reader % R + F): A ! F = reader(r)(p)

  def step: Int ! Reader % Int + State % Int = ask[Int].flatMap(n => modify[Int](_ + n))

  test("okay's runner signatures, A ! E + F to A ! F, the rest inferred"):
    assertEquals(run(state(1)(runReader(41)(step))), (42, 42))
