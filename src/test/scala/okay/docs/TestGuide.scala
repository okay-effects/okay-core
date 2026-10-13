package okay.docs

import okay.*
import okay.core.Control.{reset, shift, shift0}

/** THE EXAMPLES OF docs/guide.md and README.md, as they are written there: each compiles and gives the answer the
 * guide says */
class TestGuide extends munit.FunSuite:

  // -- a program, its effects named
  def greet: Int ! Reader % String + Writer % String =
    ask[String].flatMap(name => tell(s"hello, $name").map(_ => name.length))

  test("guide: a program with effects, under its handlers in either order"):
    assertEquals(run(reader("Ann")(writer[String, Int, Reader % String +: Pure](greet))), (List("hello, Ann"), 3))
    assertEquals(run(writer[String, Int, Pure](reader("Ann")(greet))), (List("hello, Ann"), 3))

  // -- A ! E1 + E2
  def inc: Int ! Reader % Int = ask[Int].map(_ + 1)
  def both: Int ! Reader % Int + Writer % String =
    ask[Int].flatMap(a => tell(a.toString).map(_ => a))

  test("guide: A ! E1 + E2"):
    assertEquals(run(reader(5)(inc)), 6)
    assertEquals(run(writer[String, Int, Pure](reader(5)(both))), (List("5"), 5))

  // -- a loop of operations
  def counting(i: Int): Int ! State % Int =
    if i == 0 then get[Int].perform else get[Int].flatMap(x => put(x + 1)).flatMap(_ => counting(i - 1))

  test("guide: 100 000 operations in constant stack"):
    assertEquals(run(state(0)(counting(100000))), (100000, 100000))

  // -- an effect of one's own and its handler
  enum Log[A]:
    case Line(s: String) extends Log[Unit]

  def log(s: String): Op[Log, Unit] = perform(Log.Line(s))

  def logging[A, T <: Row](body: Under[Log, T, A]): Eff[T, (List[String], A)] =
    handle(new Handler[Log, A, (List[String], A)]:
      def ret(a: A): (List[String], A) = (Nil, a)
      def apply[R <: Row, X](op: Log[X], k: X => Eff[R, (List[String], A)]): Eff[R, (List[String], A)] = op match
        case Log.Line(s) => k(()).map((lines, a) => (s :: lines, a)))(body)

  // -- an answering handler: its operations answered in place
  enum Clock[A]:
    case Now() extends Clock[Long]

  def now: Op[Clock, Long] = perform(Clock.Now())

  def fixed[A, T <: Row](t: Long)(body: Under[Clock, T, A]): Eff[T, A] =
    handle(new Answering[Clock, A, A]:
      def ret(a: A): A = a
      def value[X](op: Clock[X]): X = op match
        case Clock.Now() => t)(body)

  test("guide: an effect of one's own, a general handler and an answering one"):
    def prog: Long ! Log + Clock = now.flatMap(t => log(s"at $t").map(_ => t * 2))
    assertEquals(run(logging(fixed(21)(prog))), (List("at 21"), 42L))

  // -- control as an effect of the row
  def twice[T <: Row]: Eff[T, Int] =
    reset[Int, T](shift[Int, T, Int](k => k(10).flatMap(k)).at[Shifts[Int, T] +: T].map(_ + 1))

  test("guide: shift and reset under any handlers"):
    assertEquals(run(twice[Pure]), 12)
    assertEquals(run(reader(3)(twice[Reader % Int +: Pure].flatMap(x => ask[Int].at[Reader % Int +: Pure].map(_ + x)))), 15)
    // reset(shift0(k => k(1) + 1) * 10): k = _ * 10, the body outside the reset
    assertEquals(run(reset[Int, Pure](shift0[Int, Pure, Int](k => k(1).map(_ + 1)).at[Shifts[Int, Pure] +: Pure].map(_ * 10))), 11)

  test("guide: choose, multi-shot"):
    def pairs: (Int, Char) ! Choose = among(Seq(1, 2)).flatMap(n => among(Seq('a', 'b')).map(ch => (n, ch)))
    assertEquals(run(choose(pairs)), Seq((1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')))

  test("guide: a stream"):
    def fibs(a: Long, b: Long): Unit ! Emit % Long = yield_(a).flatMap(_ => fibs(b, a + b))
    assertEquals(stream[Long](fibs(0, 1)).take(8), List(0L, 1, 1, 2, 3, 5, 8, 13))
    val evens = for n <- stream[Long](fibs(0, 1)) if n % 2 == 0; s <- Stream.of(n, -n) yield s
    assertEquals(evens.take(6), List(0L, 0, 2, -2, 8, -8))
