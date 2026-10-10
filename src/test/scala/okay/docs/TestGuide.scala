package okay.docs

import okay.core.*
import okay.core.Control.{reset, shift}
import okay.std.*

/** THE EXAMPLES OF docs/guide.md and README.md, as they are written there: each compiles and gives the answer the guide says, on
 * Cont and on Cont0 */
class TestGuide extends munit.FunSuite:

  // -- a program, its effects named in its context
  def greet(using c: Effects, r: Reader % String :? c.Effs, w: Writer % String :? c.Effs): c.Prog[Int] =
    ask[String].flatMap(name => tell(s"hello, $name").map(_ => name.length))

  test("guide: a program with effects, run on either monad"):
    assertEquals(okay.cont.run(reader("Ann")(writer[String, Int](greet))), (List("hello, Ann"), 3))
    assertEquals(okay.zero.run(reader("Ann")(writer[String, Int](greet))), (List("hello, Ann"), 3))

  // -- A ! E1 + E2
  def inc: Int ! Reader % Int = ask[Int].map(_ + 1)
  def both: Int ! Reader % Int + Writer % String =
    ask[Int].flatMap(a => tell(a.toString).map(_ => a))

  test("guide: A ! E1 + E2"):
    assertEquals(okay.cont.run(reader(5)(inc)), 6)
    assertEquals(okay.zero.run(writer[String, Int](reader(5)(both))), (List("5"), 5))

  // -- a loop of operations
  def counting(i: Int)(using c: Effects, s: State % Int :? c.Effs): c.Prog[Int] =
    if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => counting(i - 1))

  test("guide: 100 000 operations in constant stack"):
    assertEquals(okay.zero.run(state[Int](0)(counting(100000))), (100000, 100000))

  // -- an effect of one's own and its handler
  enum Log[+A]:
    case Line(s: String) extends Log[Unit]

  def log(s: String)(using c: Effects, in: Log :? c.Effs): c.Prog[Unit] = perform(Log.Line(s))

  def logging[A](using c: Effects)(body: c.Body[Log, (List[String], A), A]): c.Prog[(List[String], A)] =
    Effects.handle(new Handler[c.Prog, Log, A, (List[String], A)]:
      def ret(a: A): (List[String], A) = (Nil, a)
      def apply[X](op: Log[X], k: X => c.Prog[(List[String], A)]): c.Prog[(List[String], A)] = op match
        case Log.Line(s) => k(()).map((lines, a) => (s :: lines, a)))(body)

  // -- an answering handler: its operations answered in place
  enum Clock[+A]:
    case Now extends Clock[Long]

  def now(using c: Effects, in: Clock :? c.Effs): c.Prog[Long] = perform(Clock.Now)

  def fixed[A](using c: Effects)(t: Long)(body: c.Body[Clock, A, A]): c.Prog[A] =
    Effects.handle(new Answering[c.Prog, Clock, A, A]:
      def ret(a: A): A = a
      def value[X](op: Clock[X]): X = op match
        case Clock.Now => t)(body)

  test("guide: an effect of one's own, a general handler and an answering one"):
    def prog(using c: Effects, l: Log :? c.Effs, k: Clock :? c.Effs): c.Prog[Long] =
      now.flatMap(t => log(s"at $t").map(_ => t * 2))
    assertEquals(okay.cont.run(logging(fixed(21)(prog))), (List("at 21"), 42L))
    assertEquals(okay.zero.run(logging(fixed(21)(prog))), (List("at 21"), 42L))

  // -- control
  def twice(using c: Effects): c.Prog[Int] = reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1))
  def modified(using c: Effects): c.Prog[String] = reset[Int](shift[Int, String](k => k(5).map(_.toString)).map(_ * 2))

  test("guide: shift and reset, the answer type modified"):
    assertEquals(okay.cont.run(twice), 12)
    assertEquals(okay.zero.run(twice), 12)
    assertEquals(okay.zero.run(modified), "10")

  test("guide: choose, multi-shot"):
    def pairs(using c: Effects, ch: Choose :? c.Effs): c.Prog[(Int, Char)] =
      among(Seq(1, 2)).flatMap(n => among(Seq('a', 'b')).map(ch => (n, ch)))
    assertEquals(okay.zero.run(choose[(Int, Char)](pairs)), Seq((1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')))

  test("guide: shift0, Cont0's own"):
    import okay.zero.shift0
    // reset(shift0(k => k(1) + 1) * 10): k = _ * 10, the body outside the reset
    assertEquals(okay.zero.run[Int](reset[Int](shift0[Int, Int](k => k(1).map(_ + 1)).map(_ * 10))), 11)

  test("guide: a stream, one type on every monad"):
    def fibs(a: Long, b: Long)(using c: Effects, e: Emit % Long :? c.Effs): c.Prog[Unit] =
      yield_(a).flatMap(_ => fibs(b, a + b))
    assertEquals(stream[Long](okay.zero.top)(fibs(0, 1)).take(8), List(0L, 1, 1, 2, 3, 5, 8, 13))
    assertEquals(stream[Long](okay.cont.top)(fibs(0, 1)).take(8), List(0L, 1, 1, 2, 3, 5, 8, 13))
    val evens = for n <- stream[Long](okay.zero.top)(fibs(0, 1)) if n % 2 == 0; s <- Stream.of(n, -n) yield s
    assertEquals(evens.take(6), List(0L, 0, 2, -2, 8, -8))
