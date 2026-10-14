package okay.one

import okay.one.Cont.*
import okay.one.row.*
import okay.one.std.*

/** the monad's laws, seen by what a program gives when run: with effects of each kind, and in control with its answer
 * changed */
class TestLaws extends munit.FunSuite:
  type R = Reader % Int +: Writer % String +: State % Int +: Choose +: Pure
  def runR[A](p: A ! R): List[(Int, (List[String], A))] =
    p.handle(Reader(7)).handle(Writer.collect).handle(State(0)).handle(Choose.all).run

  /** a program of each effect, its value from `x` */
  def f(x: Int): Int ! R =
    for
      r <- Reader.ask[Int, R]
      _ <- Writer.tell[String, R](s"f $x")
      s <- State.get[Int, R]
      _ <- State.put[Int, R](s + x)
      b <- Choose.flip[R]
    yield if b then x + r else x * r
  def g(x: Int): Int ! R = Writer.tell[String, R](s"g $x").flatMap(_ => State.get[Int, R]).map(_ + x)
  val m: Int ! R = f(1)

  test("left identity: pure(a).flatMap(f) is f(a)") {
    assertEquals(runR(pure[R, Int](3).flatMap(f)), runR(f(3)))
  }

  test("right identity: m.flatMap(pure) is m") {
    assertEquals(runR(m.flatMap(a => pure[R, Int](a))), runR(m))
  }

  test("associativity: m.flatMap(f).flatMap(g) is m.flatMap(x => f(x).flatMap(g))") {
    assertEquals(runR(m.flatMap(f).flatMap(g)), runR(m.flatMap(x => f(x).flatMap(g))))
  }

  test("map: by identity m itself; of two functions, the two maps; flatMap with pure") {
    val h = (x: Int) => x * 10
    val i = (x: Int) => x - 1
    assertEquals(runR(m.map(identity)), runR(m))
    assertEquals(runR(m.map(h).map(i)), runR(m.map(h andThen i)))
    assertEquals(runR(m.map(h)), runR(m.flatMap(x => pure[R, Int](h(x)))))
  }

  test("the laws in a $ changing its answer: binds around the shift0, associated either way") {
    type S = Int => String
    type C[A] = A ! Pure
    type P = Delim[Pure]
    val shift: Cont[P, C[String], C[S], Int] =
      Shift0[P, Pure, Nothing, Nothing, String, C[S], Int](Nearest.Here(), k => Return((x: Int) => k(x).run))
    def show(n: Int): Cont[P, C[String], C[String], String] = Return(n.toString)
    def twice(s: String): Cont[P, C[String], C[String], String] = Return(s + s)
    def printf(body: Cont[P, C[String], C[S], String]): S =
      Dollar[Pure, C[String], Nothing, Nothing, S, String](body, (s: String) => Return[Pure, Nothing, String]("<" + s + ">")).run
    val left = printf(shift.flatMap(show).flatMap(twice))
    val right = printf(shift.flatMap(n => show(n).flatMap(twice)))
    assertEquals(left(4), "<44>")
    assertEquals(right(4), left(4))
    assertEquals(printf(shift.map(_.toString)).apply(5), printf(shift.flatMap(show)).apply(5))
  }
