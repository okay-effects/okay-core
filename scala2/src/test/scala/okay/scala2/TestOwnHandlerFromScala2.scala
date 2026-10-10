package okay.scala2

import okay.core._
import okay.std._

/** an effect of one's own and its handler, written in Scala 2: the clauses over `Program.Ops` — plain methods */
/** an operation gives its own answer from the count — so a clause in Scala 2, whose pattern match cannot tell `X`,
 * answers with no cast */
sealed trait Counter[A] { def answer(n: Int): A }
case object Tick extends Counter[Unit] { def answer(n: Int): Unit = () }
case object Peek extends Counter[Int] { def answer(n: Int): Int = n }

class TestOwnHandlerFromScala2 extends munit.FunSuite {
  type Log[X] = Writer[String, X]

  /** counts the ticks; a peek answers the count so far, and tells it to the writer outside */
  def counting[A](start: Int): Program.Handler[Counter, A, (Int, A)] = new Program.Handler[Counter, A, (Int, A)] {
    private var n = start
    def ret(a: A): (Int, A) = (n, a)
    def apply[R <: Row, X](ops: Program.Ops[R])(op: Counter[X], k: X => ops.P[(Int, A)]): ops.P[(Int, A)] = {
      if (op == Tick) n += 1
      k(op.answer(n))
    }
  }

  test("Scala 2: an effect and its handler of its own, on Cont0 and on Cont") {
    type R = +:[Counter, Pure]
    val p: Program[R, Int] =
      Program.perform[Counter, R, Unit](Tick).flatMap(_ => Program.perform[Counter, R, Unit](Tick)).flatMap(_ => Program.perform[Counter, R, Int](Peek))
    assertEquals(Program.run(okay.zero.top)(Program.handle(counting[Int](0))(p)), (2, 2))
    assertEquals(Program.run(okay.cont.top)(Program.handle(counting[Int](10))(p)), (12, 12))
  }

  test("Scala 2: a clause performs an effect outside it — ops.perform, by its path in the row outside") {
    type R = +:[Counter, +:[Log, Pure]]
    type O = +:[Log, Pure]
    // a handler for the row outside it, `O`: its clause performs the writer there, by its path in `O`
    val logging = new Program.HandlerAt[O, Counter, Int, Int] {
      def ret(a: Int): Int = a
      def apply[X](ops: Program.Ops[O])(op: Counter[X], k: X => ops.P[Int]): ops.P[Int] =
        if (op == Peek) ops.flatMap(ops.perform[Log, Unit](Writer.Tell("peeked")))(_ => k(op.answer(7)))
        else k(op.answer(0))
    }
    val p: Program[R, Int] = Program.perform[Counter, R, Int](Peek).map(_ * 2)
    assertEquals(Program.run(okay.zero.top)(Programs.writer[String](Program.handleAt(logging)(p))), (List("peeked"), 14))
  }
}
