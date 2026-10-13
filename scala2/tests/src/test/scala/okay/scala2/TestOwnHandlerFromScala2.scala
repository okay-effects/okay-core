package okay.scala2

import okay.one.Cont0
import okay.core.Ops._
import okay.one.handler.Deep
import okay.one.row._
import okay.one.std.Writer

/** an effect of one's own and its handler, written in Scala 2: the kernel's `Deep`, its clause a plain method */
/** an operation gives its own answer from the count — so a clause in Scala 2, whose pattern match cannot tell `X`,
 * answers with no cast */
sealed trait Counter[A] { def answer(n: Int): A }
case object Tick extends Counter[Unit] { def answer(n: Int): Unit = () }
case object Peek extends Counter[Int] { def answer(n: Int): Int = n }

class TestOwnHandlerFromScala2 extends munit.FunSuite {
  type Log[X] = Writer[String, X]
  type P[R <: Row, A] = Cont0[R, Nothing, Nothing, A]

  /** counts the ticks; a peek answers the count so far */
  def counting[T <: Row, A](start: Int): Deep[Counter, T, A, (Int, A)] = new Deep[Counter, T, A, (Int, A)] {
    private var n = start
    def ret(a: A): P[T, (Int, A)] = Cont0.pure((n, a))
    def apply[X](op: Counter[X], k: X => P[T, (Int, A)]): P[T, (Int, A)] = {
      if (op == Tick) n += 1
      k(op.answer(n))
    }
  }

  test("Scala 2: an effect and its handler of its own") {
    type R = +:[Counter, Pure]
    val p: P[R, Int] =
      okay.core.Ops.perform[Counter, R, Unit](Tick).flatMap[Nothing, Unit](_ => okay.core.Ops.perform[Counter, R, Unit](Tick)).flatMap[Nothing, Int](_ => okay.core.Ops.perform[Counter, R, Int](Peek))
    assertEquals(okay.core.Ops.run(okay.core.Ops.handle(p)(counting[Pure, Int](0))), (2, 2))
  }

  test("Scala 2: a clause performs an effect outside it — by its path in the row outside") {
    type R = +:[Counter, +:[Log, Pure]]
    type O = +:[Log, Pure]
    // a handler over the row outside it, `O`: its clause performs the writer there, by its path in `O`
    val logging = new Deep[Counter, O, Int, Int] {
      def ret(a: Int): P[O, Int] = Cont0.pure(a)
      def apply[X](op: Counter[X], k: X => P[O, Int]): P[O, Int] =
        if (op == Peek) okay.core.Ops.perform[Log, O, Unit](Writer.Tell("peeked")).flatMap[Nothing, Int](_ => k(op.answer(7)))
        else k(op.answer(0))
    }
    val p: P[R, Int] = okay.core.Ops.perform[Counter, R, Int](Peek).map(_ * 2)
    assertEquals(okay.core.Ops.run(okay.core.Ops.handle(okay.core.Ops.handle(p)(logging))(Writer.collect[String, Pure, Int])), (List("peeked"), 14))
  }
}
