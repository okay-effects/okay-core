package okay.one

import okay.one.Cont.{perform, pure}
import okay.one.machine.Machine.Head
import okay.one.row.*
import okay.one.std.*
import okay.one.step.Step.*

/** a program at its head: the light step — each operation paused as one object, the rest the machine's own */
class TestHead extends munit.FunSuite:
  type R = Writer % Int +: Pure
  def counting(n: Int): Unit ! R =
    if n == 0 then pure(()) else Writer.tell[Int, R](n).flatMap(_ => counting(n - 1))

  @scala.annotation.tailrec final def drain(h: Head[R, Unit], acc: List[Int]): List[Int] = h match
    case Head.Done(_) => acc.reverse
    case p: Head.Paused[r, a, e, y, i, o, b, v, j] => p.op match
      case Writer.Tell(w) => drain(p.resume(().asInstanceOf[y]), w.asInstanceOf[Int] :: acc)

  test("each operation in order, the rest resumed"):
    assertEquals(drain(counting(5).head, Nil), List(5, 4, 3, 2, 1))

  test("100 000 steps in constant stack"):
    assertEquals(drain(counting(100000).head, Nil).length, 100000)

  test("a paused step resumed twice: each a run of its own"):
    counting(2).head match
      case p: Head.Paused[r, a, e, y, i, o, b, v, j] =>
        assertEquals(drain(p.resume(().asInstanceOf[y]), Nil), List(1))
        assertEquals(drain(p.resume(().asInstanceOf[y]), Nil), List(1))
      case other => fail(s"not paused: $other")

  test("failed where it was: a recover in the rest given the error"):
    val p: Int ! R = Writer.tell[Int, R](1).map(_ => 0).recover(e => pure(e.getMessage.length))
    p.head match
      case h: Head.Paused[r, a, e, y, i, o, b, v, j] => assertEquals(h.fail(RuntimeException("boom")), Head.Done(4))
      case other => fail(s"not paused: $other")
