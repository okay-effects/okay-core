package okay.one

import okay.one.Cont0.{perform, pure, rows}
import okay.one.Handlers.*
import okay.one.row.*
import okay.one.step.Step
import okay.one.step.Step.*
import scala.annotation.tailrec

/** a program stepped: its operations of the row outside given back one by one, a driver answering them */
class TestStep extends munit.FunSuite:
  type R = Ask +: Log +: Pure

  def asked[X](op: Ask[X], n: Int): X = op match { case Ask.Get => n }
  def told[X](op: Log[X]): (String, X) = op match { case Log.Tell(m) => (m, ()) }

  /** each `Get` answered `n`, each `Tell` kept: the driver a loop */
  def drive[A](s: Step[R, A], n: Int): (List[String], A) =
    @tailrec def go(s: Step[R, A], log: List[String]): (List[String], A) = s match
      case Step.Done(a) => (log.reverse, a)
      case Step.Failed(e) => throw e
      case Step.Cancelled() => throw RuntimeException("cancelled")
      case y: Step.Yield[r, a, e, x] => y.in match
        case _: In.Here[f, t] => go(y.resume(asked(y.op, n)), log)
        case th: In.There[f, g, t] => th.in match
          case _: In.Here[f2, t2] =>
            val (m, x) = told(y.op)
            go(y.resume(x), m :: log)
          case th2: In.There[f2, g2, t2] => In.absurd(th2.in)
    go(s, Nil)

  test("each operation given back, the rest resumed with its value") {
    val r = rows[R]
    val p = for a <- r.perform(Ask.Get); _ <- r.perform(Log.Tell(s"a $a")); b <- r.perform(Ask.Get) yield a + b
    assertEquals(drive(p.step, 5), (List("a 5"), 10))
  }

  test("handlers in the program: theirs answered in it, the others given back past them") {
    type Q = Choose +: St +: R
    val q = rows[Q]
    val p = (for b <- q.perform(Choose.Flip); s <- q.perform(St.Get); n <- q.perform(Ask.Get)
                 _ <- q.perform(Log.Tell(s"$b $s")); _ <- q.perform(St.Put(s + n)) yield s)
      .handle(choose).handle(state(1))
    assertEquals(drive(p.step, 10), (List("true 1", "false 11"), (21, List(1, 11))))
  }

  test("a step resumed twice: each its own rest") {
    val p = rows[R].perform(Ask.Get).map(_ * 2)
    p.step match
      case y: Step.Yield[r, a, e, x] => y.in match
        case _: In.Here[f, t] =>
          assertEquals(drive(y.resume(asked(y.op, 1)), 0)._2, 2)
          assertEquals(drive(y.resume(asked(y.op, 7)), 0)._2, 14)
        case _ => fail("not an Ask")
      case _ => fail("not a step")
  }

  test("stack safe: a loop of operations stepped") {
    def loop(i: Int, acc: Int): Int ! R =
      if i == 0 then pure(acc) else rows[R].perform(Ask.Get).flatMap(n => loop(i - 1, acc + n))
    assertEquals(drive(loop(100000, 0).step, 1)._2, 100000)
  }
