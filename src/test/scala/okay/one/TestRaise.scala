package okay.one

import okay.one.Cont.*
import okay.one.Handlers.*
import okay.one.handler.*
import okay.one.row.*
import okay.one.step.Step
import okay.one.step.Step.*

/** WHAT USER CODE THROWS: caught where it is called, a `Raise` — its levels unwound, the top's answer `Failed`; `run`
 * rethrows it. Never an exception through the machine */
class TestRaise extends munit.FunSuite:
  val boom = RuntimeException("boom")

  def failed[R <: Row, A](s: Step[R, A]): Throwable | Null = s match
    case Step.Failed(e) => e
    case _ => null

  test("a throw in a map under a state: Failed, the state's level gone with it; run rethrows") {
    type R = St +: Pure
    val p = perform[St, R, Int](St.Get).map(s => if s == 0 then throw boom else s).handle(state(0))
    assertEquals(failed(p.step), boom)
    assertEquals(intercept[RuntimeException](p.run), boom)
    assertEquals(perform[St, R, Int](St.Get).map(_ + 1).handle(state(0)).run, (0, 1))
  }

  test("a throw in each kind of user code: a bind, a value, a clause, a ret, a deferral") {
    type R = Ask +: Pure
    def thrower[A]: A = throw boom
    val inBind = perform[Ask, R, Int](Ask.Get).flatMap(_ => thrower[Int ! R]).handle(reader(1))
    val inValue = perform[Ask, R, Int](Ask.Get).handle(new Answering[Ask, Pure, Int, Int]:
      def ret(a: Int) = pure(a)
      def value[X](op: Ask[X]): X = thrower)
    val inClause = perform[Ask, R, Int](Ask.Get).handle(new Deep[Ask, Pure, Int, Int]:
      def ret(a: Int) = pure(a)
      def apply[X](op: Ask[X], k: X => Int ! Pure) = thrower)
    val inRet = perform[Ask, R, Int](Ask.Get).handle(new Answering[Ask, Pure, Int, Int]:
      def ret(a: Int) = thrower
      def value[X](op: Ask[X]): X = op match { case Ask.Get => 1 })
    val inDefer = Cont.defer[Pure, Int](thrower)
    for p <- List(inBind, inValue, inClause, inRet, inDefer) do assertEquals(failed(p.step), boom)
  }

  test("a throw in k resumed twice by choose: the whole run Failed, the other path not run") {
    type R = Choose +: Pure
    var ran = List.empty[Boolean]
    val p = perform[Choose, R, Boolean](Choose.Flip).map(b => { ran = b :: ran; if b then throw boom else 1 }).handle(choose)
    assertEquals(failed(p.step), boom)
    assertEquals(ran, List(true))
  }

  test("a throw in control: a shift0's body, a $'s ret — out of the reset") {
    type L = Int ! Pure
    val inShift: Int ! Pure = Dollar[Pure, L, Nothing, Nothing, Int, Int](
      Shift0[Reset, Pure, Nothing, Nothing, Int, L, Int](Nearest.Here(), _ => throw boom), (a: Int) => Return(a))
    val inRet: Int ! Pure = Dollar[Pure, L, Nothing, Nothing, Int, Int](Return(1), (_: Int) => throw boom)
    assertEquals(failed(inShift.step), boom)
    assertEquals(failed(inRet.step), boom)
  }

  test("a step failed by its driver: the error raised where the operation was") {
    type R = Ask +: Pure
    val p = perform[Ask, R, Int](Ask.Get).map(_ + 1)
    p.step match
      case y: Step.Yield[r, a, e, x] => assertEquals(failed(y.resume.fail(boom)), boom)
      case other => fail(s"not a step: $other")
  }

  test("stack safe: a throw under 100 000 handlers and binds, unwound") {
    def nested[R <: Row](i: Int)(using In[Ask, R]): Int ! R =
      if i == 0 then perform[Ask, R, Int](Ask.Get).map(_ => throw boom)
      else Cont.defer(nested[Ask +: R](i - 1)).handle(reader(i)).map(_ + 1)
    assertEquals(failed(nested[Ask +: Pure](100000).handle(reader(0)).step), boom)
    def deep(i: Int): Int ! Ask +: Pure =
      if i == 0 then pure(0).map(_ => throw boom) else Cont.defer(deep(i - 1)).map(_ + 1)
    assertEquals(failed(deep(100000).handle(reader(1)).step), boom)
  }
