package okay.core

import okay.cont.{Cont, Free, At}

/** A MONAD of master's `Effects`, for the tests written once: its run of a program at the top, over `Pure` */
trait Backend:
  def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A
  /** whether a run of thousands of steps keeps its stack: the textbook CPS does not */
  def stackSafe: Boolean = true

/** a suite written once, run on the monad `on`: on one that is not stack safe, each test on a thread of a stack of
 * its own, a large one — the CPS of a program as deep as its run */
abstract class BackendSuite(on: Backend) extends munit.FunSuite:
  def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A = on.run(body)
  override def munitTestTransforms: List[TestTransform] = super.munitTestTransforms :+ TestTransform("stack", t =>
    if on.stackSafe then t else t.withBody(() => BackendSuite.onLargeStack(t.body())))

object BackendSuite:
  /** `a` on a stack large enough for a program as deep as its run, where the platform has one (`LargeStack`) */
  def onLargeStack[A](a: => A): A = LargeStack(a)

object Backend:
  /** `Cont`: `Effects.At` the context */
  object cont extends Backend:
    def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A =
      okay.cont.run(body(using summon[okay.cont.At[Pure, A]]))

  /** shift0 and `$` in CPS, the textbook reference: `Cps0.At` the context */
  object cps0 extends Backend:
    override def stackSafe: Boolean = false
    def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A =
      Cps0.run(body(using summon[Cps0.At[Pure, A, A]]))

/** the suites written once, on the textbook reference */
class TestEffectsOnCps0 extends TestEffectsOn(Backend.cps0)
class TestContextOnCps0 extends TestContextOn(Backend.cps0)
class TestControlOnCps0 extends TestControlOn(Backend.cps0)
class TestTogetherOnCps0 extends TestTogetherOn(Backend.cps0)
class TestDynamicOnCps0 extends TestDynamicOn(Backend.cps0)
class TestPStateOnCps0 extends TestPStateOn(Backend.cps0)
