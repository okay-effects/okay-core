package okay.one

import okay.one.Cont.{perform, pure}
import okay.one.row.*
import okay.one.std.*
import okay.one.step.Step
import okay.one.step.Step.*

/** GUARDS: a level of no effect, its finalizer run when it is left, its recover given an error thrown in it */
class TestGuard extends munit.FunSuite:
  type W = Writer % String
  type T = Throws % String
  val boom = RuntimeException("boom")
  def tell[R <: Row](s: String)(using In[W, R]): Unit ! R = Writer.tell[String, R](s)

  test("ensuring: the finalizer after the program, its value kept") {
    type R = W +: Pure
    val p = tell[R]("body").map(_ => 1).ensure(tell[R]("fin"))
    assertEquals(p.handle(Writer.collect).run, (List("body", "fin"), 1))
  }

  test("ensuring and a throw: the finalizer, then the error out — Failed") {
    type R = W +: Pure
    var log = List.empty[String]
    val p = tell[R]("body").map[Int](_ => throw boom).ensure(tell[R]("fin"))
    p.handle(Writer.collect).step match
      case Step.Failed(e) => assertEquals(e, boom)
      case other => fail(s"not failed: $other")
    // the finalizer's tell seen by the writer outside: a writer of its own state, dropped with its level — so a cell
    val cell = new okay.one.handler.Answering[W, Pure, Int, Int]:
      def ret(a: Int) = pure(a)
      def value[X](op: Writer[String, X]): X = op match { case t: Writer.Tell[w] => log = t.w :: log }
    p.handle(cell).step match { case Step.Failed(_) => (); case other => fail(s"not failed: $other") }
    assertEquals(log.reverse, List("body", "fin"))
  }

  test("ensuring under attempt: a raise runs the finalizer, then attempt answers Left") {
    type R = T +: W +: Pure
    val p = tell[R]("body").flatMap(_ => Throws.raise[String, R]("no")).map(_ => 1).ensure(tell[R]("fin"))
    assertEquals(p.handle(Throws.either).handle(Writer.collect).run, (List("body", "fin"), Left("no")))
  }

  test("a finalizer under choose's k resumed twice runs twice") {
    type R = Choose +: W +: Pure
    val p = Choose.flip[R].map(b => if b then 1 else 2).ensure(tell[R]("fin"))
    assertEquals(p.handle(Choose.all).handle(Writer.collect).run, (List("fin", "fin"), List(1, 2)))
  }

  test("recover catches a throw, not a raise") {
    type R = T +: Pure
    val thrown = pure[R, Int](0).map[Int](_ => throw boom).recover(e => pure(if e eq boom then 7 else -1))
    assertEquals(thrown.handle(Throws.either).run, Right(7))
    val raised = Throws.raise[String, R]("no").map(_ => 0).recover(_ => pure(-1))
    assertEquals(raised.handle(Throws.either).run, Left("no"))
  }

  test("guards nested: unwound innermost first; a finalizer's throw replaces the error") {
    type R = W +: Pure
    var log = List.empty[String]
    val cell = new okay.one.handler.Answering[W, Pure, Int, Int]:
      def ret(a: Int) = pure(a)
      def value[X](op: Writer[String, X]): X = op match { case t: Writer.Tell[w] => log = t.w :: log }
    val second = RuntimeException("second")
    val p = pure[R, Int](0).map[Int](_ => throw boom).ensure(tell[R]("inner"))
      .ensure(tell[R]("outer").map(_ => throw second))
    p.handle(cell).step match
      case Step.Failed(e) => assertEquals(e, second)
      case other => fail(s"not failed: $other")
    assertEquals(log.reverse, List("inner", "outer"))
  }

  test("an abort past guards and states: each finalizer, the states dropped") {
    type R = T +: State % Int +: W +: Pure
    val p = State.put[Int, R](5).flatMap(_ => Throws.raise[String, R]("no")).map(_ => 1)
      .ensure(State.get[Int, R].flatMap(s => tell[R](s"fin $s")))
    assertEquals(p.handle(Throws.either).handle(State(0)).handle(Writer.collect).run, (List("fin 5"), (5, Left("no"))))
  }

/** a step failed, cancelled: the error raised inside the rest's levels — their guards asked */
class TestGuardStep extends munit.FunSuite:
  type W = Writer % String
  val boom = RuntimeException("boom")
  var log = List.empty[String]
  def cell[T <: Row, A] = new okay.one.handler.Answering[W, T, A, A]:
    def ret(a: A) = pure(a)
    def value[X](op: Writer[String, X]): X = op match { case t: Writer.Tell[w] => log = t.w :: log }
  type R = Reader % Int +: W +: Pure
  def ask: Int ! R = Reader.ask[Int, R]
  def tell(s: String): Unit ! R = Writer.tell[String, R](s)

  def first[A](s: Step[Reader % Int +: Pure, A]): Step.Resume[Int, Reader % Int +: Pure, A] = s match
    case y: Step.Yield[r, a, e, x] => y.in match
      case _: In.Here[f, t] => Step.Resume[Int, Reader % Int +: Pure, A](
        (n: Int) => y.resume(asked(y.op, n)), e => y.resume.fail(e), () => y.resume.cancel())
      case th: In.There[f, g, t] => In.absurd(th.in)
    case other => throw RuntimeException(s"not a step: $other")
  def asked[X](op: Reader[Int, X], n: Int): X = op match { case _: Reader.Ask[w] => n }

  test("resume.fail: the finalizers in the rest run, then Failed") {
    log = Nil
    val p = ask.map(_ + 1).ensure(tell("fin")).handle(cell[Reader % Int +: Pure, Int])
    first(p.step).fail(boom) match
      case Step.Failed(e) => assertEquals(e, boom)
      case other => fail(s"not failed: $other")
    assertEquals(log, List("fin"))
  }

  test("resume.fail: a recover in the rest given the error") {
    val p = ask.map(_ + 1).recover(e => pure(if e eq boom then 7 else -1)).handle(cell[Reader % Int +: Pure, Int])
    first(p.step).fail(boom) match
      case Step.Done(v) => assertEquals(v, 7)
      case other => fail(s"not recovered: $other")
  }

  test("cancel: the finalizers run, no recover asked, Cancelled") {
    log = Nil
    val p = ask.map(_ + 1).recover(_ => pure(-1)).ensure(tell("fin")).handle(cell[Reader % Int +: Pure, Int])
    first(p.step).cancel() match
      case Step.Cancelled() => ()
      case other => fail(s"not cancelled: $other")
    assertEquals(log, List("fin"))
  }

  test("cancel under uncancelable: the step given back, the cancel fired when the mask is left") {
    log = Nil
    val p = ask.flatMap(n => tell(s"in $n").map(_ => n)).uncancelable.flatMap(n => tell("after").map(_ => n))
      .ensure(tell("fin")).handle(cell[Reader % Int +: Pure, Int])
    val s = p.step
    val r = first(s)
    s match
      case y: Step.Yield[r, a, e, x] => assert(y.resume.cancel() eq s, "the same step given back")
      case other => fail(s"not a step: $other")
    r(5) match
      case Step.Cancelled() => ()
      case other => fail(s"not cancelled at the mask's exit: $other")
    assertEquals(log.reverse, List("in 5", "fin"))
  }
