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

  test("a translation's error raised where the operation was: a recover in the body under the translating handler sees it") {
    // `W` translated into a program over the row outside that throws: the body's own recover, inside the handler,
    // is asked — not skipped by an error going out of the translating handler
    type R = W +: Pure
    val failing = new okay.one.handler.Translate[W, Pure, Int]:
      def apply[X](op: Writer[String, X]): X ! Pure = Cont.defer[Pure, X](throw boom)
    val body: Int ! R = tell[R]("x").map(_ => 1).recover(e => pure(if e eq boom then 7 else 0))
    assertEquals(body.handle(failing).run, 7)
  }

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

  test("an interrupt thrown by an answering handler's value: the finalizer runs, the interrupt reaches the top as itself") {
    // a fiber's cancel on a blocking scheduler: its park, in the handler's answer, throws InterruptedException —
    // which NonFatal counts fatal, and which must not leave the guard unrun (`Machine.raisable`)
    type R = W +: Pure
    var log = List.empty[String]
    val interrupt = InterruptedException("cancelled")
    val parking = new okay.one.handler.Answering[W, Pure, Int, Int]:
      def ret(a: Int) = pure(a)
      def value[X](op: Writer[String, X]): X = op match
        case t: Writer.Tell[w] => if t.w == "park" then throw interrupt else log = t.w :: log
    val p = tell[R]("park").map(_ => 1).ensure(tell[R]("fin"))
    // not munit's `intercept`, which lets through what NonFatal counts fatal
    val thrown = try { p.handle(parking).run; null } catch case e: InterruptedException => e
    assertEquals(thrown, interrupt)
    assertEquals(log, List("fin"))
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

  test("a guard told of an operation passing out: another sent in its place, a count at each resumption") {
    type R = Choose +: Pure
    var resumed = 0
    var told = List.empty[String]
    val g = new okay.one.handler.Guard[R, Int, Int]:
      def ret(a: Int) = pure(a)
      override def passing[E[_], X](op: E[X]): okay.one.handler.Guard.Out[E, X] = op match
        case Choose.Among(xs) =>
          told = xs.mkString(",") :: told
          // the same effect: a choice of the alternatives reversed
          okay.one.handler.Guard.Out(Choose.Among(xs.reverse).asInstanceOf[E[X]], () => resumed += 1)
    val p = Choose.among[Int, R](Seq(1, 2, 3)).guard(g)
    assertEquals(p.handle(Choose.all).run, List(3, 2, 1))
    assertEquals((told, resumed), (List("1,2,3"), 3))
  }

  test("a guard's passing that throws: raised where the operation is, inside the guard") {
    type R = Choose +: Pure
    var fin = 0
    val g = new okay.one.handler.Guard[R, Int, Int]:
      def ret(a: Int) = pure(a)
      override def finalizer = Cont.defer { fin += 1; pure(()) }
      override def passing[E[_], X](op: E[X]): okay.one.handler.Guard.Out[E, X] = throw boom
    Choose.flip[R].map(_ => 1).guard(g).handle(Choose.all).step match
      case Step.Failed(e) => assertEquals(e, boom)
      case other => fail(s"not failed: $other")
    assertEquals(fin, 1)
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

  test("an abort whose finalizer's operation takes the aborting level into a k: the abort still its level's, on each path") {
    // the finalizer's `among` goes out past `either`'s level to `all`, which takes it into k and resumes it twice:
    // the level is made again each time, the abort pending with it — each path ends in the abort's answer
    type R = T +: Choose +: Pure
    val p = Throws.raise[String, R]("no").map(_ => 1).ensure(Choose.among[Int, R](Seq(1, 2)).map(_ => ()))
    assertEquals(p.handle(Throws.either).handle(Choose.all).run, List(Left("no"), Left("no")))
  }

  test("an abort past guards and states: each finalizer, the states dropped") {
    type R = T +: State % Int +: W +: Pure
    val p = State.put[Int, R](5).flatMap(_ => Throws.raise[String, R]("no")).map(_ => 1)
      .ensure(State.get[Int, R].flatMap(s => tell[R](s"fin $s")))
    assertEquals(p.handle(Throws.either).handle(State(0)).handle(Writer.collect).run, (List("fin 5"), (5, Left("no"))))
  }

/** a step failed, cancelled: the error raised inside the rest's levels — their guards asked */
  // -- a guard at answers: under a `$`, a shift0 passing it, its `k` holding it

  test("ensure under a $: a shift0 passes the guard, k resumed twice runs the finalizer twice, the value through") {
    type R = W +: Pure
    val r = Cont.rows[Delim[R]]
    val body: Cont[Delim[R], Int ! R, Int ! R, Int] = for
      x <- r.shift0[Int, Int](k => k(1).flatMap(a => k(2).map(b => a + b)))
      _ <- Writer.tell[String, Delim[R]](s"got $x")
    yield x * 10
    val guarded = body.ensure(tell[Delim[R]]("fin"))
    val p: Int ! R = Cont.dollar(guarded)((a: Int) => pure(a))
    assertEquals(p.handle(Writer.collect).run, (List("got 1", "fin", "got 2", "fin"), 30))
  }

  test("ensure under a $ and a throw in the rest: the finalizer, the error out") {
    type R = W +: Pure
    val r = Cont.rows[Delim[R]]
    val body: Cont[Delim[R], Int ! R, Int ! R, Int] = for
      x <- r.shift0[Int, Int](k => k(1))
      _ <- Writer.tell[String, Delim[R]]("rest")
    yield if x == 1 then throw boom else x
    var log = List.empty[String]
    val cell = new okay.one.handler.Answering[W, Pure, Int, Int]:
      def ret(a: Int) = pure(a)
      def value[X](op: Writer[String, X]): X = op match { case Writer.Tell(m) => log = m :: log }
    Cont.dollar(body.ensure(tell[Delim[R]]("fin")))((a: Int) => pure(a)).handle(cell).step match
      case Step.Failed(e) =>
        assertEquals(e, boom)
        assertEquals(log.reverse, List("rest", "fin"))
      case other => fail(s"not failed: $other")
  }

  test("uncancelable under a $: the mask held through the shift0, the finalizer run when it is left") {
    type R = W +: Pure
    val r = Cont.rows[Delim[R]]
    val body: Cont[Delim[R], Int ! R, Int ! R, Int] =
      r.shift0[Int, Int](k => k(1).flatMap(a => k(2).map(b => a + b))).uncancelable.ensure(tell[Delim[R]]("fin"))
    assertEquals(Cont.dollar(body)((a: Int) => pure(a)).handle(Writer.collect).run, (List("fin", "fin"), 3))
  }

  test("recover at answers does not compile: what it recovers with is a program of effects") {
    val errors = compileErrors("""
      type R = Pure
      val r = Cont.rows[Delim[R]]
      r.shift0[Int, Int](k => k(1)).guard(okay.one.handler.Guard.recover[Delim[R], Int](_ => pure(0)))""")
    assert(errors.contains("a recovering guard around a program at answers"), errors)
    assert(compileErrors("""pure[Pure, Int](1).guard(okay.one.handler.Guard.recover[Pure, Int](_ => pure(0)))""").isEmpty)
  }

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
      case at: In.At[f, t] => throw RuntimeException(s"not at the head: $at")
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


  test("cancel under uncancelable, then the region fails: the cancel fires at the mask's exit — Cancelled, not recovered") {
    val p = ask.flatMap(_ => pure(1).map(_ => throw boom)).uncancelable.recover(_ => pure(-1)).handle(cell[Reader % Int +: Pure, Int])
    val s = p.step
    s match
      case y: Step.Yield[r, a, e, x] => assert(y.resume.cancel() eq s, "the same step given back")
      case other => fail(s"not a step: $other")
    first(s)(5) match
      case Step.Cancelled() => ()
      case other => fail(s"not cancelled: $other")
  }
