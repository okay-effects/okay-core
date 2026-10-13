package okay.core.kernel

import munit.FunSuite
import okay.kernel.*
import okay.kernel.{Control => KControl}
import okay.kernel.{Control => KControl}
import okay.kernel.std.{Choose, Handlers, Results, State}
import okay.kernel.steps.Steps
import okay.kernel.types.typeOf

/** the reference machine's suite (okay-kernel's TestSteps), the same twelve checks, on an `Effects` of the bridge:
 * the standard handlers written once, control, failures, finalizers, cancellation, the protocol */
abstract class OnCoreSuite(name: String, e: Effects) extends FunSuite:
  val st: Key[okay.kernel.std.State[Integer]] = okay.kernel.std.State.key[Integer](typeOf[Integer])
  val log: Key[okay.kernel.std.Writer[String]] = okay.kernel.std.Writer.key[String](typeOf[String])
  val err: Key[okay.kernel.std.Throws[String]] = okay.kernel.std.Throws.key[String](typeOf[String])
  val rd: Key[okay.kernel.std.Reader[Integer]] = okay.kernel.std.Reader.key[Integer](typeOf[Integer])

  def prog[A](f: Effects => Prog[A]): Program[A] = new Program[A] { def in(e: Effects): Prog[A] = f(e) }
  def fn[A, B](f: A => B): Fn[A, B] = new Fn[A, B] { def apply(a: A): B = f(a) }
  def run[A](p: Program[A]): A = OnCoreSuite.run(e, p)

  def count(n: Int): Program[Integer] = prog { e =>
    if n == 0 then Handlers.get[Integer](st).in(e)
    else e.flatMap(Handlers.modify[Integer](st, fn((s: Integer) => Integer.valueOf(s + 1))).in(e), fn((_: Void) => count(n - 1).in(e)))
  }

  test(s"$name: state as a cell, 100 000 operations in constant stack") {
    val r = run(Handlers.state[Integer, Integer](st, 0, count(100000)))
    assertEquals(r.state().intValue, 100000)
  }

  test(s"$name: writer and state, handled in either order; reader answered in place") {
    val body = prog { e =>
      e.flatMap(Handlers.ask[Integer](rd).in(e), fn((n: Integer) =>
        e.flatMap(Handlers.tell[String](log, s"n=$n").in(e), fn((_: Void) =>
          e.flatMap(Handlers.put[Integer](st, n).in(e), fn((_: Void) => Handlers.get[Integer](st).in(e)))))))
    }
    val r = run(Handlers.reader[Integer, Results.Logged[String, Results.Final[Integer, Integer]]](rd, 7,
      Handlers.writer[String, Results.Final[Integer, Integer]](log, Handlers.state[Integer, Integer](st, 0, body))))
    assertEquals(r.log(), java.util.List.of("n=7"))
    assertEquals(r.value().state().intValue, 7)
    val swapped = run(Handlers.reader[Integer, Results.Final[Integer, Results.Logged[String, Integer]]](rd, 7,
      Handlers.state[Integer, Results.Logged[String, Integer]](st, 0, Handlers.writer[String, Integer](log, body))))
    assertEquals((swapped.state().intValue, swapped.value().log()), (7, java.util.List.of("n=7")))
  }

  test(s"$name: a raise ends the body, its finalizer run; handleWith answers it") {
    var finalized = false
    val body = prog { e => e.ensuring(Handlers.raise[String, Integer](err, "boom"), prog { e2 => finalized = true; e2.pure(null: Void) }) }
    val r = run(Handlers.throwsOf[String, Integer](err, body))
    assert(r.isInstanceOf[Results.Err[?, ?]] && finalized)
    assertEquals(run(Handlers.handleWith[String, Integer](err, body, fn((m: String) => prog(_.pure(Integer.valueOf(m.length)))))).intValue, 4)
  }

  test(s"$name: choose: every path; the first") {
    val pair = prog { e =>
      e.flatMap(Handlers.among[Integer](java.util.List.of(1, 2)).in(e), fn((a: Integer) =>
        e.map(Handlers.among[Integer](java.util.List.of(10, 20)).in(e), fn((b: Integer) => Integer.valueOf(a * 100 + b)))))
    }
    assertEquals(run(Handlers.all(pair)), java.util.List.of[Integer](110, 120, 210, 220))
    assertEquals(run(Handlers.first(pair)).get().intValue, 110)
  }

  test(s"$name: shift0 and $$: the laws") {
    val k = fn((x: Integer) => e.pure(Integer.valueOf(x * 2)))
    assertEquals(run(prog(e => KControl.of(e).dollar[Integer, Integer, Integer](prog(_.pure(Integer.valueOf(21))), k))).intValue, 42)
    val shifted = prog(e => KControl.of(e).shift0[Integer, Integer, Integer](fn((kk: Fn[Integer, Prog[Integer]]) => e.flatMap(kk(1), fn((a: Integer) => kk(a))))))
    assertEquals(run(prog(e => KControl.of(e).dollar[Integer, Integer, Integer](shifted, fn((x: Integer) => e.pure(Integer.valueOf(x + 10)))))).intValue, 21)
    // shift0's body runs outside its delimiter: a shift0 in it reaches the next one out; a handler is a delimiter
    val two = prog(e => KControl.of(e).shift0[Integer, Integer, Integer](fn((k1: Fn[Integer, Prog[Integer]]) =>
      KControl.of(e).shift0[Integer, Integer, Integer](fn((k2: Fn[Integer, Prog[Integer]]) => e.flatMap(k1(1), fn((a: Integer) => k2(a))))))))
    val nested = prog(e => KControl.of(e).dollar[Integer, Integer, Integer](prog(e2 => KControl.of(e2).dollar[Integer, Integer, Integer](two,
      fn((x: Integer) => e2.pure(Integer.valueOf(x + 10))))), fn((x: Integer) => e.pure(Integer.valueOf(x * 2)))))
    assertEquals(run(nested).intValue, 22)
    val underReader = Handlers.reader[Integer, Integer](rd, 5, prog(e => KControl.of(e).shift0[Integer, Integer, Integer](
      fn((kk: Fn[Integer, Prog[Integer]]) => e.flatMap(kk(1), fn((a: Integer) => e.pure(Integer.valueOf(a + 100))))))))
    assertEquals(run(underReader).intValue, 101)
    // recover and ensuring are no delimiters: shift0 passes them to the $ around
    val throughRecover = prog(e => KControl.of(e).dollar[Integer, Integer, Integer](prog(e2 => e2.recover[Integer](
      prog(e3 => KControl.of(e3).shift0[Integer, Integer, Integer](fn((kk: Fn[Integer, Prog[Integer]]) => e3.map(kk(1), fn((a: Integer) => Integer.valueOf(a + 100)))))),
      fn((_: Throwable) => e2.pure(Integer.valueOf(-1))))), fn((x: Integer) => e.pure(Integer.valueOf(x * 2)))))
    assertEquals(run(throughRecover).intValue, 102)
  }

  test(s"$name: a k resumed under other handlers than it was made under: its operations go to them, as the reference's") {
    val t = Key.of[Void](Type.of("okay.core.kernel.TestOnCore.t"))
    final case class Tick() extends Op[Void] { def key(): Key[?] = t }
    // the clause resumes k under a reader of its own: the body's ask, after the tick, is that reader's
    val p = prog(e => e.handle[Integer, Integer](new General[Integer, Integer] {
      def key(): Key[?] = t
      def ret(a: Integer, e2: Effects): Prog[Integer] = e2.pure(a)
      def apply[X](op: Op[X], k: Fn[X, Prog[Integer]], e2: Effects): Prog[Integer] =
        Handlers.reader[Integer, Integer](rd, 9, prog(_ => k(null.asInstanceOf[X]))).in(e2)
    }, prog(e2 => e2.flatMap(e2.perform(Tick()), fn((_: Void) => e2.map(ask.in(e2), fn((n: Integer) => Integer.valueOf(n + 1))))))))
    assertEquals(run(p).intValue, 10)
    assertEquals(Steps.run(p).intValue, 10)
  }

  test(s"$name: a generator: values pulled one by one") {
    val em = okay.kernel.std.Emit.key[Integer](typeOf[Integer])
    val three = prog { e =>
      e.flatMap(Handlers.send[Integer](em, 1).in(e), fn((_: Void) => e.flatMap(Handlers.send[Integer](em, 2).in(e), fn((_: Void) => e.pure("done")))))
    }
    def pull(g: Results.Gen[Integer, String], acc: List[Int]): (List[Int], String) = g match
      case n: Results.Next[Integer @unchecked, String @unchecked] => pull(run(n.rest()), acc :+ n.value().intValue)
      case end: Results.End[Integer @unchecked, String @unchecked] => (acc, end.value())
    assertEquals(pull(run(Handlers.generate[Integer, String](em, three)), Nil), (List(1, 2), "done"))
  }

  test(s"$name: a failure is data; recover catches it; an exception of a function is one") {
    val c = e.computation(prog(e => e.map(e.pure(Integer.valueOf(1)), fn((_: Integer) => (throw new IllegalStateException("x")): Integer))), java.util.Set.of[Type]())
    assert(c.start(Answers.NONE).isInstanceOf[Step.Failed[?]])
    assertEquals(run(prog(e => e.recover[Integer](prog(_.failed(new RuntimeException("no"))), fn((t: Throwable) => e.pure(Integer.valueOf(t.getMessage.length)))))).intValue, 2)
    assertEquals(run(prog(e => e.recover[Integer](prog(e2 => e2.map(e2.pure(Integer.valueOf(1)), fn((_: Integer) => (throw new IllegalStateException("x")): Integer))),
      fn((t: Throwable) => e.pure(Integer.valueOf(-1)))))).intValue, -1)
  }

  /** an operation nothing here handles: handed out */
  val ask: Program[Integer] = Handlers.ask[Integer](rd)

  test(s"$name: export: an operation handed out, k resumed — twice, multi-shot") {
    val c = e.computation(prog(e => e.map(ask.in(e), fn((n: Integer) => Integer.valueOf(n + 1)))), java.util.Set.of(rd.`type`()))
    c.start(Answers.NONE) match
      case y: Step.Yield[?, ?] =>
        val k = y.k().asInstanceOf[Resume[Integer, Integer]]
        assertEquals(k.resume(41).asInstanceOf[Step.Done[Integer]].value().intValue, 42)
        assertEquals(k.resume(1).asInstanceOf[Step.Done[Integer]].value().intValue, 2)
        assertEquals(c.effects(), java.util.Set.of(rd.`type`()))
      case s => fail(s"expected a yield: $s")
  }

  test(s"$name: export: answered in place by the host, no yield") {
    val host = new Answers {
      def answers(t: Type): Boolean = t eq rd.`type`()
      def value[X](op: Op[X]): X = Integer.valueOf(41).asInstanceOf[X]
    }
    val c = e.computation(prog(e => e.map(ask.in(e), fn((n: Integer) => Integer.valueOf(n + 1)))), java.util.Set.of(rd.`type`()))
    assertEquals(c.start(host).asInstanceOf[Step.Done[Integer]].value().intValue, 42)
  }

  test(s"$name: k.fail: the computation catches it where the operation was") {
    val c = e.computation(prog(e => e.recover[Integer](ask, fn((t: Throwable) => e.pure(Integer.valueOf(-1))))), java.util.Set.of(rd.`type`()))
    val y = c.start(Answers.NONE).asInstanceOf[Step.Yield[Integer, Integer]]
    assertEquals(y.k().fail(new RuntimeException("x")).asInstanceOf[Step.Done[Integer]].value().intValue, -1)
  }

  test(s"$name: k.cancel: finalizers run, not catchable; inside uncancelable it waits") {
    var finalized = 0
    val fin = prog { e => finalized += 1; e.pure(null: Void) }
    val c = e.computation(prog(e => e.recover[Integer](prog(e2 => e2.ensuring(ask, fin)), fn((_: Throwable) => e.pure(Integer.valueOf(0))))), java.util.Set.of(rd.`type`()))
    val y = c.start(Answers.NONE).asInstanceOf[Step.Yield[Integer, Integer]]
    assert(y.k().cancel().isInstanceOf[Step.Cancelled[?]] && finalized == 1, "cancelled, finalizer once, recover not taken")
    val u = e.computation(prog(e => e.uncancelable(ask)), java.util.Set.of(rd.`type`()))
    val wait = u.start(Answers.NONE).asInstanceOf[Step.Yield[Integer, Integer]].k().cancel()
    assert(wait.isInstanceOf[Step.Yield[?, ?]], "the region waits")
    assert(wait.asInstanceOf[Step.Yield[Integer, Integer]].k().resume(5).isInstanceOf[Step.Cancelled[?]], "its end cancels")
  }

  test(s"$name: import: another's program under this one's handlers — in place and by k") {
    val inner = e.computation(prog { e =>
      e.flatMap(ask.in(e), fn((n: Integer) => e.map(Handlers.among[Integer](java.util.List.of(n, n + 1)).in(e), fn((x: Integer) => Integer.valueOf(x * 10)))))
    }, java.util.Set.of(rd.`type`(), Choose.TYPE))
    val outer = Handlers.reader[Integer, java.util.List[Integer]](rd, 4, Handlers.all(prog(e => e.foreign(inner))))
    assertEquals(run(outer), java.util.List.of[Integer](40, 50))
  }

  // -- across implementations: the kernel's reference machine and this one, each importing the other's computation

  /** a program asking a reader, choosing, telling, raising on one path: its value a list of results */
  val mixed: Program[Integer] = prog { e =>
    e.flatMap(ask.in(e), fn((n: Integer) =>
      e.flatMap(Handlers.among[Integer](java.util.List.of(n, n + 1, n + 2)).in(e), fn((x: Integer) =>
        e.flatMap(Handlers.tell[String](log, s"x=$x").in(e), fn((_: Void) =>
          if x == n + 1 then Handlers.raise[String, Integer](err, s"odd $x").in(e)
          else e.pure(Integer.valueOf(x * 10))))))))
  }
  val row: java.util.Set[Type] = java.util.Set.of(rd.`type`(), Choose.TYPE, log.`type`(), err.`type`())
  def handled(body: Program[Integer]): Program[Results.Logged[String, java.util.List[Results.Result[String, Integer]]]] =
    Handlers.reader(rd, 4, Handlers.writer(log, Handlers.all(Handlers.throwsOf[String, Integer](err, body))))

  test(s"$name: across implementations — the reference's computation under these handlers, and back: the same") {
    val reference = Steps.run(handled(mixed))
    assertEquals(run(handled(prog(_.foreign(Steps.computation(mixed, row))))), reference)
    assertEquals(Steps.run(handled(prog(_.foreign(e.computation(mixed, row))))), reference)
    assertEquals(reference.log(), java.util.List.of("x=4", "x=5", "x=6"))
  }

object OnCoreSuite:
  def run[A](e: Effects, p: Program[A]): A = e.computation(p, java.util.Set.of[Type]()).start(Answers.NONE) match
    case d: Step.Done[A @unchecked] => d.value()
    case f: Step.Failed[A @unchecked] => throw f.error()
    case _: Step.Cancelled[A @unchecked] => throw IllegalStateException("cancelled")
    case y: Step.Yield[?, ?] => throw IllegalStateException(s"an operation with no handler: ${y.op()}")

class TestOnCoreOne extends OnCoreSuite("one", OnCore)

/** okay-kernel's TCK on each machine, nesting through the reference machine */
class TestTckOne extends okay.kernel.tck.TckSuite("okay-core", OnCore, others = List(Steps))
