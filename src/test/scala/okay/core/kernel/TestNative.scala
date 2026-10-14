package okay.core.kernel

import munit.FunSuite
import okay.kernel.*
import okay.kernel.{Control => KControl}
import okay.kernel.std.{Choose, Handlers, Reader, Results, State, Throws, Writer}

/** okay-kernel's programs, written once over its `Effects`, on okay-core's machines natively — on every platform
 * (the TCK and the reference machine, the JVM's, in the module `kernel`) */
abstract class NativeSuite(name: String, e: Effects) extends FunSuite:
  val st: Key[okay.kernel.std.State[Integer]] = okay.kernel.std.State.key[Integer](Type.of("okay.core.kernel.s"))
  val log: Key[okay.kernel.std.Writer[String]] = okay.kernel.std.Writer.key[String](Type.of("okay.core.kernel.w"))
  val err: Key[okay.kernel.std.Throws[String]] = okay.kernel.std.Throws.key[String](Type.of("okay.core.kernel.t"))
  val rd: Key[okay.kernel.std.Reader[Integer]] = okay.kernel.std.Reader.key[Integer](Type.of("okay.core.kernel.r"))

  def prog[A](f: Effects => Prog[A]): Program[A] = new Program[A] { def in(e: Effects): Prog[A] = f(e) }
  def fn[A, B](f: A => B): Fn[A, B] = new Fn[A, B] { def apply(a: A): B = f(a) }
  def int(i: Int): Integer = Integer.valueOf(i)
  def run[A](p: Program[A]): A = e.computation(p, java.util.Collections.emptySet[Type]()).start(Answers.NONE) match
    case d: Step.Done[A @unchecked] => d.value()
    case other => throw IllegalStateException(other.toString)

  def count(n: Int): Program[Integer] = prog { x =>
    if n == 0 then Handlers.get[Integer](st).in(x)
    else x.flatMap(Handlers.modify[Integer](st, fn((s: Integer) => int(s + 1))).in(x), fn((_: Void) => count(n - 1).in(x)))
  }

  test(s"$name: a kernel program is the machine's own nodes") {
    assert(e.pure(int(1)) match { case p: OnCore.P[?] => p.c.isInstanceOf[okay.one.Cont[?, ?, ?, ?]]; case _ => false })
  }

  test(s"$name: state 100 000 operations in constant stack, answered in place") {
    assertEquals(run(Handlers.state[Integer, Integer](st, int(0), count(100000))).state().intValue, 100000)
  }

  test(s"$name: reader, writer, a raise with its finalizer, choose") {
    var fins = 0
    val body = prog { x =>
      x.flatMap(Handlers.ask[Integer](rd).in(x), fn((n: Integer) =>
        x.flatMap(Handlers.among[Integer](java.util.Arrays.asList(n, int(n + 1))).in(x), fn((v: Integer) =>
          x.flatMap(Handlers.tell[String](log, s"v=$v").in(x), fn((_: Void) =>
            if v == n + 1 then x.ensuring(Handlers.raise[String, Integer](err, "odd"), prog { y => fins += 1; y.pure(null: Void) })
            else x.pure(int(v * 10))))))))
    }
    type Rs = java.util.List[Results.Result[String, Integer]]
    val r = run(Handlers.reader[Integer, Results.Logged[String, Rs]](rd, int(4),
      Handlers.writer[String, Rs](log, Handlers.all[Results.Result[String, Integer]](Handlers.throwsOf[String, Integer](err, body)))))
    assertEquals(r.log(), java.util.Arrays.asList("v=4", "v=5"))
    assertEquals(r.value().get(0), new Results.Ok[String, Integer](int(40)))
    assert(r.value().get(1).isInstanceOf[Results.Err[?, ?]] && fins == 1)
  }

  test(s"$name: shift0 and $$, through recover") {
    val twice = prog(x => KControl.of(x).shift0[Integer, Integer, Integer](fn((kk: Fn[Integer, Prog[Integer]]) => x.flatMap(kk(int(1)), fn((a: Integer) => kk(a))))))
    assertEquals(run(prog(x => KControl.of(x).dollar[Integer, Integer, Integer](twice, fn((v: Integer) => x.pure(int(v + 10)))))).intValue, 21)
    val through = prog(x => KControl.of(x).dollar[Integer, Integer, Integer](prog(y => y.recover[Integer](
      prog(z => KControl.of(z).shift0[Integer, Integer, Integer](fn((kk: Fn[Integer, Prog[Integer]]) => z.map(kk(int(1)), fn((a: Integer) => int(a + 100)))))),
      fn((_: Throwable) => y.pure(int(-1))))), fn((v: Integer) => x.pure(int(v * 2)))))
    assertEquals(run(through).intValue, 102)
  }

  test(s"$name: export — handed out, k multi-shot; k.cancel runs the finalizer") {
    val c = e.computation(prog(x => x.map(Handlers.ask[Integer](rd).in(x), fn((n: Integer) => int(n + 1)))), java.util.Collections.singleton(rd.`type`()))
    val y = c.start(Answers.NONE).asInstanceOf[Step.Yield[Integer, Integer]]
    assertEquals(y.k().resume(int(41)), new Step.Done[Integer](int(42)))
    assertEquals(y.k().resume(int(1)), new Step.Done[Integer](int(2)))
    var fins = 0
    val f = e.computation(prog(x => x.ensuring(Handlers.ask[Integer](rd), prog { z => fins += 1; z.pure(null: Void) })), java.util.Collections.singleton(rd.`type`()))
    assert(f.start(Answers.NONE).asInstanceOf[Step.Yield[Integer, Integer]].k().cancel().isInstanceOf[Step.Cancelled[?]] && fins == 1)
  }

  test(s"$name: import — another machine's computation under these handlers") {
    val other: Effects = e // its own: an import through the same implementation, on every platform
    val inner = other.computation(prog { x =>
      x.flatMap(Handlers.ask[Integer](rd).in(x), fn((n: Integer) => x.map(Handlers.among[Integer](java.util.Arrays.asList(n, int(n + 1))).in(x),
        fn((v: Integer) => int(v * 10)))))
    }, java.util.Collections.emptySet[Type]())
    assertEquals(run(Handlers.reader[Integer, java.util.List[Integer]](rd, int(4), Handlers.all[Integer](prog(x => x.foreign(inner))))),
      java.util.Arrays.asList(int(40), int(50)))
  }

class TestNative extends NativeSuite("one", OnCore)
