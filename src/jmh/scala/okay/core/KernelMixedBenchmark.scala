package okay.core

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.kernel.{Answers, Effects, Fn, Prog, Program, Step, Thunk, Type}
import okay.kernel.std.{Handlers, Reader, Results, State, Throws, Writer}

/**
 * `MixedBenchmark`'S PROGRAM WRITTEN ONCE over okay-kernel's `Effects` — okay-kernel's bench/MixedBenchmark, the same
 * code — run on okay-core's machines natively (`okay.core.kernel.OnCore`): n orders, each priced from a reader's
 * base, its supplier the first of three that fits (a choice), a failure for some (a raise, a handler per order), the
 * total a state, the failures a log. Against `MixedBenchmark`, the same program in okay-core's own words, and okay-kernel's
 * reference machine (`steps`), the same program on a plain interpreter
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class KernelMixedBenchmark:

  @Param(Array("one", "steps"))
  var backend: String = ""

  @Param(Array("1000"))
  var n: Int = 0

  private var e: Effects = null

  @Setup def setup(): Unit = e = backend match
    case "one" => okay.core.kernel.OnCore
    // okay-kernel's reference machine, a plain interpreter of the same program: the yardstick
    case _ => okay.kernel.steps.Steps

  private def prog[A](f: Effects => Prog[A]): Program[A] = new Program[A] { def in(e: Effects): Prog[A] = f(e) }
  private def fn[A, B](f: A => B): Fn[A, B] = new Fn[A, B] { def apply(a: A): B = f(a) }
  private def int(i: Int): Integer = Integer.valueOf(i)

  private val rd = Reader.key[Integer](Type.of(classOf[Integer]))
  private val st = State.key[Integer](Type.of(classOf[Integer]))
  private val log = Writer.key[String](Type.of(classOf[String]))
  private val err = Throws.key[String](Type.of(classOf[String]))
  private val suppliers: java.util.List[Integer] = java.util.Arrays.asList(int(0), int(1), int(2))

  /** one order: its price, or its failure in the log; the total moved by its price */
  private def order(i: Int): Program[Void] = prog { x =>
    x.flatMap(Handlers.ask[Integer](rd).in(x), fn((base: Integer) => {
      val price: Program[Integer] =
        if i % 13 == 0 then Handlers.raise[String, Integer](err, "no stock")
        else prog(y => y.map(Handlers.first[Integer](prog(z => z.flatMap(Handlers.among[Integer](suppliers).in(z), fn((s: Integer) =>
          if (i + s) % 3 == 0 then z.pure(int(base + i % 7 + s)) else Handlers.among[Integer](java.util.Collections.emptyList[Integer]()).in(z))))).in(y),
          fn((o: java.util.Optional[Integer]) => o.orElse(int(0)))))
      x.flatMap(Handlers.throwsOf[String, Integer](err, price).in(x), fn((r: Results.Result[String, Integer]) => r match
        case ok: Results.Ok[String @unchecked, Integer @unchecked] => Handlers.modify[Integer](st, fn((t: Integer) => int(t + ok.value()))).in(x)
        case f: Results.Err[String @unchecked, Integer @unchecked] => Handlers.tell[String](log, f.error()).in(x)))
    }))
  }

  private def orders(i: Int): Program[Void] = prog { x =>
    if i == n then x.pure(null: Void) else x.flatMap(order(i).in(x), fn((_: Void) => x.delay(new Thunk[Prog[Void]] { def get(): Prog[Void] = orders(i + 1).in(x) })))
  }

  @Benchmark
  def orders(): Int =
    val p = Handlers.reader[Integer, Results.Logged[String, Results.Final[Integer, Void]]](rd, int(100),
      Handlers.writer[String, Results.Final[Integer, Void]](log, Handlers.state[Integer, Void](st, int(0), orders(0))))
    val r = e.computation(p, new java.util.HashSet[Type]()).start(Answers.NONE) match
      case d: Step.Done[Results.Logged[String, Results.Final[Integer, Void]] @unchecked] => d.value()
      case other => throw IllegalStateException(other.toString)
    r.value().state().intValue + r.log().size
