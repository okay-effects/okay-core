package okay.async

import okay.core.*
import okay.std.*
import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/** `Async` on each monad, `backend`: `n` awaits, each callback answered at once, a run of `Async.run` — its slot
 * raced, its result waited on */
@JmhState(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(1)
class AsyncBenchmark:
  @Param(Array("cont", "zero"))
  var backend: String = ""
  @Param(Array("1000"))
  var n: Int = 0

  def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A =
    backend match
      case "cont" => okay.cont.run(body(using summon[okay.cont.At[Pure, A]]))
      case _ => okay.zero.run(body(using summon[okay.zero.At[Pure, A, A]]))

  def awaits(i: Int, acc: Int)(using c: Effects, a: Async :? c.Effs): c.Prog[Int] =
    if i == 0 then c.pure(acc) else await[Int](cb => cb(Right(1))).flatMap(x => awaits(i - 1, acc + x))

  @Benchmark
  def asyncAwaits(): Int = run(throws[Throwable, Int](Async.run(awaits(n, 0)))).getOrElse(-1)
