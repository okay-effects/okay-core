package okay.async

import okay.core.*
import okay.core.std.*
import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/** `Async`: `n` awaits, each callback answered at once, a run of `Async.run` — its slot raced, its result waited on */
@JmhState(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(1)
class AsyncBenchmark:
  @Param(Array("1000"))
  var n: Int = 0

  def awaits(i: Int, acc: Int): Int ! Async =
    if i == 0 then pure(acc) else await[Int](cb => cb(Right(1))).flatMap(x => awaits(i - 1, acc + x))

  @Benchmark
  def asyncAwaits(): Int = run(throws[Throwable, Int, Pure](Async.run(awaits(n, 0)))).getOrElse(-1)
