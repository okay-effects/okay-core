package okay.zero

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/** Control's `shift0` up to its reset, as ZeroCaptureBenchmark's `shift`: `k` called `shots` times, the rest `depth`
 * maps; and `n` shift0s in a row, each `k` once in tail position */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ZeroShift0Benchmark:
  @Param(Array("1", "16"))
  var depth: Int = 0
  @Param(Array("1", "8"))
  var shots: Int = 0

  def calls[R <: Row, J](k: Unit => Eff[R, J, Int, Int, Int], i: Int, acc: Int): Eff[R, J, Int, Int, Int] =
    if i == 0 then pure(acc) else k(()).flatMap(r => calls(k, i - 1, acc + r))

  def captured[R <: Row, J](using InReset[R, J, Int, Int]): Eff[R, okay.zero.Level[R, J, Int], Int, Int, Int] =
    var p = shift0[Unit, Int](k => calls(k, shots, 0)).map(_ => 1)
    var i = 0
    while i < depth do
      p = p.map(_ + 1)
      i += 1
    p

  @Benchmark
  def shift0Depth(): Int = run[Int](reset[Int](captured))

@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ZeroShift0ChainBenchmark:
  def chain[R <: Row, J](i: Int, acc: Int)(using InReset[R, J, Int, Int]): Eff[R, okay.zero.Level[R, J, Int], Int, Int, Int] =
    if i == 0 then pure(acc) else shift0[Int, Int](k => k(1)).flatMap(x => chain(i - 1, acc + x))

  @Benchmark
  def shift0Chain(): Int = run[Int](reset[Int](chain(1000, 0)))
