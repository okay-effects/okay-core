package okay.zero

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.core.Effects
import okay.core.Control.reset

/** `shift0`, `Cont0`'s own, up to its reset: `k` called `shots` times, the rest `depth` maps; and 1000 shift0s in a
 * row, each `k` once in tail position */
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

  def calls(using c: Effects)(k: Unit => c.P[Int], i: Int, acc: Int): c.P[Int] =
    if i == 0 then c.pure(acc) else k(()).flatMap(r => calls(using c)(k, i - 1, acc + r))

  def captured[R <: Row, J](using r: Fx[R, okay.zero.Level[R, J, Int], Int]): r.P[Int] =
    var p = shift0[Unit, Int](k => calls(k, shots, 0)).map(_ => 1)
    var i = 0
    while i < depth do
      p = p.map(_ + 1)
      i += 1
    p

  @Benchmark
  def shift0Depth(): Int = Fx.run[Int](reset[Int](captured))

@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ZeroShift0ChainBenchmark:
  def chain[R <: Row, J](i: Int, acc: Int)(using r: Fx[R, okay.zero.Level[R, J, Int], Int]): r.P[Int] =
    if i == 0 then r.pure(acc) else shift0[Int, Int](k => k(1)).flatMap(x => chain(i - 1, acc + x))

  @Benchmark
  def shift0Chain(): Int = Fx.run[Int](reset[Int](chain(1000, 0)))
