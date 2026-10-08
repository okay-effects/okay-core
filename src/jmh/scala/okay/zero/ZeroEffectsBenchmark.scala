package okay.zero

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.zero.std.*

/** master's EffectsBenchmark on okay.zero, the same programs: stateGetPut (answered in place), its `!` forms,
 * writerTell (the general clause), throwsRaise (a handler installed and aborted), chooseAmong (multi-shot) */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ZeroEffectsBenchmark:

  @Param(Array("1000"))
  var n: Int = 0

  def counting(i: Int)(using c: Ctx, s: State % Int :? c.R): c.P[Int] =
    if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => counting(i - 1))

  @Benchmark
  def stateGetPutZero(): Int = run(state[Int](0)(counting(n)))._1

  def countingBang(i: Int): Int ! State % Int =
    if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => countingBang(i - 1))

  @Benchmark
  def stateGetPutBangZero(): Int = run(state[Int](0)(countingBang(n)))._1

  def telling(i: Int)(using c: Ctx, w: Writer % Int :? c.R): c.P[Unit] =
    if i == 0 then pure(()) else tell(i).flatMap(_ => telling(i - 1))

  @Benchmark
  def writerTellZero(): Int = run(writer[Int, Unit](telling(n)))._1.length

  def raising(i: Int, acc: Int)(using c: Ctx): c.P[Int] =
    if i == 0 then pure(acc)
    else throws[String, Int](raise[String, Int]("no")).flatMap(e => raising(i - 1, acc + e.fold(_.length, identity)))

  @Benchmark
  def throwsRaiseZero(): Int = run(raising(n, 0))

  @Benchmark
  def chooseAmongZero(): Int = run(choose[Int](among(0 until n).map(_ + 1))).length
