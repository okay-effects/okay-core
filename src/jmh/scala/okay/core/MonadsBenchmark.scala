package okay.core

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import Control.{reset, shift}
import okay.std.*
import okay.zero.Fx

/**
 * THE SAME PROGRAMS ON EACH MONAD: written once over master's `Effects` and `Control` and `okay.std`, run on the
 * monad `backend` — `cont` (`Cont`, `Effects.At`) or `zero` (`Cont0`, `okay.zero.Fx`); `n` operations a run.
 *
 *  - pure: `n` binds, tail calls, no effect
 *  - answered / general / forwarded: `n` operations of a handler answering in place, of its clause written out,
 *    across a handler of another effect; across 16 handlers (`forwarded16`)
 *  - state, writer, throwing, choose: master's EffectsBenchmark
 *  - collect: `n` elements of a lazy body collected
 *  - capture: a shift under a reset, the rest 16 maps, `k` 8 times — not divided by `n`
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class MonadsBenchmark:

  @Param(Array("cont", "zero"))
  var backend: String = ""

  @Param(Array("1000"))
  var n: Int = 0

  def run[A](body: (c: Delimited { type R = Pure; type S = A }) ?=> c.M[Pure, A, A, A]): A =
    if backend == "cont" then Effects.run(body(using summon[Effects.At[Pure, A]])) else Fx.run(body(using summon[Fx[Pure, A, A]]))

  def isEven(i: Int)(using c: Effects): c.M[c.R, c.S, c.S, Boolean] =
    if i == 0 then c.pure(true) else c.pure(()).flatMap(_ => isOdd(i - 1))
  def isOdd(i: Int)(using c: Effects): c.M[c.R, c.S, c.S, Boolean] =
    if i == 0 then c.pure(false) else c.pure(()).flatMap(_ => isEven(i - 1))

  @Benchmark
  def pure(): Boolean = run(isEven(n))

  def isEvenT(i: Int)(using c: Effects, t: Tick :? c.R): c.M[c.R, c.S, c.S, Boolean] =
    if i == 0 then c.pure(true) else perform(Tick.Value(i)).flatMap(_ => isOddT(i - 1))
  def isOddT(i: Int)(using c: Effects, t: Tick :? c.R): c.M[c.R, c.S, c.S, Boolean] =
    if i == 0 then c.pure(false) else perform(Tick.Value(i)).flatMap(_ => isEvenT(i - 1))

  def ticking[A](using c: Effects): Handler[c.M, Tick, c.R, c.S, A, A] = new Answering[c.M, Tick, c.R, c.S, A, A]:
    def ret(a: A): A = a
    def value[X](op: Tick[X]): X = op match
      case Tick.Value(a) => a
  def tickingK[A](using c: Effects): Handler[c.M, Tick, c.R, c.S, A, A] = new Handler[c.M, Tick, c.R, c.S, A, A]:
    def ret(a: A): A = a
    def apply[X](op: Tick[X], k: X => c.M[c.R, c.S, c.S, A]): c.M[c.R, c.S, c.S, A] = op match
      case Tick.Value(a) => k(a)
  def skipping[A](using c: Effects): Handler[c.M, Skip, c.R, c.S, A, A] = new Answering[c.M, Skip, c.R, c.S, A, A]:
    def ret(a: A): A = a
    def value[X](op: Skip[X]): X = op match
      case Skip.Value(a) => a

  @Benchmark
  def answered(): Boolean = run(Effects.handle(ticking)(isEvenT(n)))

  @Benchmark
  def general(): Boolean = run(Effects.handle(tickingK)(isEvenT(n)))

  @Benchmark
  def forwarded(): Boolean = run(Effects.handle(ticking)(Effects.handle(skipping)(isEvenT(n))))

  def skips(d: Int)(using c: Effects, t: Tick :? c.R): c.M[c.R, c.S, c.S, Boolean] =
    if d == 0 then isEvenT(n) else Effects.handle(skipping)(skips(d - 1))

  @Benchmark
  def forwarded16(): Boolean = run(Effects.handle(ticking)(skips(16)))

  def counting(i: Int)(using c: Effects, s: State % Int :? c.R): c.M[c.R, c.S, c.S, Int] =
    if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => counting(i - 1))

  @Benchmark
  def state(): Int = run(okay.std.state[Int](0)(counting(n)))._1

  def telling(i: Int)(using c: Effects, w: Writer % Int :? c.R): c.M[c.R, c.S, c.S, Unit] =
    if i == 0 then c.pure(()) else tell(i).flatMap(_ => telling(i - 1))

  @Benchmark
  def writer(): Int = run(okay.std.writer[Int, Unit](telling(n)))._1.length

  def raising(i: Int, acc: Int)(using c: Effects): c.M[c.R, c.S, c.S, Int] =
    if i == 0 then c.pure(acc)
    else okay.std.throws[String, Int](raise[String, Int]("no")).flatMap(e => raising(i - 1, acc + e.fold(_.length, identity)))

  @Benchmark
  def throwing(): Int = run(raising(n, 0))

  @Benchmark
  def choose(): Int = run(okay.std.choose[Int](among(0 until n).map(_ + 1))).length

  def fibs(i: Int, a: Long, b: Long)(using c: Effects, e: Emit % Long :? c.R): c.M[c.R, c.S, c.S, Unit] =
    if i == 0 then c.pure(()) else yield_(a).flatMap(_ => fibs(i - 1, b, a + b))

  @Benchmark
  def collect(): Long = run(okay.std.collect[Long, Unit](fibs(n, 0, 1)))._1.sum

  def shots(using c: Control)(k: Unit => c.M[c.R, Int, Int, Int], i: Int, acc: Int): c.M[c.R, Int, Int, Int] =
    if i == 0 then c.pure(acc) else k(()).flatMap(r => shots(using c)(k, i - 1, acc + r))

  def captured(using c: Control { type S = Int }): c.M[c.R, Int, Int, Int] =
    var p = shift[Unit, Int](k => shots(using c)(k, 8, 0)).map(_ => 1)
    var i = 0
    while i < 16 do
      p = p.map(_ + 1)
      i += 1
    p

  @Benchmark
  def capture(): Int = run[Int](reset[Int](captured))
