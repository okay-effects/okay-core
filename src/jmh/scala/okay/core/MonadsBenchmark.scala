package okay.core

import okay.cont.{Cont, Free, At}

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import Control.{reset, shift}
import okay.std.*

/**
 * THE SAME PROGRAMS ON EACH MONAD: written once over master's `Effects` and `Control` and `okay.std`, run on the
 * monad `backend` — `cont` (`Cont`, `okay.cont.At`), `zero` (`Cont0`, `okay.zero.At`); `n` operations a run.
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

  def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A =
    backend match
      case "cont" => okay.cont.run(body(using summon[okay.cont.At[Pure, A]]))
      case _ => okay.zero.run(body(using summon[okay.zero.At[Pure, A, A]]))

  def isEven(i: Int)(using c: Effects): c.Prog[Boolean] =
    if i == 0 then c.pure(true) else c.pure(()).flatMap(_ => isOdd(i - 1))
  def isOdd(i: Int)(using c: Effects): c.Prog[Boolean] =
    if i == 0 then c.pure(false) else c.pure(()).flatMap(_ => isEven(i - 1))

  @Benchmark
  def pure(): Boolean = run(isEven(n))

  def isEvenT(i: Int)(using c: Effects, t: Tick :? c.Effs): c.Prog[Boolean] =
    if i == 0 then c.pure(true) else perform(Tick.Value(i)).flatMap(_ => isOddT(i - 1))
  def isOddT(i: Int)(using c: Effects, t: Tick :? c.Effs): c.Prog[Boolean] =
    if i == 0 then c.pure(false) else perform(Tick.Value(i)).flatMap(_ => isEvenT(i - 1))

  def ticking[A](using c: Effects): Handler[c.Prog, Tick, A, A] = new Answering[c.Prog, Tick, A, A]:
    def ret(a: A): A = a
    def value[X](op: Tick[X]): X = op match
      case Tick.Value(a) => a
  def tickingK[A](using c: Effects): Handler[c.Prog, Tick, A, A] = new Handler[c.Prog, Tick, A, A]:
    def ret(a: A): A = a
    def apply[X](op: Tick[X], k: X => c.Prog[A]): c.Prog[A] = op match
      case Tick.Value(a) => k(a)
  def skipping[A](using c: Effects): Handler[c.Prog, Skip, A, A] = new Answering[c.Prog, Skip, A, A]:
    def ret(a: A): A = a
    def value[X](op: Skip[X]): X = op match
      case Skip.Value(a) => a

  @Benchmark
  def answered(): Boolean = run(Effects.handle(ticking)(isEvenT(n)))

  @Benchmark
  def general(): Boolean = run(Effects.handle(tickingK)(isEvenT(n)))

  @Benchmark
  def forwarded(): Boolean = run(Effects.handle(ticking)(Effects.handle(skipping)(isEvenT(n))))

  def skips(d: Int)(using c: Effects, t: Tick :? c.Effs): c.Prog[Boolean] =
    if d == 0 then isEvenT(n) else Effects.handle(skipping)(skips(d - 1))

  @Benchmark
  def forwarded16(): Boolean = run(Effects.handle(ticking)(skips(16)))

  def counting(i: Int)(using c: Effects, s: State % Int :? c.Effs): c.Prog[Int] =
    if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => counting(i - 1))

  @Benchmark
  def state(): Int = run(okay.std.state[Int](0)(counting(n)))._1

  def telling(i: Int)(using c: Effects, w: Writer % Int :? c.Effs): c.Prog[Unit] =
    if i == 0 then c.pure(()) else tell(i).flatMap(_ => telling(i - 1))

  @Benchmark
  def writer(): Int = run(okay.std.writer[Int, Unit](telling(n)))._1.length

  def raising(i: Int, acc: Int)(using c: Effects): c.Prog[Int] =
    if i == 0 then c.pure(acc)
    else okay.std.throws[String, Int](raise[String, Int]("no")).flatMap(e => raising(i - 1, acc + e.fold(_.length, identity)))

  @Benchmark
  def throwing(): Int = run(raising(n, 0))

  @Benchmark
  def choose(): Int = run(okay.std.choose[Int](among(0 until n).map(_ + 1))).length

  def fibs(i: Int, a: Long, b: Long)(using c: Effects, e: Emit % Long :? c.Effs): c.Prog[Unit] =
    if i == 0 then c.pure(()) else yield_(a).flatMap(_ => fibs(i - 1, b, a + b))

  @Benchmark
  def collect(): Long = run(okay.std.collect[Long, Unit](fibs(n, 0, 1)))._1.sum

  def leftMapping(using c: Effects): c.Prog[Int] =
    var p: c.Prog[Int] = c.pure(0)
    var i = 0
    while i < n do
      p = p.map(_ + 1)
      i += 1
    p

  @Benchmark
  def leftMaps(): Int = run(leftMapping)

  def rightMapping(i: Int)(using c: Effects): c.Prog[Int] = if i == 0 then c.pure(0) else c.delay(rightMapping(i - 1)).map(_ + 1)

  @Benchmark
  def rightMaps(): Int = run(rightMapping(n))

  def shots(using c: Control)(k: Unit => c.Prog[Int], i: Int, acc: Int): c.Prog[Int] =
    if i == 0 then c.pure(acc) else k(()).flatMap(r => shots(using c)(k, i - 1, acc + r))

  /** a shift up to the nearest delimiter, `k` `times` times, the rest `depth` maps */
  def captured(depth: Int, times: Int)(using c: Control { type S = Int }): c.Prog[Int] =
    var p = shift[Unit, Int](k => shots(using c)(k, times, 0)).map(_ => 1)
    var i = 0
    while i < depth do
      p = p.map(_ + 1)
      i += 1
    p

  /** under a reset, the rest 16 maps, `k` 8 times; the rest one map */
  @Benchmark
  def capture(): Int = run[Int](reset[Int](captured(16, 8)))
  @Benchmark
  def captureShallow(): Int = run[Int](reset[Int](captured(1, 8)))

  def nop[A](using c: Effects): Handler[c.Prog, Skip, A, A] = new Answering[c.Prog, Skip, A, A]:
    def ret(a: A): A = a
    def value[X](op: Skip[X]): X = op match
      case Skip.Value(a) => a
  def nopK[A](using c: Effects): Handler[c.Prog, Skip, A, A] = new Handler[c.Prog, Skip, A, A]:
    def ret(a: A): A = a
    def apply[X](op: Skip[X], k: X => c.Prog[A]): c.Prog[A] = op match
      case Skip.Value(a) => k(a)

  /** directly under a handler, answering and general, the rest 16 maps or one, `k` 8 times */
  @Benchmark
  def captureHandled(): Int = run[Int](Effects.handle(nop[Int])(captured(16, 8)))
  @Benchmark
  def captureHandledShallow(): Int = run[Int](Effects.handle(nop[Int])(captured(1, 8)))
  @Benchmark
  def captureHandledGeneral(): Int = run[Int](Effects.handle(nopK[Int])(captured(16, 8)))
