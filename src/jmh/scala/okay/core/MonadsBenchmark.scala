package okay.core

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.one.handler.{Answering as OneAnswering, Deep}
import Control.{reset, shift}
import okay.core.std.*

/** an operation answered with its value; one crossed by every `Tick`, performed never */
enum Tick[A]:
  case Value[A](a: A) extends Tick[A]
enum Skip[A]:
  case Value[A](a: A) extends Skip[A]

/**
 * THE PROGRAMS OF THE LANES, over the facade on the one monad (`okay.one`); `n` operations a run — the same lanes as
 * before the one monad, okay-core-zero's numbers in bench.d the base they are measured against (docs/one.md §4 H):
 *
 *  - pure: `n` binds, tail calls, no effect
 *  - answered / general / forwarded: `n` operations of a handler answering in place, of its clause written out,
 *    across a handler of another effect; across 4 and 16 handlers
 *  - state, writer, throwing, choose, collect, reads: the standard effects
 *  - stateBang, stateReaderBang: `Int ! State % Int`, the paths found through the program's place
 *  - capture: a shift under a reset (`Shifts`, control as an effect), the rest 16 maps, `k` 8 times — not divided by `n`
 *  - generate: `n` elements of a generator pulled, mapped, taken, pushed
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class MonadsBenchmark:

  @Param(Array("1000"))
  var n: Int = 0

  def isEven[R <: Row](i: Int): Eff[R, Boolean] = if i == 0 then okay.core.pure(true) else unit[R].flatMap(_ => isOdd(i - 1))
  def isOdd[R <: Row](i: Int): Eff[R, Boolean] = if i == 0 then okay.core.pure(false) else unit[R].flatMap(_ => isEven(i - 1))

  @Benchmark
  def pure(): Boolean = run(isEven[Pure](n))

  def isEvenT[R <: Row: Has[Tick]](i: Int): Eff[R, Boolean] =
    if i == 0 then okay.core.pure(true) else perform(Tick.Value(i)).at[R].flatMap(_ => isOddT(i - 1))
  def isOddT[R <: Row: Has[Tick]](i: Int): Eff[R, Boolean] =
    if i == 0 then okay.core.pure(false) else perform(Tick.Value(i)).at[R].flatMap(_ => isEvenT(i - 1))

  def ticking[A, T <: Row]: OneAnswering[Tick, T, A, A] = new OneAnswering[Tick, T, A, A]:
    def ret(a: A): Eff[T, A] = okay.core.pure(a)
    def value[X](op: Tick[X]): X = op match
      case Tick.Value(a) => a
  def tickingK[A, T <: Row]: Deep[Tick, T, A, A] = new Deep[Tick, T, A, A]:
    def ret(a: A): Eff[T, A] = okay.core.pure(a)
    def apply[X](op: Tick[X], k: X => Eff[T, A]): Eff[T, A] = op match
      case Tick.Value(a) => k(a)
  def skipping[A, T <: Row]: OneAnswering[Skip, T, A, A] = new OneAnswering[Skip, T, A, A]:
    def ret(a: A): Eff[T, A] = okay.core.pure(a)
    def value[X](op: Skip[X]): X = op match
      case Skip.Value(a) => a

  type T1 = Tick +: Pure

  @Benchmark
  def answered(): Boolean = run(handleIn(ticking[Boolean, Pure])(isEvenT[T1](n)))

  @Benchmark
  def general(): Boolean = run(handleIn(tickingK[Boolean, Pure])(isEvenT[T1](n)))

  @Benchmark
  def forwarded(): Boolean = run(handleIn(ticking[Boolean, Pure])(handleIn(skipping[Boolean, T1])(isEvenT[Skip +: T1](n))))

  def skips[R <: Row: Has[Tick]](d: Int): Eff[R, Boolean] =
    if d == 0 then isEvenT[R](n) else handleIn(skipping[Boolean, R])(skips[Skip +: R](d - 1))

  @Benchmark
  def forwarded4(): Boolean = run(handleIn(ticking[Boolean, Pure])(skips[T1](4)))
  @Benchmark
  def forwarded16(): Boolean = run(handleIn(ticking[Boolean, Pure])(skips[T1](16)))

  type St = State % Int +: Pure
  def counting(i: Int): Eff[St, Int] =
    if i == 0 then get[Int].at[St] else get[Int].at[St].flatMap(x => put(x + 1).at[St]).flatMap(_ => counting(i - 1))

  @Benchmark
  def state(): Int = run(okay.core.std.state(0)(counting(n)))._1
  @Benchmark
  def stateCell(): Int = run(cell(0)(counting(n)))._1

  type DeepRow = State % Int +: Writer % Int +: Reader % Int +: Pure
  def countingDeep(i: Int): Eff[DeepRow, Int] =
    if i == 0 then get[Int].at[DeepRow] else get[Int].at[DeepRow].flatMap(x => put(x + 1).at[DeepRow]).flatMap(_ => countingDeep(i - 1))
  /** the same, the state under two more handlers */
  @Benchmark
  def stateDeep(): Int = run(reader(0)(okay.core.std.writer[Int, (Int, Int), Reader % Int +: Pure](okay.core.std.state(0)(countingDeep(n)))))._2._1

  type Rd = Reader % Int +: Pure
  def asks(i: Int, acc: Int): Int ! Reader % Int = if i == 0 then okay.core.pure(acc) else ask[Int].flatMap(v => asks(i - 1, acc + v))

  /** `n` asks under a reader, answered in place */
  @Benchmark
  def reads(): Int = run(reader(1)(asks(n, 0)))
  /** the same under an interceptor of the reader, `local`: each ask its clause, the reader asked outside */
  @Benchmark
  def readsLocal(): Int = run(reader(1)(local[Int, Int, Rd](_ + 1)(asks(n, 0))))

  type Wr = Writer % Int +: Pure
  def telling(i: Int): Eff[Wr, Unit] = if i == 0 then okay.core.pure(()) else tell(i).at[Wr].flatMap(_ => telling(i - 1))

  @Benchmark
  def writer(): Int = run(okay.core.std.writer[Int, Unit, Pure](telling(n)))._1.length

  def raising(i: Int, acc: Int): Eff[Pure, Int] =
    if i == 0 then okay.core.pure(acc)
    else okay.core.std.throws[String, Int, Pure](raise("no").at[Throws % String +: Pure]).flatMap(e => raising(i - 1, acc + e.fold(_.length, identity)))

  @Benchmark
  def throwing(): Int = run(raising(n, 0))

  @Benchmark
  def choose(): Int = run(okay.core.std.choose(among(0 until n).map(_ + 1))).length
  /** the same under one more handler */
  @Benchmark
  def chooseNested(): Int = run(handleIn(ticking[Int, Pure])(okay.core.std.choose(among(0 until n).map(_ + 1)).map(_.length)))

  def fibs(i: Int, a: Long, b: Long): Unit ! Emit % Long =
    if i == 0 then okay.core.pure(()) else yield_(a).flatMap(_ => fibs(i - 1, b, a + b))

  @Benchmark
  def collect(): Long = run(okay.core.std.collect[Long, Unit, Pure](fibs(n, 0, 1)))._1.sum

  def leftMapping: Eff[Pure, Int] =
    var p: Eff[Pure, Int] = okay.core.pure(0)
    var i = 0
    while i < n do
      p = p.map(_ + 1)
      i += 1
    p

  @Benchmark
  def leftMaps(): Int = run(leftMapping)

  def rightMapping(i: Int): Eff[Pure, Int] = if i == 0 then okay.core.pure(0) else defer(rightMapping(i - 1)).map(_ + 1)

  @Benchmark
  def rightMaps(): Int = run(rightMapping(n))

  type Sh = Shifts[Int, Pure] +: Pure
  def shots(k: Unit => Eff[Sh, Int], i: Int, acc: Int): Eff[Sh, Int] =
    if i == 0 then okay.core.pure(acc) else k(()).flatMap(r => shots(k, i - 1, acc + r))

  /** a shift up to the nearest reset, `k` `times` times, the rest `depth` maps */
  def captured(depth: Int, times: Int): Eff[Sh, Int] =
    var p = shift[Int, Pure, Unit](k => shots(k, times, 0)).at[Sh].map(_ => 1)
    var i = 0
    while i < depth do
      p = p.map(_ + 1)
      i += 1
    p

  /** under a reset, the rest 16 maps, `k` 8 times; the rest one map; 256 maps; `k` once */
  @Benchmark
  def capture(): Int = run(reset[Int, Pure](captured(16, 8)))
  @Benchmark
  def captureShallow(): Int = run(reset[Int, Pure](captured(1, 8)))
  @Benchmark
  def captureDeep(): Int = run(reset[Int, Pure](captured(256, 8)))
  @Benchmark
  def captureOnce(): Int = run(reset[Int, Pure](captured(1, 1)))

  @Benchmark
  def tailcallDelay(): Boolean = run(evenDelayed(n))
  def evenDelayed(i: Int): Eff[Pure, Boolean] = if i == 0 then okay.core.pure(true) else defer(oddDelayed(i - 1))
  def oddDelayed(i: Int): Eff[Pure, Boolean] = if i == 0 then okay.core.pure(false) else defer(evenDelayed(i - 1))

  def leftBinding: Eff[Pure, Int] =
    var p: Eff[Pure, Int] = okay.core.pure(0)
    var i = 0
    while i < n do
      p = p.flatMap(x => okay.core.pure(x + 1))
      i += 1
    p

  @Benchmark
  def leftBinds(): Int = run(leftBinding)

  def sums(i: Int, a: Long, b: Long, acc: Long): Eff[Pure, Long] =
    if i == 0 then okay.core.pure(acc) else unit[Pure].flatMap(_ => sums(i - 1, b, a + b, acc + a))

  @Benchmark
  def fib(): Long = run(sums(n, 0, 1, 0))

  /** `Int ! State % Int`: the paths found through the program's place, per call; found once, the loop `counting` */
  def countingBang(i: Int): Int ! State % Int =
    if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => countingBang(i - 1))

  @Benchmark
  def stateBang(): Int = run(okay.core.std.state(0)(countingBang(n)))._1

  /** `Int ! State % Int + Reader % Int`: two effects joined, each path found through `+`'s sides, per call */
  def countingBoth(i: Int): Int ! State % Int + Reader % Int =
    if i == 0 then get[Int] else ask[Int].flatMap(d => get[Int].flatMap(x => put(x + d))).flatMap(_ => countingBoth(i - 1))

  @Benchmark
  def stateReaderBang(): Int = run(okay.core.std.reader(1)(okay.core.std.state(0)(countingBoth(n))))._1

  /** a column safe from the queens placed, the nearest first — a loop */
  def safe(col: Int, placed: List[Int]): Boolean =
    var rest = placed
    var d = 1
    while rest.nonEmpty do
      val c = rest.head
      if c == col || math.abs(c - col) == d then return false
      rest = rest.tail
      d += 1
    true
  def place(row: Int, placed: List[Int]): List[Int] ! Choose =
    if row == 8 then okay.core.pure(placed) else among((0 until 8).filter(safe(_, placed))).flatMap(col => place(row + 1, col :: placed))

  /** the 92 solutions of 8 queens by choose — not divided by `n` */
  @Benchmark
  def queens(): Int = run(okay.core.std.choose(place(0, Nil))).length

  def forever(a: Long, b: Long): Unit ! Emit % Long = yield_(a).flatMap(_ => forever(b, a + b))

  /** `n` elements of an infinite generator pulled, each a run of the rest */
  @Benchmark
  def generate(): Long = sum(okay.core.std.stream[Long](forever(0, 1)).start)

  /** the same, each element mapped in the generator's handler: `Stream.map`, fused */
  @Benchmark
  def generateMapped(): Long = sum(okay.core.std.stream[Long](forever(0, 1)).map(_ + 1).start)

  /** `n` elements of an infinite generator taken: each element's rest run in place by the handler, the rest after
   * the `n`th dropped */
  @Benchmark
  def generateTaken(): Long = okay.core.std.stream[Long](forever(0, 1)).take(n).sum

  /** `n` elements of a finite generator summed, pushed: each where it is yielded, no rest captured */
  @Benchmark
  def generatePushed(): Long = okay.core.std.stream[Long](fibs(n, 0, 1)).fold(0L)(_ + _)

  /** the first `n` elements summed, each pulled by a run of the rest: no list made */
  private def sum(s0: okay.core.std.Step[Long]): Long =
    var s = s0
    var i = 0
    var sum = 0L
    while i < n do
      s match
        case e: okay.core.std.Step.Next[Long] => sum += e.w; i += 1; if i < n then s = e.pull()
        case e: okay.core.std.Step.Later[Long] => sum += e.w; i += 1; if i < n then s = e.pull()
        case okay.core.std.Step.Done() => i = n
    sum
