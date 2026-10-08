package okay.zero

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.zero.std.*

enum Tick[+A]:
  case Value[A](a: A) extends Tick[A]
enum Skip[+A]:
  case Value[A](a: A) extends Skip[A]

/** okay.zero: `n` operations of `Tick`, each answered `k(a)` — or in place, `answering` — crossing `crossed`
 * handlers of `Skip`. okay.core's ContBenchmark.tailcallHandledGeneral and tailcallHandled are `crossed=0` */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ZeroBenchmark:
  @Param(Array("10000"))
  var n: Int = 0
  @Param(Array("0", "1", "4"))
  var crossed: Int = 0
  @Param(Array("general", "answering"))
  var clause: String = ""

  def ticks(i: Int, acc: Int)(using c: Ctx, t: Tick :? c.R): c.P[Int] =
    if i == 0 then pure(acc) else perform(Tick.Value(1)).flatMap(x => ticks(i - 1, acc + x))
  def ticking[A](using c: Ctx)(body: c.Body[Tick, A, A]): c.P[A] =
    if clause == "answering" then
      handle(new Answering[c.P, Tick, A, A]:
        def ret(a: A): A = a
        def value[X](op: Tick[X]): X = op match
          case Tick.Value(a) => a)(body)
    else
      handle(new Handler[c.P, Tick, A, A]:
        def ret(a: A): A = a
        def apply[X](op: Tick[X], k: X => c.P[A]): c.P[A] = op match
          case Tick.Value(a) => k(a))(body)
  def skipping[A](using c: Ctx)(body: c.Body[Skip, A, A]): c.P[A] =
    handle(new Handler[c.P, Skip, A, A]:
      def ret(a: A): A = a
      def apply[X](op: Skip[X], k: X => c.P[A]): c.P[A] = op match
        case Skip.Value(a) => k(a))(body)
  def nest(d: Int)(using c: Ctx, t: Tick :? c.R): c.P[Int] = if d == 0 then ticks(n, 0) else skipping(nest(d - 1))

  @Benchmark
  def opsZero(): Int = run(ticking[Int](nest(crossed)))

/** QueensBenchmark's on okay.zero, okay.zero.std's choose */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ZeroQueensBenchmark:
  @Param(Array("8"))
  var size: Int = 0
  def safe(col: Int, placed: List[Int]): Boolean = placed.zipWithIndex.forall((c, d) => c != col && math.abs(c - col) != d + 1)
  def place(row: Int, placed: List[Int])(using c: Ctx, ch: Choose :? c.R): c.P[List[Int]] =
    if row == size then pure(placed) else among((0 until size).filter(safe(_, placed))).flatMap(col => place(row + 1, col :: placed))
  @Benchmark
  def queensZero(): Int = run(choose[List[Int]](place(0, Nil))).length
