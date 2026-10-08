package okay.zero

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.zero.std.*

/** master's ContBenchmark on okay.zero: a tail call a bind, an operation answered in place, by its clause, through
 * a handler of another effect */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ZeroContBenchmark:
  @Param(Array("10000"))
  var n: Int = 0

  def isEven(i: Int)(using c: Ctx): c.P[Boolean] = if i == 0 then pure(true) else pure[c.R, c.J, c.T, Unit](()).flatMap(_ => isOdd(i - 1))
  def isOdd(i: Int)(using c: Ctx): c.P[Boolean] = if i == 0 then pure(false) else pure[c.R, c.J, c.T, Unit](()).flatMap(_ => isEven(i - 1))
  @Benchmark
  def tailcallPureZero(): Boolean = run[Boolean](isEven(n))

  def isEvenT(i: Int)(using c: Ctx, t: Tick :? c.R): c.P[Boolean] = if i == 0 then pure(true) else perform(Tick.Value(i)).flatMap(_ => isOddT(i - 1))
  def isOddT(i: Int)(using c: Ctx, t: Tick :? c.R): c.P[Boolean] = if i == 0 then pure(false) else perform(Tick.Value(i)).flatMap(_ => isEvenT(i - 1))
  def ticking[A](using c: Ctx)(body: c.Body[Tick, A, A]): c.P[A] =
    handle(new Answering[c.P, Tick, A, A]:
      def ret(a: A): A = a
      def value[X](op: Tick[X]): X = op match
        case Tick.Value(a) => a)(body)
  def tickingK[A](using c: Ctx)(body: c.Body[Tick, A, A]): c.P[A] =
    handle(new Handler[c.P, Tick, A, A]:
      def ret(a: A): A = a
      def apply[X](op: Tick[X], k: X => c.P[A]): c.P[A] = op match
        case Tick.Value(a) => k(a))(body)
  def skipping[A](using c: Ctx)(body: c.Body[Skip, A, A]): c.P[A] =
    handle(new Answering[c.P, Skip, A, A]:
      def ret(a: A): A = a
      def value[X](op: Skip[X]): X = op match
        case Skip.Value(a) => a)(body)

  @Benchmark
  def tailcallHandledZero(): Boolean = run(ticking[Boolean](isEvenT(n)))
  @Benchmark
  def tailcallHandledGeneralZero(): Boolean = run(tickingK[Boolean](isEvenT(n)))
  @Benchmark
  def tailcallForwardedZero(): Boolean = run(ticking[Boolean](skipping[Boolean](isEvenT(n))))

/** master's BindBenchmark on okay.zero: the monad alone, `n` binds or maps */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ZeroBindBenchmark:
  @Param(Array("10000"))
  var n: Int = 0

  def isEven(i: Int)(using c: Ctx): c.P[Boolean] = if i == 0 then pure(true) else delay(isOdd(i - 1))
  def isOdd(i: Int)(using c: Ctx): c.P[Boolean] = if i == 0 then pure(false) else delay(isEven(i - 1))
  @Benchmark
  def tailcallDelayZero(): Boolean = run[Boolean](isEven(n))

  def leftBinds(using c: Ctx): c.P[Int] =
    var p: c.P[Int] = pure(0)
    var i = 0
    while i < n do
      p = p.flatMap(x => pure(x + 1))
      i += 1
    p
  @Benchmark
  def leftBindsZero(): Int = run[Int](leftBinds)

  def leftMaps(using c: Ctx): c.P[Int] =
    var p: c.P[Int] = pure(0)
    var i = 0
    while i < n do
      p = p.map(_ + 1)
      i += 1
    p
  @Benchmark
  def leftMapsZero(): Int = run[Int](leftMaps)

  def go(i: Int)(using c: Ctx): c.P[Int] = if i == 0 then pure(0) else delay(go(i - 1)).map(_ + 1)
  @Benchmark
  def rightMapsZero(): Int = run[Int](go(n))

enum Far[+A]:
  case Value[A](a: A) extends Far[A]
enum Near[+A]:
  case Value[A](a: A) extends Near[A]

/** master's DepthBenchmark on okay.zero: an operation through `depth` handlers of another effect, all answering */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ZeroDepthBenchmark:
  @Param(Array("10000"))
  var n: Int = 0
  @Param(Array("0", "1", "4", "16"))
  var depth: Int = 0

  def far[A](using c: Ctx)(body: c.Body[Far, A, A]): c.P[A] =
    handle(new Answering[c.P, Far, A, A]:
      def ret(a: A): A = a
      def value[X](op: Far[X]): X = op match
        case Far.Value(a) => a)(body)
  def near[A](using c: Ctx)(body: c.Body[Near, A, A]): c.P[A] =
    handle(new Answering[c.P, Near, A, A]:
      def ret(a: A): A = a
      def value[X](op: Near[X]): X = op match
        case Near.Value(a) => a)(body)
  def loop(i: Int, acc: Int)(using c: Ctx, f: Far :? c.R): c.P[Int] =
    if i == 0 then pure(acc) else perform(Far.Value(1)).flatMap(x => loop(i - 1, acc + x))
  def nested(d: Int)(using c: Ctx, f: Far :? c.R): c.P[Int] = if d == 0 then loop(n, 0) else near[Int](nested(d - 1))

  @Benchmark
  def forwardedDepthZero(): Int = run(far[Int](nested(depth)))

/** master's FibBenchmark on okay.zero: the monad's binds alone, a yield per number collected, a generator pulled */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ZeroFibBenchmark:
  @Param(Array("10000"))
  var n: Int = 0

  def sums(i: Int, a: Long, b: Long, acc: Long)(using c: Ctx): c.P[Long] =
    if i == 0 then pure(acc) else pure[c.R, c.J, c.T, Unit](()).flatMap(_ => sums(i - 1, b, a + b, acc + a))
  @Benchmark
  def fibPureZero(): Long = run[Long](sums(n, 0, 1, 0))

  def fibs(i: Int, a: Long, b: Long)(using c: Ctx, e: Emit % Long :? c.R): c.P[Unit] =
    if i == 0 then pure(()) else yield_(a).flatMap(_ => fibs(i - 1, b, a + b))
  @Benchmark
  def fibCollectZero(): Long = run(collect[Long, Unit](fibs(n, 0, 1)))._1.sum

  def forever(a: Long, b: Long)(using c: Ctx, e: Emit % Long :? c.R): c.P[Unit] = yield_(a).flatMap(_ => forever(b, a + b))
  @Benchmark
  def fibGenerateZero(): Long =
    var first: Option[Eff[Empty, FibGen, FibGen, FibGen, FibGen]] = None
    run[FibGen] { first = Some(generate[Long, FibGen](FibGen.Next(_, _), FibGen.Done)(forever(0, 1))); pure(FibGen.Done) }
    var gen = first.get
    var i = 0
    var sum = 0L
    while i < n do
      run(gen) match
        case FibGen.Next(w, rest) => sum += w; gen = rest; i += 1
        case FibGen.Done => i = n
    sum

enum FibGen:
  case Next(w: Long, rest: Eff[Empty, FibGen, FibGen, FibGen, FibGen])
  case Done

/** master's CaptureBenchmark and CaptureHandledBenchmark on okay.zero: a shift through `depth` maps, its `k` applied
 * `shots` times — under a reset, and under a handler with no reset between */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ZeroCaptureBenchmark:
  @Param(Array("1", "16", "256"))
  var depth: Int = 0
  @Param(Array("1", "8"))
  var shots: Int = 0

  enum Nop[X]:
    case Op extends Nop[Unit]

  def calls[R <: Row, J](k: Unit => Eff[R, J, Int, Int, Int], i: Int, acc: Int): Eff[R, J, Int, Int, Int] =
    if i == 0 then pure(acc) else k(()).flatMap(r => calls(k, i - 1, acc + r))

  def captured(using c: Ctx { type T = Int }): Eff[c.R, c.J, Int, Int, Int] =
    var p = shift[Unit, Int](k => calls(k, shots, 0)).map(_ => 1)
    var i = 0
    while i < depth do
      p = p.map(_ + 1)
      i += 1
    p

  @Benchmark
  def captureDepthZero(): Int = run[Int](reset[Int](captured))

  @Benchmark
  def captureHandledZero(): Int = run[Int](handle(new Answering[[X] =>> Eff[Empty, Int, Int, Int, X], Nop, Int, Int]:
    def ret(a: Int): Int = a
    def value[X](op: Nop[X]): X = op match
      case Nop.Op => ())(captured))

  /** the same under a general handler: a shift up to it read, not run by the machine */
  @Benchmark
  def captureHandledGeneralZero(): Int = run[Int](handle(new Handler[[X] =>> Eff[Empty, Int, Int, Int, X], Nop, Int, Int]:
    def ret(a: Int): Int = a
    def apply[X](op: Nop[X], k: X => Eff[Empty, Int, Int, Int, Int]): Eff[Empty, Int, Int, Int, Int] = op match
      case Nop.Op => k(()))(captured))
