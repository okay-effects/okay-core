package okay.zero

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit


enum Tick[+A]:
  case Value[A](a: A) extends Tick[A]
enum Skip[+A]:
  case Value[A](a: A) extends Skip[A]

/** okay.zero: `n` operations of `Tick`, each answered `k(a)`, crossing `crossed` handlers of `Skip` — okay.core's
 * ContBenchmark.tailcallHandledGeneral is `crossed=0` on `Cont` */
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

  def ticks[R <: Row, J, T](i: Int, acc: Int)(using Ctx[R, J], Tick :? R): Eff[R, J, T, T, Int] =
    if i == 0 then pure(acc) else perform(Tick.Value(1)).flatMap(x => ticks(i - 1, acc + x))
  def ticking[R <: Row, J, T, A](using Ctx[R, J])(body: Body[Tick, R, J, T, A, A]): Eff[R, J, T, T, A] =
    handle(new Handler[[X] =>> Eff[R, J, T, T, X], Tick, A, A]:
      def ret(a: A): A = a
      def apply[X](op: Tick[X], k: X => Eff[R, J, T, T, A]): Eff[R, J, T, T, A] = op match
        case Tick.Value(a) => k(a))(body)
  def skipping[R <: Row, J, T, A](using Ctx[R, J])(body: Body[Skip, R, J, T, A, A]): Eff[R, J, T, T, A] =
    handle(new Handler[[X] =>> Eff[R, J, T, T, X], Skip, A, A]:
      def ret(a: A): A = a
      def apply[X](op: Skip[X], k: X => Eff[R, J, T, T, A]): Eff[R, J, T, T, A] = op match
        case Skip.Value(a) => k(a))(body)
  def nest[R <: Row, J, T](d: Int)(using Ctx[R, J], Tick :? R): Eff[R, J, T, T, Int] = if d == 0 then ticks(n, 0) else skipping(nest(d - 1))

  @Benchmark
  def opsZero(): Int = run[Int](ticking(nest(crossed)))

/** QueensBenchmark's on okay.zero */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ZeroQueensBenchmark:
  @Param(Array("8"))
  var size: Int = 0
  enum Among[+A]:
    case Of[A](as: Seq[A]) extends Among[A]
  def choosing[R <: Row, J, T, A](using Ctx[R, J])(body: Body[Among, R, J, T, A, Seq[A]]): Eff[R, J, T, T, Seq[A]] =
    handle(new Handler[[X] =>> Eff[R, J, T, T, X], Among, A, Seq[A]]:
      def ret(a: A): Seq[A] = Vector(a)
      def apply[X](op: Among[X], k: X => Eff[R, J, T, T, Seq[A]]): Eff[R, J, T, T, Seq[A]] = op match
        case Among.Of(as) => as.foldLeft(pure[R, J, T, Seq[A]](Vector.empty))((acc, x) => acc.flatMap(v => k(x).map(v ++ _))))(body)
  def safe(col: Int, placed: List[Int]): Boolean = placed.zipWithIndex.forall((c, d) => c != col && math.abs(c - col) != d + 1)
  def place[R <: Row, J, T](row: Int, placed: List[Int])(using Ctx[R, J], Among :? R): Eff[R, J, T, T, List[Int]] =
    if row == size then pure(placed)
    else perform(Among.Of((0 until size).filter(safe(_, placed)))).flatMap(col => place(row + 1, col :: placed))
  @Benchmark
  def queensZero(): Int = run[Seq[List[Int]]](choosing(place(0, Nil))).length
