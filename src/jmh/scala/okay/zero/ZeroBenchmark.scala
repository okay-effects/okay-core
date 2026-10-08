package okay.zero

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import Cont0.*

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

  def ticks[R <: Row, S](i: Int, acc: Int)(using Ctx[R, S], Tick :? R): Cont0[S, S, Int] =
    if i == 0 then pure(acc) else perform(Tick.Value(1)).flatMap(x => ticks(i - 1, acc + x))
  def ticking[R <: Row, S, A](using Ctx[R, S])(body: Ctx[Tick +: R, Inner[Tick, R, S, A]] ?=> Cont0[Inner[Tick, R, S, A], Inner[Tick, R, S, A], A]): Cont0[S, S, A] =
    handle(new Handler[[X] =>> Cont0[S, S, X], Tick, A, A]:
      def ret(a: A): A = a
      def apply[X](op: Tick[X], k: X => Cont0[S, S, A]): Cont0[S, S, A] = op match
        case Tick.Value(a) => k(a))(body)
  def skipping[R <: Row, S, A](using Ctx[R, S])(body: Ctx[Skip +: R, Inner[Skip, R, S, A]] ?=> Cont0[Inner[Skip, R, S, A], Inner[Skip, R, S, A], A]): Cont0[S, S, A] =
    handle(new Handler[[X] =>> Cont0[S, S, X], Skip, A, A]:
      def ret(a: A): A = a
      def apply[X](op: Skip[X], k: X => Cont0[S, S, A]): Cont0[S, S, A] = op match
        case Skip.Value(a) => k(a))(body)
  def nest[R <: Row, S](d: Int)(using Ctx[R, S], Tick :? R): Cont0[S, S, Int] = if d == 0 then ticks(n, 0) else skipping(nest(d - 1))

  @Benchmark
  def opsZero(): Int = run(ticking(nest(crossed)))

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
  def choosing[R <: Row, S, A](using Ctx[R, S])(body: Ctx[Among +: R, Inner[Among, R, S, A]] ?=> Cont0[Inner[Among, R, S, A], Inner[Among, R, S, A], A]): Cont0[S, S, Seq[A]] =
    handle(new Handler[[X] =>> Cont0[S, S, X], Among, A, Seq[A]]:
      def ret(a: A): Seq[A] = Vector(a)
      def apply[X](op: Among[X], k: X => Cont0[S, S, Seq[A]]): Cont0[S, S, Seq[A]] = op match
        case Among.Of(as) => as.foldLeft(pure[S, Seq[A]](Vector.empty))((acc, x) => acc.flatMap(v => k(x).map(v ++ _))))(body)
  def safe(col: Int, placed: List[Int]): Boolean = placed.zipWithIndex.forall((c, d) => c != col && math.abs(c - col) != d + 1)
  def place[R <: Row, S](row: Int, placed: List[Int])(using Ctx[R, S], Among :? R): Cont0[S, S, List[Int]] =
    if row == size then pure(placed)
    else perform(Among.Of((0 until size).filter(safe(_, placed)))).flatMap(col => place(row + 1, col :: placed))
  @Benchmark
  def queensZero(): Int = run(choosing(place(0, Nil))).length
