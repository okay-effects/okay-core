package okay.core

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import Cont.*
import Effects.*

/**
 * WHERE THE TYPE CLASSES COST: one program — a `Tick` answered in place, then the next call — four ways, each a
 * step nearer the polymorphic one, all in one run (the machine is shared; only a run's own benchmarks compare):
 *
 *  - cont: the concrete `Cont`, no parameters
 *  - contParams: the concrete `Cont`, with the parameters the polymorphic one has (`c`, `t`), captured by the
 *    closure as there
 *  - atStatic: `perform` and `flatMap`, the context typed `At` — the calls to a final class, no interface
 *  - tagless: the context typed `Effects`, as a program is written
 *  - monadKnown: the context an `Effects` whose monad is `Cont` — the row and the answer still any
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class DispatchBenchmark:

  @Param(Array("10000"))
  var n: Int = 0

  type T = Tick +: Pure
  type At0 = Effects.At[T, Boolean]

  val ticking: Handler[Cont, Tick, Pure, Boolean, Boolean, Boolean] = new Answering[Cont, Tick, Pure, Boolean, Boolean, Boolean]:
    def ret(a: Boolean): Boolean = a
    def value[X](op: Tick[X]): X = op match
      case Tick.Value(a) => a

  def evenC(n: Int): Cont[T, Boolean, Boolean, Boolean] =
    if n == 0 then pure(true) else inject[T, Boolean, Tick, Int](Tick.Value(n)).flatMap(_ => oddC(n - 1))
  def oddC(n: Int): Cont[T, Boolean, Boolean, Boolean] =
    if n == 0 then pure(false) else inject[T, Boolean, Tick, Int](Tick.Value(n)).flatMap(_ => evenC(n - 1))

  def evenP(n: Int)(using c: At0, t: In[Tick, T]): Cont[T, Boolean, Boolean, Boolean] =
    if n == 0 then Return(true) else Inject[Tick, T, Boolean, Int](Tick.Value(n), t).flatMap(_ => oddP(n - 1))
  def oddP(n: Int)(using c: At0, t: In[Tick, T]): Cont[T, Boolean, Boolean, Boolean] =
    if n == 0 then Return(false) else Inject[Tick, T, Boolean, Int](Tick.Value(n), t).flatMap(_ => evenP(n - 1))

  def evenA(n: Int)(using c: At0, t: Tick :? c.R): c.P[Boolean] =
    if n == 0 then c.pure(true) else perform(Tick.Value(n)).flatMap(_ => oddA(n - 1))
  def oddA(n: Int)(using c: At0, t: Tick :? c.R): c.P[Boolean] =
    if n == 0 then c.pure(false) else perform(Tick.Value(n)).flatMap(_ => evenA(n - 1))

  def evenT(n: Int)(using c: Effects, t: Tick :? c.R): c.P[Boolean] =
    if n == 0 then c.pure(true) else perform(Tick.Value(n)).flatMap(_ => oddT(n - 1))
  def oddT(n: Int)(using c: Effects, t: Tick :? c.R): c.P[Boolean] =
    if n == 0 then c.pure(false) else perform(Tick.Value(n)).flatMap(_ => evenT(n - 1))

  /** the context an `Effects` whose monad is `Cont`: any row, any answer, the monad known */
  type OnCont = Effects { type M[X <: Row, I, O, A] = Cont[X, I, O, A] }
  def evenM(n: Int)(using c: OnCont, t: Tick :? c.R): c.P[Boolean] =
    if n == 0 then c.pure(true) else perform(Tick.Value(n)).flatMap(_ => oddM(n - 1))
  def oddM(n: Int)(using c: OnCont, t: Tick :? c.R): c.P[Boolean] =
    if n == 0 then c.pure(false) else perform(Tick.Value(n)).flatMap(_ => evenM(n - 1))

  @Benchmark
  def monadKnown(): Boolean = evenM(n)(using Effects.At[T, Boolean](), summon).handle(ticking).value

  @Benchmark
  def cont(): Boolean = evenC(n).handle(ticking).value

  @Benchmark
  def contParams(): Boolean = evenP(n)(using Effects.At(), summon).handle(ticking).value

  @Benchmark
  def atStatic(): Boolean = evenA(n)(using Effects.At(), summon).handle(ticking).value

  @Benchmark
  def tagless(): Boolean = evenT(n)(using Effects.At[T, Boolean](), summon).handle(ticking).value
