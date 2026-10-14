package okay.core

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.core.std.*
import okay.core.*

/**
 * NOT ONE EFFECT IN A LOOP BUT A PROGRAM OF SEVERAL, as a program is — okay-java's `MixedBenchmark`, its program on
 * master's std: `n` orders, each priced from a reader's base, its supplier the first of three that fits (a choice,
 * `first`), a failure for some (`throws`, a handler per order), the total a state and the failures a log. Many
 * functions at each of the monad's call sites, as in a program; the total a `state` (each path its own) or a `cell`
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class MixedBenchmark:

  @Param(Array("state", "cell"))
  var total: String = ""

  @Param(Array("1000"))
  var n: Int = 0

  val suppliers: Vector[Int] = Vector(0, 1, 2)

  def supplier(i: Int, base: Int): Int ! Choose =
    among(suppliers).flatMap(s => if (i + s) % 3 == 0 then pure(base + i % 7 + s) else fail[Int])

  def price[R <: Row: Has[Throws % String]](i: Int, base: Int): Eff[R, Int] =
    if i % 13 == 0 then raise("no stock").at[R]
    else first[Int, R](supplier(i, base)).map(_.getOrElse(0))

  /** one order: its price, or its failure in the log; the total moved by its price */
  def order(i: Int): Unit ! Reader % Int + State % Int + Writer % String =
    ask[Int].flatMap { base =>
      throws(price(i, base)).flatMap {
        case Right(p) => modify[Int](_ + p).map(_ => ())
        case Left(e) => tell(e)
      }
    }

  def orders(i: Int): Unit ! Reader % Int + State % Int + Writer % String =
    if i == n then pure(()) else order(i).flatMap(_ => orders(i + 1))

  type R1 = Reader % Int +: Pure
  type R2 = Writer % String +: R1

  @Benchmark
  def orders(): Int =
    val (log, (t, _)) =
      if total == "cell" then run(reader(100)(writer[String, (Int, Unit), R1](cell(0)(orders(0)))))
      else run(reader(100)(writer[String, (Int, Unit), R1](state(0)(orders(0)))))
    t + log.length

