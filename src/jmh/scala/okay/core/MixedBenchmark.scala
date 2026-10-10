package okay.core

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.std.*

/**
 * NOT ONE EFFECT IN A LOOP BUT A PROGRAM OF SEVERAL, as a program is — okay-java's `MixedBenchmark`, its program on
 * master's std: `n` orders, each priced from a reader's base, its supplier the first of three that fits (a choice,
 * `first`), a failure for some (`throws`, a handler per order), the total a state and the failures a log. Many
 * functions at each of the monad's call sites, as in a program. With no type profiles: `Jmh/run -jvmArgsAppend -XX:-UseTypeProfile MixedBenchmark`
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class MixedBenchmark:

  @Param(Array("cont", "zero"))
  var backend: String = ""

  @Param(Array("1000"))
  var n: Int = 0

  def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A =
    backend match
      case "cont" => okay.cont.run(body(using summon[okay.cont.At[Pure, A]]))
      case _ => okay.zero.run(body(using summon[okay.zero.At[Pure, A, A]]))

  val suppliers: Vector[Int] = Vector(0, 1, 2)

  /** the first value the body comes to, its choices tried in order, the rest not tried */
  def first[A](using c: Effects)(body: c.Body[Choose, Option[A], A]): c.Prog[Option[A]] =
    Effects.handle(new Handler[c.Prog, Choose, A, Option[A]]:
      def ret(a: A): Option[A] = Some(a)
      def apply[X](op: Choose[X], k: X => c.Prog[Option[A]]): c.Prog[Option[A]] = op match
        case Choose.Among(as) =>
          def from(i: Int): c.Prog[Option[A]] =
            if i == as.length then c.pure[c.S, Option[A]](None)
            else k(as(i)).flatMap(r => if r.isDefined then c.pure[c.S, Option[A]](r) else from(i + 1))
          from(0))(body)

  /** one order: its price, or its failure in the log; the total moved by its price */
  def order(i: Int)(using c: Effects, r: Reader % Int :? c.Effs, s: State % Int :? c.Effs,
      w: Writer % String :? c.Effs): c.Prog[Unit] =
    ask[Int].flatMap(base =>
      throws[String, Int](
        if i % 13 == 0 then raise[String, Int]("no stock")
        else first[Int](among(suppliers).flatMap(s => if (i + s) % 3 == 0 then pure(base + i % 7 + s) else among(Vector.empty[Int])))
          .map(_.getOrElse(0))))
      .flatMap {
        case Right(p) => modify[Int](_ + p).map(_ => ())
        case Left(e) => tell(e)
      }

  def orders(i: Int)(using c: Effects, r: Reader % Int :? c.Effs, s: State % Int :? c.Effs,
      w: Writer % String :? c.Effs): c.Prog[Unit] =
    if i == n then c.pure(()) else order(i).flatMap(_ => orders(i + 1))

  @Benchmark
  def orders(): Int =
    val (log, (total, _)) = run(reader(100)(writer[String, (Int, Unit)](state[Unit](0)(orders(0)))))
    total + log.length

