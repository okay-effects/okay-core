package okay.one.bench

import okay.one.{!, Cont}
import okay.one.Cont.rows
import okay.one.handler.*
import okay.one.row.{+:, In, Pure, Row}
import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

enum Tick[X]:
  case Value[A](a: A) extends Tick[A]
enum Skip[X]:
  case Value[A](a: A) extends Skip[A]
enum Throws[X]:
  case Raise(e: String) extends Throws[Nothing]
enum Ask[X]:
  case Get extends Ask[Int]
enum Tell[X]:
  case Put(i: Int) extends Tell[Unit]
enum St[X]:
  case Get extends St[Int]
  case Put(i: Int) extends St[Unit]
enum Among[X]:
  case Of[A](xs: Seq[A]) extends Among[A]
enum Emit[X]:
  case Value(a: Long) extends Emit[Unit]

/** THE KERNEL'S OWN LANES, no facade — okay-zero's MonadsBenchmark programs written in `Cont` over their rows, their handlers registered,
 * run — the same shapes, the same n as okay-zero's `MonadsBenchmark` */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class KernelBenchmark:
  @Param(Array("1000"))
  var n: Int = 0

  def unit[R <: Row]: Unit ! R = Cont.pure(())
  def isEven[R <: Row](i: Int): Boolean ! R = if i == 0 then Cont.pure(true) else unit[R].flatMap(_ => isOdd(i - 1))
  def isOdd[R <: Row](i: Int): Boolean ! R = if i == 0 then Cont.pure(false) else unit[R].flatMap(_ => isEven(i - 1))

  @Benchmark
  def pure(): Boolean = isEven[Pure](n).run

  def isEvenT[R <: Row](i: Int)(using In[Tick, R]): Boolean ! R =
    if i == 0 then Cont.pure(true) else Cont.perform[Tick, R, Int](Tick.Value(i)).flatMap(_ => isOddT(i - 1))
  def isOddT[R <: Row](i: Int)(using In[Tick, R]): Boolean ! R =
    if i == 0 then Cont.pure(false) else Cont.perform[Tick, R, Int](Tick.Value(i)).flatMap(_ => isEvenT(i - 1))

  def ticking[T <: Row, A] = new Answering[Tick, T, A, A]:
    def ret(a: A) = Cont.pure(a)
    def value[X](op: Tick[X]): X = op match { case Tick.Value(a) => a }
  def tickingK[T <: Row, A] = new Deep[Tick, T, A, A]:
    def ret(a: A) = Cont.pure(a)
    def apply[X](op: Tick[X], k: X => A ! T) = op match { case Tick.Value(a) => k(a) }
  def skipping[T <: Row, A] = new Answering[Skip, T, A, A]:
    def ret(a: A) = Cont.pure(a)
    def value[X](op: Skip[X]): X = op match { case Skip.Value(a) => a }

  type T = Tick +: Pure

  @Benchmark
  def answered(): Boolean = isEvenT[T](n).handle(ticking).run

  @Benchmark
  def general(): Boolean = isEvenT[T](n).handle(tickingK).run

  @Benchmark
  def forwarded(): Boolean = isEvenT[Skip +: T](n).handle(skipping).handle(ticking).run

  def skips[R <: Row](d: Int, h: Boolean ! R => Boolean ! Pure)(using In[Tick, R]): Boolean =
    if d == 0 then h(isEvenT[R](n)).run else skips[Skip +: R](d - 1, p => h(p.handle(skipping)))

  @Benchmark
  def forwarded16(): Boolean = skips[T](16, p => p.handle(ticking))

  @Benchmark
  def forwardedK(): Boolean = isEvenT[Skip +: T](n).handle(skipping).handle(tickingK).run

  @Benchmark
  def forwardedK16(): Boolean = skips[T](16, p => p.handle(tickingK))

  type Rd = Ask +: Pure
  def asks(i: Int, acc: Int): Int ! Rd =
    if i == 0 then Cont.pure(acc) else rows[Rd].perform(Ask.Get).flatMap(v => asks(i - 1, acc + v))
  def reader[T <: Row] = new Answering[Ask, T, Int, Int]:
    def ret(a: Int) = Cont.pure(a)
    def value[X](op: Ask[X]): X = op match { case Ask.Get => 1 }

  @Benchmark
  def reads(): Int = asks(n, 0).handle(reader).run

  type Wr = Tell +: Pure
  def telling(i: Int): Unit ! Wr = if i == 0 then unit else rows[Wr].perform(Tell.Put(i)).flatMap(_ => telling(i - 1))
  /** the log in the handler, answered in place: okay-zero's writer */
  def writing[T <: Row] = new Answering[Tell, T, Unit, (List[Int], Unit)]:
    private var log: List[Int] = Nil
    def ret(a: Unit) = Cont.pure(log.reverse -> a)
    def value[X](op: Tell[X]): X = op match
      case Tell.Put(i) => log = i :: log

  @Benchmark
  def writer(): Int = telling(n).handle(writing).run._1.length

  type Sc = St +: Pure
  def counting(i: Int): Int ! Sc =
    if i == 0 then rows[Sc].perform(St.Get)
    else rows[Sc].perform(St.Get).flatMap(x => rows[Sc].perform(St.Put(x + 1))).flatMap(_ => counting(i - 1))
  def cell[T <: Row](s0: Int) = new Answering[St, T, Int, Int]:
    var s = s0
    def ret(a: Int) = Cont.pure(a)
    def value[X](op: St[X]): X = op match
      case St.Get => s
      case St.Put(i) => s = i

  @Benchmark
  def stateCell(): Int = counting(n).handle(cell(0)).run

  @Benchmark
  def leftMaps(): Int =
    var p: Int ! Pure = Cont.pure(0)
    var i = 0
    while i < n do
      p = p.map(_ + 1)
      i += 1
    p.run

  def rightMapping(i: Int): Int ! Pure = if i == 0 then Cont.pure(0) else Cont.defer(rightMapping(i - 1)).map(_ + 1)

  @Benchmark
  def rightMaps(): Int = rightMapping(n).run

  type Th = Throws +: Pure
  def catching[T <: Row] = new Aborting[Throws, T, Int, Either[String, Int]]:
    def ret(a: Int) = Cont.pure(Right(a))
    def abort[X](op: Throws[X]) = op match { case Throws.Raise(e) => Cont.pure(Left(e)) }
  /** the same, a handler dropping `k` by hand */
  def catchingK[T <: Row] = new Deep[Throws, T, Int, Either[String, Int]]:
    def ret(a: Int) = Cont.pure(Right(a))
    def apply[X](op: Throws[X], k: X => Either[String, Int] ! T) = op match
      case Throws.Raise(e) => Cont.pure(Left(e))
  /** each raise handled where it is, a handler in the program */
  def raising(h: => Handler[Throws, Pure, Int, Either[String, Int]])(i: Int, acc: Int): Int ! Pure =
    if i == 0 then Cont.pure(acc)
    else rows[Th].perform(Throws.Raise("no")).map(_ => 0).handle(h)
      .flatMap(e => raising(h)(i - 1, acc + e.fold(_.length, identity)))

  @Benchmark
  def throwing(): Int = raising(catching)(n, 0).run

  @Benchmark
  def throwingK(): Int = raising(catchingK)(n, 0).run

  /** state answered in place, each path of a `k` its own: okay-zero's `state` */
  def counter[T <: Row, A](s0: Int) = new Stateful[St, T, Int, A, (Int, A)]:
    def initial = s0
    def ret(s: Int, a: A) = Cont.pure(s -> a)
    def value[X](s: Int, op: St[X]): X = op match
      case St.Get => s
      case St.Put(_) => ()
    def next[X](s: Int, op: St[X]): Int = op match
      case St.Get => s
      case St.Put(i) => i

  @Benchmark
  def state(): Int = counting(n).handle(counter(0)).run._1

  type Deep3 = St +: Ask +: Tell +: Pure
  def countingDeep(i: Int): Int ! Deep3 =
    if i == 0 then rows[Deep3].perform(St.Get)
    else rows[Deep3].perform(St.Get).flatMap(x => rows[Deep3].perform(St.Put(x + 1))).flatMap(_ => countingDeep(i - 1))
  def readerAt[T <: Row, A] = new Answering[Ask, T, A, A]:
    def ret(a: A) = Cont.pure(a)
    def value[X](op: Ask[X]): X = op match { case Ask.Get => 0 }
  def writerAt[T <: Row, A] = new Answering[Tell, T, A, (List[Int], A)]:
    private var log: List[Int] = Nil
    def ret(a: A) = Cont.pure(log.reverse -> a)
    def value[X](op: Tell[X]): X = op match { case Tell.Put(i) => log = i :: log }

  /** the state under two handlers more */
  @Benchmark
  def stateDeep(): Int = countingDeep(n).handle(counter(0)).handle(readerAt).handle(writerAt).run._2._1

  /** every path, its values: the paths one by one */
  def choosing[T <: Row, A] = new Deep[Among, T, A, List[A]]:
    def ret(a: A) = Cont.pure(List(a))
    def apply[X](op: Among[X], k: X => List[A] ! T) = op match
      case Among.Of(xs) =>
        val as = xs.toIndexedSeq
        def from(i: Int, acc: List[A]): List[A] ! T =
          if i == as.length then Cont.pure(acc.reverse) else k(as(i)).flatMap(r => from(i + 1, r reverse_::: acc))
        from(0, Nil)

  /** okay-zero's `choose`: every value of a choice among n */
  @Benchmark
  def choose(): Int = rows[Among +: Pure].perform(Among.Of(0 until n)).map(_ + 1).handle(choosing).run.length

  def collecting[T <: Row, A] = new Stateful[Emit, T, List[Long], A, (List[Long], A)]:
    def initial = Nil
    def ret(s: List[Long], a: A) = Cont.pure(s.reverse -> a)
    def value[X](s: List[Long], op: Emit[X]): X = op match { case Emit.Value(_) => () }
    def next[X](s: List[Long], op: Emit[X]) = op match { case Emit.Value(a) => a :: s }

  type Em = Emit +: Pure
  def fibs(i: Int, a: Long, b: Long): Unit ! Em =
    if i == 0 then Cont.pure(()) else rows[Em].perform(Emit.Value(a)).flatMap(_ => fibs(i - 1, b, a + b))

  /** okay-zero's `collect`: n values given out */
  @Benchmark
  def collect(): Long = fibs(n, 0, 1).handle(collecting).run._1.sum

  // okay-core's: control as an effect, a generator pulled

  type Sh = okay.one.std.Shifts[Int, Pure] +: Pure
  def shots(k: Unit => Int ! Sh, i: Int, acc: Int): Int ! Sh =
    if i == 0 then Cont.pure(acc) else k(()).flatMap(r => shots(k, i - 1, acc + r))

  /** a shift up to the nearest reset, `k` `times` times, the rest `depth` maps */
  def captured(depth: Int, times: Int): Int ! Sh =
    var p = okay.one.std.Shift.shift[Int, Pure, Unit, Sh](k => shots(k, times, 0)).map(_ => 1)
    var i = 0
    while i < depth do
      p = p.map(_ + 1)
      i += 1
    p

  /** under a reset, the rest 16 maps, `k` 8 times; the rest one map; 256 maps; `k` once — not divided by `n` */
  @Benchmark
  def capture(): Int = captured(16, 8).handle(okay.one.std.Shift.reset[Int, Pure]).run
  @Benchmark
  def captureShallow(): Int = captured(1, 8).handle(okay.one.std.Shift.reset[Int, Pure]).run
  @Benchmark
  def captureDeep(): Int = captured(256, 8).handle(okay.one.std.Shift.reset[Int, Pure]).run
  @Benchmark
  def captureOnce(): Int = captured(1, 1).handle(okay.one.std.Shift.reset[Int, Pure]).run

  /** a generator's consumer: an element and the rest, or the end */
  enum Gen:
    case Done
    case Next(w: Long, rest: Gen ! Pure)
  type Y = okay.one.std.%[okay.one.std.Emit, Long] +: Pure
  def forever(a: Long, b: Long): Unit ! Y = okay.one.std.Emit.emit[Long, Y](a).flatMap(_ => forever(b, a + b))

  /** `n` elements of an infinite generator pulled, each a run of the rest */
  @Benchmark
  def generate(): Long =
    var g: Gen ! Pure = forever(0, 1).handle(okay.one.std.Emit.generate[Long, Gen, Pure](Gen.Next(_, _), Gen.Done))
    var i = 0
    var sum = 0L
    while i < n do
      g.run match
        case Gen.Next(w, rest) => sum += w; i += 1; g = rest
        case Gen.Done => i = n
    sum

  type Sh0 = okay.one.std.Shifts[Int, Pure] +: Pure
  def shots0(k: Unit => Int ! Pure, i: Int, acc: Int): Int ! Pure =
    if i == 0 then Cont.pure(acc) else k(()).flatMap(r => shots0(k, i - 1, acc + r))
  def captured0(depth: Int, times: Int): Int ! Sh0 =
    var p = okay.one.std.Shift.shift0[Int, Pure, Unit, Sh0](k => shots0(k, times, 0)).map(_ => 1)
    var i = 0
    while i < depth do
      p = p.map(_ + 1)
      i += 1
    p

  /** the same by shift0: its body outside the reset — no reset of its own, no k widened */
  @Benchmark
  def capture0(): Int = captured0(16, 8).handle(okay.one.std.Shift.reset[Int, Pure]).run
  @Benchmark
  def capture0Shallow(): Int = captured0(1, 8).handle(okay.one.std.Shift.reset[Int, Pure]).run

  // the same capture under a $ over Pure: shift0 and $ the machine's own nodes, k a program over Pure

  type C = Int ! Pure
  def shotsC(k: Unit => C, i: Int, acc: Int): C =
    if i == 0 then Cont.Return(acc) else Cont.Bind(k(()), (r: Int) => shotsC(k, i - 1, acc + r))
  def capturedC(depth: Int, times: Int): Cont[okay.one.Reset, C, C, Int] =
    var p: Cont[okay.one.Reset, C, C, Int] =
      Cont.Map(Cont.Shift0[okay.one.Reset, Pure, Nothing, Nothing, Int, C, Unit](okay.one.row.Nearest.Here(),
        k => shotsC(k, times, 0)), (_: Unit) => 1)
    var i = 0
    while i < depth do
      p = Cont.Map(p, (x: Int) => x + 1)
      i += 1
    p

  /** under a $, the rest 16 maps, `k` 8 times — control's own nodes */
  @Benchmark
  def captureControl(): Int =
    Cont.Dollar[Pure, C, Nothing, Nothing, Int, Int](capturedC(16, 8), (a: Int) => Cont.Return(a)).run
  @Benchmark
  def captureControlShallow(): Int =
    Cont.Dollar[Pure, C, Nothing, Nothing, Int, Int](capturedC(1, 8), (a: Int) => Cont.Return(a)).run
