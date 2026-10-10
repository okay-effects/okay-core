package okay.core

import okay.cont.Cont

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import Control.{reset, shift}
import okay.std.*

/** an operation answered with its value; one crossed by every `Tick`, performed never */
enum Tick[+A]:
  case Value[A](a: A) extends Tick[A]
enum Skip[+A]:
  case Value[A](a: A) extends Skip[A]

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

  type St = State % Int +: Pure
  def countingP(i: Int): Program[St, Int] =
    if i == 0 then Programs.get[St, Int]
    else Programs.get[St, Int].flatMap(x => Programs.put[St, Int](x + 1)).flatMap(_ => countingP(i - 1))
  def top: Top = if backend == "cont" then okay.cont.top else okay.zero.top

  /** `state`'s loop as a `Program`, built on the monad where it runs: what the row in the type costs */
  @Benchmark
  def stateProgram(): Int = Program.run(top)(Programs.state(0)(countingP(n)))._1

  type Deep = Reader % Int +: Writer % Int +: State % Int +: Pure
  def countingDeep(i: Int): Program[Deep, Int] =
    if i == 0 then Programs.get[Deep, Int]
    else Programs.get[Deep, Int].flatMap(x => Programs.put[Deep, Int](x + 1)).flatMap(_ => countingDeep(i - 1))
  /** the same, the state third in the row and handled first: its paths translated in the handler's view */
  @Benchmark
  def stateProgramDeep(): Int = Program.run(top)(Programs.reader(0)(Programs.writer[Int](Programs.state(0)(countingDeep(n)))))._2._1

  def asks(i: Int, acc: Int)(using c: Effects, r: Reader % Int :? c.Effs): c.Prog[Int] =
    if i == 0 then c.pure(acc) else ask[Int].flatMap(v => asks(i - 1, acc + v))

  /** `n` asks under a reader, answered in place */
  @Benchmark
  def reads(): Int = run(reader(1)(asks(n, 0)))
  /** the same under an interceptor of the reader, `local`: each ask its clause, the reader asked outside */
  @Benchmark
  def readsLocal(): Int = run(reader(1)(local[Int, Int](_ + 1)(asks(n, 0))))

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
  /** the same under one more handler: the clauses' programs run in the machine's loop, not the kernel's at the top —
   * on Cont0 the difference is what running the top in the kernel costs */
  @Benchmark
  def chooseNested(): Int = run(Effects.handle(ticking[Int])(okay.std.choose[Int](among(0 until n).map(_ + 1)).map(_.length)))

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

  @Benchmark
  def tailcallDelay(): Boolean = run(evenDelayed(n))
  def evenDelayed(i: Int)(using c: Effects): c.Prog[Boolean] = if i == 0 then c.pure(true) else c.delay(oddDelayed(i - 1))
  def oddDelayed(i: Int)(using c: Effects): c.Prog[Boolean] = if i == 0 then c.pure(false) else c.delay(evenDelayed(i - 1))

  def leftBinding(using c: Effects): c.Prog[Int] =
    var p: c.Prog[Int] = c.pure(0)
    var i = 0
    while i < n do
      p = p.flatMap(x => c.pure(x + 1))
      i += 1
    p

  @Benchmark
  def leftBinds(): Int = run(leftBinding)

  def sums(i: Int, a: Long, b: Long, acc: Long)(using c: Effects): c.Prog[Long] =
    if i == 0 then c.pure(acc) else c.pure(()).flatMap(_ => sums(i - 1, b, a + b, acc + a))

  @Benchmark
  def fib(): Long = run(sums(n, 0, 1, 0))

  /** `Int ! State % Int`: the path found through the program's context, per call; found once, the loop `counting` */
  def countingBang(i: Int): Int ! State % Int =
    if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => countingBang(i - 1))
  def countingHoisted(i: Int): Int ! State % Int = counting(i)

  @Benchmark
  def stateBang(): Int = run(okay.std.state[Int](0)(countingBang(n)))._1
  @Benchmark
  def stateHoisted(): Int = run(okay.std.state[Int](0)(countingHoisted(n)))._1

  @Benchmark
  def forwarded4(): Boolean = run(Effects.handle(ticking)(skips(4)))

  /** a column safe from the queens placed, the nearest first — a loop, not `zipWithIndex`: that one's pairs, made or
   * not as the JIT inlines it, were most of the run and its spread, the monad's part a few percent */
  def safe(col: Int, placed: List[Int]): Boolean =
    var rest = placed
    var d = 1
    while rest.nonEmpty do
      val c = rest.head
      if c == col || math.abs(c - col) == d then return false
      rest = rest.tail
      d += 1
    true
  def place(row: Int, placed: List[Int])(using c: Effects, ch: Choose :? c.Effs): c.Prog[List[Int]] =
    if row == 8 then c.pure(placed) else among((0 until 8).filter(safe(_, placed))).flatMap(col => place(row + 1, col :: placed))

  /** the 92 solutions of 8 queens by choose — not divided by `n` */
  @Benchmark
  def queens(): Int = run(okay.std.choose[List[Int]](place(0, Nil))).length

  def forever(a: Long, b: Long)(using c: Effects, e: Emit % Long :? c.Effs): c.Prog[Unit] =
    yield_(a).flatMap(_ => forever(b, a + b))

  /** `n` elements of an infinite generator pulled, each a run of the rest: one `Step` on either monad */
  @Benchmark
  def generate(): Long = backend match
    case "cont" => sum(okay.std.stream[Long](okay.cont.top)(forever(0, 1)).start)
    case _ => sum(okay.std.stream[Long](okay.zero.top)(forever(0, 1)).start)

  /** the same, each element mapped in the generator's handler: `Stream.map`, fused */
  @Benchmark
  def generateMapped(): Long = backend match
    case "cont" => sum(okay.std.stream[Long](okay.cont.top)(forever(0, 1)).map(_ + 1).start)
    case _ => sum(okay.std.stream[Long](okay.zero.top)(forever(0, 1)).map(_ + 1).start)

  /** `n` elements of an infinite generator taken: each element's rest run in place by the handler, the rest after
   * the `n`th dropped */
  @Benchmark
  def generateTaken(): Long = backend match
    case "cont" => okay.std.stream[Long](okay.cont.top)(forever(0, 1)).take(n).sum
    case _ => okay.std.stream[Long](okay.zero.top)(forever(0, 1)).take(n).sum

  /** `n` elements of a finite generator summed, pushed: each where it is yielded, no rest captured */
  @Benchmark
  def generatePushed(): Long = backend match
    case "cont" => okay.std.stream[Long](okay.cont.top)(fibs(n, 0, 1)).fold(0L)(_ + _)
    case _ => okay.std.stream[Long](okay.zero.top)(fibs(n, 0, 1)).fold(0L)(_ + _)

  /** the first `n` elements summed, each pulled by a run of the rest: no list made */
  private def sum(s0: okay.std.Step[Long]): Long =
    var s = s0
    var i = 0
    var sum = 0L
    while i < n do
      s match
        case e: okay.std.Step.Next[Long] => sum += e.w; i += 1; if i < n then s = e.pull()
        case e: okay.std.Step.Later[Long] => sum += e.w; i += 1; if i < n then s = e.pull()
        case okay.std.Step.Done() => i = n
    sum

  /** a shift under a reset: the rest 256 maps, `k` 8 times; the rest one map, `k` once */
  @Benchmark
  def captureDeep(): Int = run[Int](reset[Int](captured(256, 8)))
  @Benchmark
  def captureOnce(): Int = run[Int](reset[Int](captured(1, 1)))
