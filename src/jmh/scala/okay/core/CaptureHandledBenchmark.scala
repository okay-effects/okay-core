package okay.core

import okay.cont.{Cont, Free, At}

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.cont.*
import okay.cont.Cont.*

/** CaptureBenchmark's capture under a HANDLER, no reset between: the handler is the delimiter, so each shot of
 * `k` resumes the frames and the handler's `ret` after them (`captured`, in Effects.scala) */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class CaptureHandledBenchmark:

  @Param(Array("1", "16", "256"))
  var depth: Int = 0

  @Param(Array("1", "8"))
  var shots: Int = 0

  enum Nop[X]:
    case Op extends Nop[Unit]

  def nop[A](using c: Effects): Handler[c.P, Nop, A, A] = new Answering[c.P, Nop, A, A]:
    def ret(a: A): A = a
    def value[X](op: Nop[X]): X = op match
      case Nop.Op => ()

  def calls[R <: Row](k: Unit => Cont[R, Int, Int, Int], i: Int, acc: Int): Cont[R, Int, Int, Int] =
    if i == 0 then pure(acc) else k(()).flatMap(r => calls(k, i - 1, acc + r))

  @Benchmark
  def captureHandled(): Int = okay.cont.run:
    Effects.handle(nop[Int]):
      var p: Cont[Nop +: Pure, Int, Int, Int] = Control.shift[Unit, Int](k => calls(k, shots, 0)).map(_ => 1)
      var i = 0
      while i < depth do
        p = p.map(_ + 1)
        i += 1
      p
