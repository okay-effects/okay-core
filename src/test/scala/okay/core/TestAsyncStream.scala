package okay.core

import okay.std.*

import java.util.concurrent.Executors

/** an asynchronous stream on a monad's top: elements from callbacks, the body never ahead of its consumer */
abstract class TestAsyncStreamOn(top: Top, stackSafe: Boolean = true) extends munit.FunSuite:
  override def munitTestTransforms: List[TestTransform] = super.munitTestTransforms :+ TestTransform("stack", t =>
    if stackSafe then t else t.withBody(() => BackendSuite.onLargeStack(t.body())))

  def later[A](pool: java.util.concurrent.Executor, a: A)(using c: Effects, has: Async :? c.Effs): c.Prog[A] =
    await[A](cb => pool.execute(() => cb(Right(a))))

  test("async stream: elements from callbacks of other threads, as an iterator"):
    val pool = Executors.newFixedThreadPool(2)
    try
      def body(using c: Effects, e: Emit % Int :? c.Effs, a: Async :? c.Effs): c.Prog[Unit] =
        later(pool, 1).flatMap(yield_(_)).flatMap(_ => later(pool, 2).flatMap(yield_(_))).flatMap(_ => async(3).flatMap(yield_(_)))
      assertEquals(Async.iterator[Int](top)(body).toList, List(1, 2, 3))
    finally pool.shutdown()

  test("async stream: back-pressure — the body waits at each yield until the consumer asks"):
    var made = 0
    def from(i: Int)(using c: Effects, e: Emit % Int :? c.Effs, a: Async :? c.Effs): c.Prog[Unit] =
      async { made += 1; i }.flatMap(yield_(_)).flatMap(_ => from(i + 1))
    val it = Async.iterator[Int](top)(from(0))
    assertEquals(made, 0)
    assertEquals(it.take(3).toList, List(0, 1, 2))
    assertEquals(made, 3)

  test("async stream: pushed, the consumer asking in place — 100 000 elements in constant stack, then done"):
    def upTo(i: Int, n: Int)(using c: Effects, e: Emit % Int :? c.Effs, a: Async :? c.Effs): c.Prog[Unit] =
      if i > n then c.pure(()) else await[Int](cb => cb(Right(i))).flatMap(yield_(_)).flatMap(_ => upTo(i + 1, n))
    var sum = 0L
    var end: Async.Step[Int] | Null = null
    Async.stream[Int](top)(upTo(1, 100000)):
      case Async.Step.Next(w, more) => sum += w; more()
      case s => end = s
    assertEquals((sum, end), (5000050000L, Async.Step.Done[Int]()))

  test("async stream: a failure at a callback ends the stream, thrown by the iterator after the elements before it"):
    def body(using c: Effects, e: Emit % Int :? c.Effs, a: Async :? c.Effs): c.Prog[Unit] =
      yield_(1).flatMap(_ => await[Int](cb => cb(Left(IllegalStateException("cb"))))).flatMap(yield_(_))
    val it = Async.iterator[Int](top)(body)
    assertEquals(it.next(), 1)
    assertEquals(intercept[IllegalStateException](it.hasNext).getMessage, "cb")

class TestAsyncStream extends TestAsyncStreamOn(okay.cont.top)
class TestAsyncStreamOnCont0 extends TestAsyncStreamOn(okay.zero.top)
class TestAsyncStreamOnRef extends TestAsyncStreamOn(okay.zero.Ref.top)
/** the textbook CPS, not stack safe: on a thread of a large stack */
class TestAsyncStreamOnCps0 extends TestAsyncStreamOn(Cps0.top, stackSafe = false)
