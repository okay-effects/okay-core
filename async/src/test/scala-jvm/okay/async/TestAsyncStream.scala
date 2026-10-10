package okay.async

import okay.core.*
import okay.std.*

import java.util.concurrent.Executors

/** an asynchronous source on a monad's top: elements from callbacks, the body never ahead of its consumer */
abstract class TestAsyncStreamOn(top: Top, stackSafe: Boolean = true) extends munit.FunSuite:
  override def munitTestTransforms: List[TestTransform] = super.munitTestTransforms :+ TestTransform("stack", t =>
    if stackSafe then t else t.withBody(() => BackendSuite.onLargeStack(t.body())))

  def later[A](pool: java.util.concurrent.Executor, a: A)(using c: Effects, has: Async :? c.Effs): c.Prog[A] =
    await[A](cb => pool.execute(() => cb(Right(a))))
  def upTo(i: Int, n: Int)(using c: Effects, e: Emit % Int :? c.Effs, a: Async :? c.Effs): c.Prog[Unit] =
    if i > n then c.pure(()) else await[Int](cb => cb(Right(i))).flatMap(yield_(_)).flatMap(_ => upTo(i + 1, n))

  test("async source: elements from callbacks of other threads — pulled, pushed, map and filter fused"):
    val pool = Executors.newFixedThreadPool(2)
    try
      def body(using c: Effects, e: Emit % Int :? c.Effs, a: Async :? c.Effs): c.Prog[Unit] =
        later(pool, 1).flatMap(yield_(_)).flatMap(_ => later(pool, 2).flatMap(yield_(_))).flatMap(_ => async(3).flatMap(yield_(_)))
      val src = Async.stream[Int](top)(body)
      assertEquals(src.iterator.toList, List(1, 2, 3))
      assertEquals(src.toList, List(1, 2, 3))
      assertEquals(src.map(_ * 10).filter(_ > 10).toList, List(20, 30))
      assertEquals(src.map(_ * 10).filter(_ > 10).take(1), List(20))
    finally pool.shutdown()

  test("async source: back-pressure — the body waits at each yield until asked; take cancels the rest"):
    var made = 0
    def from(i: Int)(using c: Effects, e: Emit % Int :? c.Effs, a: Async :? c.Effs): c.Prog[Unit] =
      async { made += 1; i }.flatMap(yield_(_)).flatMap(_ => from(i + 1))
    val src = Async.stream[Int](top)(from(0))
    assertEquals(made, 0)
    assertEquals(src.take(3), List(0, 1, 2))
    assertEquals(made, 3)

  test("async source: close cancels the run where it waits — after an element taken, or before"):
    var made = 0
    def from(i: Int)(using c: Effects, e: Emit % Int :? c.Effs, a: Async :? c.Effs): c.Prog[Unit] =
      async { made += 1; i }.flatMap(yield_(_)).flatMap(_ => from(i + 1))
    val it = Async.stream[Int](top)(from(0)).iterator
    assertEquals(it.next(), 0)
    it.close()
    assertEquals((made, it.hasNext), (1, false))
    val it2 = Async.stream[Int](top)(from(0)).iterator
    assertEquals(it2.hasNext, true)
    it2.close()
    assertEquals(it2.hasNext, false)

  test("async source: subscribed, not waiting — 100 000 elements in constant stack, then done"):
    var sum = 0L
    var end: Either[Throwable, Unit] | Null = null
    Async.stream[Int](top)(upTo(1, 100000)).subscribe(sum += _)(end = _)
    assertEquals((sum, end), (5000050000L, Right(())))
    assertEquals(Async.stream[Int](top)(upTo(1, 100000)).fold(0L)(_ + _), 5000050000L)

  test("async source: a failure at a callback ends it — the elements before it, then thrown"):
    def body(using c: Effects, e: Emit % Int :? c.Effs, a: Async :? c.Effs): c.Prog[Unit] =
      yield_(1).flatMap(_ => await[Int](cb => cb(Left(IllegalStateException("cb"))))).flatMap(yield_(_))
    val it = Async.stream[Int](top)(body).iterator
    assertEquals(it.next(), 1)
    assertEquals(intercept[IllegalStateException](it.hasNext).getMessage, "cb")
    assertEquals(intercept[IllegalStateException](Async.stream[Int](top)(body).toList).getMessage, "cb")

class TestAsyncStream extends TestAsyncStreamOn(okay.cont.top)
class TestAsyncStreamOnCont0 extends TestAsyncStreamOn(okay.zero.top)
class TestAsyncStreamOnRef extends TestAsyncStreamOn(okay.zero.Ref.top)
/** the textbook CPS, not stack safe: on a thread of a large stack */
class TestAsyncStreamOnCps0 extends TestAsyncStreamOn(Cps0.top, stackSafe = false)
