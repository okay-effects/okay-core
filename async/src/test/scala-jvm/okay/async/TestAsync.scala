package okay.async

import okay.core.*
import okay.std.*
import java.util.concurrent.{Executors, TimeUnit}

/** `Async` on any monad that runs it: `runIO` the run, at the top, its failure the `Left` */
abstract class TestAsyncOn extends munit.FunSuite:
  def runIO[A](body: (c: Effects { type Effs = Async +: Async.Fails; type S = Unit }) ?=> c.Prog[A]): Either[Throwable, A]

  test("async: computations run in order, their values bound"):
    assertEquals(runIO[Int](async(20).flatMap(a => async(a + 1)).map(_ * 2)), Right(42))

  test("await: a callback from another thread, the run going on in it"):
    val pool = Executors.newFixedThreadPool(2)
    try
      def later(x: Int)(using c: Effects, has: Async :? c.Effs): c.Prog[Int] =
        await[Int](cb => pool.execute(() => { Thread.sleep(1); cb(Right(x)) }))
      assertEquals(runIO[Int](later(1).flatMap(a => later(2).map(_ + a)).flatMap(a => later(3).map(_ + a))), Right(6))
    finally pool.shutdown()

  test("await: 100 000 callbacks answered at once, in constant stack"):
    def loop(i: Int, acc: Int)(using c: Effects, has: Async :? c.Effs): c.Prog[Int] =
      if i == 0 then c.pure(acc) else await[Int](cb => cb(Right(1))).flatMap(x => loop(i - 1, acc + x))
    assertEquals(runIO[Int](loop(100000, 0)), Right(100000))

  test("async: a failure — a Left at a callback, a throw in a computation — raised, the rest never runs, the rest never runs"):
    var ran = false
    val e1 = runIO[Int](await[Int](cb => cb(Left(IllegalStateException("cb")))).map(x => { ran = true; x }))
    val e2 = runIO[Int](async(1 / 0).map(x => { ran = true; x }))
    assertEquals((e1.left.map(_.getMessage), ran), (Left("cb"), false))
    assertEquals(e2.left.map(_.getMessage), Left("/ by zero"))

  test("async: the program's own raise, a Throws of the row, fails the run"):
    assertEquals(runIO[Int](async(1).flatMap(_ => raise[Throwable, Int](IllegalStateException("own")))).left.map(_.getMessage), Left("own"))

  test("async with state inside: the state's handler goes on through the callbacks"):
    val pool = Executors.newSingleThreadExecutor()
    try
      val r = runIO[(Int, Int)](state[Int](0)(
        await[Int](cb => pool.execute(() => cb(Right(5)))).flatMap(x => modify[Int](_ + x)).flatMap(_ =>
          await[Int](cb => pool.execute(() => cb(Right(7)))).flatMap(y => modify[Int](_ + y)))))
      assertEquals(r, Right((12, 12)))
    finally pool.shutdown()

  def bang: Int ! Reader % Int + Async = ask[Int].flatMap(a => async(a + 1))
  test("async in A ! Es"):
    assertEquals(runIO[Int](reader(41)(bang)), Right(42))

/** on `Cont` */
class TestAsync extends TestAsyncOn:
  def runIO[A](body: (c: Effects { type Effs = Async +: Async.Fails; type S = Unit }) ?=> c.Prog[A]): Either[Throwable, A] =
    okay.cont.run(throws[Throwable, A](Async.run(body)))

/** on the textbook CPS, not stack safe: on a thread of a large stack */
class TestAsyncOnCps0 extends TestAsyncOn:
  def runIO[A](body: (c: Effects { type Effs = Async +: Async.Fails; type S = Unit }) ?=> c.Prog[A]): Either[Throwable, A] =
    okay.core.BackendSuite.onLargeStack(okay.core.Cps0.run(throws[Throwable, A](Async.run(body))))

/** master's tests of `Async` on `Cont0`: its top, `okay.zero.top`, the run */
class TestAsyncOnZero extends TestAsyncOn:
  def runIO[A](body: (c: Effects { type Effs = Async +: Async.Fails; type S = Unit }) ?=> c.Prog[A]): Either[Throwable, A] =
    okay.zero.run(throws[Throwable, A](Async.run(body)))

/** `Async` on the reference: its top */
class TestAsyncOnSpec extends TestAsyncOn:
  def runIO[A](body: (c: Effects { type Effs = Async +: Async.Fails; type S = Unit }) ?=> c.Prog[A]): Either[Throwable, A] =
    okay.zero.Spec.run(throws[Throwable, A](Async.run(body)))
