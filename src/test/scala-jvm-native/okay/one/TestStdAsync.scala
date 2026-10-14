package okay.one

import okay.one.Cont.pure
import okay.one.row.*
import okay.one.std.*
import scala.concurrent.{Await as Wait, Promise}
import scala.concurrent.duration.*

/** ASYNC by the program stepped — the JVM's and Native's: a callback from another thread, the result awaited (no
 * thread, no wait on Scala.js: the shared suites link there) */
class TestStdAsync extends munit.FunSuite:
  def result[A](p: A ! Async +: Pure): Either[Throwable, A] =
    val r = Promise[Either[Throwable, A]]()
    Async.run(p)(r.success)
    Wait.result(r.future, 10.seconds)

  test("async: callbacks at once, and from another thread; handlers in the program") {
    type R = Async +: Pure
    type Q = State % Int +: R
    def later(n: Int): Int ! Q = Async.await[Int, Q](cb => Thread(() => { Thread.sleep(5); cb(Right(n)) }).start())
    val p = (for a <- Async.await[Int, Q](cb => cb(Right(1))); b <- later(2); _ <- State.put[Int, Q](a + b); s <- State.get[Int, Q]
      yield s * 10).handle(State(0))
    assertEquals(result(p), Right((3, 30)))
  }

  test("async: an error given back ends the run") {
    val boom = RuntimeException("boom")
    val p = Async.await[Int, Async +: Pure](cb => cb(Left(boom))).map(_ + 1)
    assertEquals(result(p), Left(boom))
  }

  test("async: stack safe — a loop of callbacks called at once") {
    def loop(i: Int, acc: Int): Int ! Async +: Pure =
      if i == 0 then pure(acc) else Async.await[Int, Async +: Pure](cb => cb(Right(1))).flatMap(n => loop(i - 1, acc + n))
    assertEquals(result(loop(100000, 0)), Right(100000))
  }
