package okay.one

import okay.one.Cont0.pure
import okay.one.row.*
import okay.one.std.*
import scala.concurrent.{Await as Wait, Promise}
import scala.concurrent.duration.*

/** the standard effects, their handlers; async by the program stepped */
class TestStd extends munit.FunSuite:
  test("reader, state, writer, throws: each by its handler, any order of the row") {
    type R = Reader % Int +: State % Int +: Writer % String +: Throws % String +: Pure
    val p: Int ! R = for
      n <- Reader.ask[Int, R]
      _ <- State.modify[Int, R](_ + n)
      s <- State.get[Int, R]
      _ <- Writer.tell[String, R](s"s $s")
      _ <- if s > 100 then Throws.raise[String, R]("big") else pure[R, Unit](())
    yield s * 2
    assertEquals(p.handle(Reader(5)).handle(State(1)).handle(Writer.collect).handle(Throws.either).run,
      Right((List("s 6"), (6, 12))))
    assertEquals(p.handle(Throws.either).handle(Reader(500)).handle(State(1)).handle(Writer.collect).run,
      (List("s 501"), (501, Left("big"))))
  }

  test("choose: every path, a state inside each its own") {
    type R = Choose +: State % Int +: Pure
    val p = for b <- Choose.flip[R]; i <- Choose.among[Int, R](1 to 2); _ <- State.modify[Int, R](_ + i) ; s <- State.get[Int, R]
      yield (b, s)
    assertEquals(p.handle(Choose.all).handle(State(0)).run._2, List((true, 1), (true, 3), (false, 4), (false, 6)))
  }

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
