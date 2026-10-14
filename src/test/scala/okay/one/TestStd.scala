package okay.one

import okay.one.Cont.pure
import okay.one.row.*
import okay.one.std.*

/** the standard effects, their handlers; async in TestStdAsync, the JVM's and Native's: a thread, a wait */
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
