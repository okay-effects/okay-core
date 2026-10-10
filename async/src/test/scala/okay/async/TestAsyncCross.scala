package okay.async

import okay.core.*
import okay.std.*

/** `Async` on every platform: what needs no other thread — computations, callbacks answered at once, a run not
 * waited for (`subscribe`); where the platform cannot wait (JS), a run waited for and not complete at once fails
 * saying so */
class TestAsyncCross extends munit.FunSuite:
  def io(using c: Effects, a: Async :? c.Effs): c.Prog[Int] = async(20).flatMap(x => await[Int](cb => cb(Right(x + 1))))

  test("Async.run: computations and callbacks answered at once, on Cont and on Cont0"):
    assertEquals(okay.cont.run(throws[Throwable, Int](Async.run(io))), Right(21))
    assertEquals(okay.zero.run(throws[Throwable, Int](Async.run(io))), Right(21))

  test("an async stream pushed and pulled, its callbacks answered at once"):
    def ticks(using c: Effects, e: Emit % Int :? c.Effs, a: Async :? c.Effs): c.Prog[Unit] =
      async(1).flatMap(yield_(_)).flatMap(_ => await[Int](cb => cb(Right(2))).flatMap(yield_(_)))
    assertEquals(Async.stream[Int](okay.zero.top)(ticks).toList, List(1, 2))
    assertEquals(Async.stream[Int](okay.cont.top)(ticks).iterator.toList, List(1, 2))

  test("subscribe: a run not waited for, its callback coming later — on every platform"):
    var later: Either[Throwable, Int] => Unit = _ => ()
    def body(using c: Effects, e: Emit % Int :? c.Effs, a: Async :? c.Effs): c.Prog[Unit] =
      await[Int](cb => later = cb).flatMap(yield_(_))
    var got = List.empty[Int]
    var end: Either[Throwable, Unit] | Null = null
    Async.stream[Int](okay.zero.top)(body).subscribe(w => got = w :: got)(end = _)
    assertEquals((got, end), (Nil, null))
    later(Right(7))
    assertEquals((got, end), (List(7), Right(())))

  test("where the platform cannot wait: Async.run of a callback not come fails, saying so"):
    assume(!Platform.waits, "this platform waits: the run would wait for the callback")
    def never(using c: Effects, a: Async :? c.Effs): c.Prog[Int] = await[Int](_ => ())
    val e = intercept[IllegalStateException](okay.zero.run(throws[Throwable, Int](Async.run(never))))
    assert(e.getMessage.contains("did not complete at once"), e.getMessage)
