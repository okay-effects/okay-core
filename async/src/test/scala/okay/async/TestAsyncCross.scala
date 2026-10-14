package okay.async

import okay.core.*
import okay.core.std.*

/** `Async` on every platform: what needs no other thread — computations, callbacks answered at once, a run not
 * waited for (`subscribe`); where the platform cannot wait (JS), a run waited for and not complete at once fails
 * saying so */
class TestAsyncCross extends munit.FunSuite:
  def io: Int ! Async = async(20).flatMap(x => await[Int](cb => cb(Right(x + 1))))

  test("Async.run: computations and callbacks answered at once"):
    assertEquals(run(throws[Throwable, Int, Pure](Async.run(io))), Right(21))

  test("an async stream pushed and pulled, its callbacks answered at once"):
    def ticks: Unit ! Emit % Int + Async =
      async(1).flatMap(yield_(_)).flatMap(_ => await[Int](cb => cb(Right(2))).flatMap(yield_(_)))
    assertEquals(Async.stream[Int](ticks).toList, List(1, 2))
    assertEquals(Async.stream[Int](ticks).iterator.toList, List(1, 2))

  test("subscribe: a run not waited for, its callback coming later — on every platform"):
    var later: Either[Throwable, Int] => Unit = _ => ()
    def body: Unit ! Emit % Int + Async = await[Int](cb => later = cb).flatMap(yield_(_))
    var got = List.empty[Int]
    var end: Either[Throwable, Unit] | Null = null
    Async.stream[Int](body).subscribe(w => got = w :: got)(end = _)
    assertEquals((got, end), (Nil, null))
    later(Right(7))
    assertEquals((got, end), (List(7), Right(())))

  test("where the platform cannot wait: Async.run of a callback not come fails, saying so"):
    assume(!Platform.waits, "this platform waits: the run would wait for the callback")
    def never: Int ! Async = await[Int](_ => ())
    val e = intercept[IllegalStateException](run(throws[Throwable, Int, Pure](Async.run(never))))
    assert(e.getMessage.contains("did not complete at once"), e.getMessage)
