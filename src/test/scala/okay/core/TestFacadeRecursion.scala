package okay.core

import okay.core.std.*

/** RECURSION IN CONSTANT STACK in the facade: functions calling each other in tail position — through `flatMap` or
 * `defer` — and `tailRecM`, a million steps, an operation at each */
class TestFacadeRecursion extends munit.FunSuite:
  val million = 1000000

  def isEven(i: Int): Boolean ! Reader % Int = if i == 0 then pure(true) else ask[Int].flatMap(d => isOdd(i - d))
  def isOdd(i: Int): Boolean ! Reader % Int = if i == 0 then pure(false) else ask[Int].flatMap(d => isEven(i - d))

  test("mutual tail recursion in the facade: a million steps, a reader asked at each"):
    assert(run(reader(1)(isEven(million))))

  test("tailRecM in the facade: a million steps, a state at each"):
    def count: Int ! State % Int =
      tailRecM(0)(i => if i == million then get[Int].perform.map(Right(_)) else modify[Int](_ + 1).map(_ => Left(i + 1)))
    assertEquals(run(state(0)(count)), (million, million))
