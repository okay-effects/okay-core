package okay.core

import okay.core.std.*
import okay.core.Control.reset

/** DELIMITERS NESTED DEEP — a handler in a handler in a handler…, as a recursive function installing one each call
 * makes: the handlers a stack of the machine's own, on the heap; the body built when the run enters the handler
 * (docs/stack-safety.md) — any depth in constant stack */
class TestNesting extends munit.FunSuite:
  def readers[T <: Row](n: Int): Eff[T, Int] = if n == 0 then pure(0) else reader[Int, Int, T](n)(readers[Reader % Int +: T](n - 1).map(_ + 1))
  def resets[T <: Row](n: Int): Eff[T, Int] = if n == 0 then pure(0) else reset[Int, T](resets[Shifts[Int, T] +: T](n - 1).map(_ + 1))

  test("1 000 handlers nested, each installed by a recursive call; 1 000 resets"):
    assertEquals(run(readers[Pure](1000)), 1000)
    assertEquals(run(resets[Pure](1000)), 1000)

  test("100 000 handlers nested, each installed by a recursive call; 100 000 resets — in constant stack"):
    assertEquals(run(readers[Pure](100000)), 100000)
    assertEquals(run(resets[Pure](100000)), 100000)
