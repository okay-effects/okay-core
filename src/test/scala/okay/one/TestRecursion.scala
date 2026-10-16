package okay.one

import okay.one.Cont.*
import okay.one.Handlers.*
import okay.one.row.*

/** RECURSION IN CONSTANT STACK: functions calling each other in tail position — through `flatMap` or `defer` — and a
 * loop (`tailRecM`), a million steps, with and without an operation at each, on the suites' 512k stack.
 * okay-rust's tests/recursion.rs, the same programs */
class TestRecursion extends munit.FunSuite:
  val million = 1000000

  def isEven(i: Int): Boolean ! Pure = if i == 0 then pure(true) else defer(isOdd(i - 1))
  def isOdd(i: Int): Boolean ! Pure = if i == 0 then pure(false) else defer(isEven(i - 1))

  test("mutual tail recursion by defer: a million steps, constant stack") {
    assert(isEven(million).run)
    assert(!isEven(million + 1).run)
  }

  type Counted = St +: Pure
  def evenCounted(i: Int): Boolean ! Counted =
    if i == 0 then pure(true)
    else perform[St, Counted, Int](St.Get).flatMap(s => perform[St, Counted, Unit](St.Put(s + 1))).flatMap(_ => oddCounted(i - 1))
  def oddCounted(i: Int): Boolean ! Counted =
    if i == 0 then pure(false)
    else perform[St, Counted, Int](St.Get).flatMap(s => perform[St, Counted, Unit](St.Put(s + 1))).flatMap(_ => evenCounted(i - 1))

  test("mutual tail recursion with a state at each step: two million operations") {
    assertEquals(evenCounted(million).handle(state(0)).run, (million, true))
  }

  enum Call:
    case Even(i: Int)
    case Odd(i: Int)

  test("mutual recursion by tailRecM: the state names the function, a reader asked at each step") {
    type R = Ask +: Pure
    val p: Boolean ! R = tailRecM[R, Call, Boolean](Call.Even(million)) { c =>
      perform[Ask, R, Int](Ask.Get).map(step => c match
        case Call.Even(0) => Right(true)
        case Call.Odd(0) => Right(false)
        case Call.Even(i) => Left(Call.Odd(i - step))
        case Call.Odd(i) => Left(Call.Even(i - step)))
    }
    assert(p.handle(reader(1)).run)
  }

  test("a loop under a choice: each path its own loop, the rest resumed into it") {
    type R = Choose +: Pure
    val p: Int ! R = perform[Choose, R, Boolean](Choose.Flip).flatMap { b =>
      val n = if b then 10 else 1000
      tailRecM[R, (Int, Int), Int]((0, 0)) { case (i, acc) => pure(if i == n then Right(acc) else Left((i + 1, acc + i))) }
    }
    assertEquals(p.handle(choose).run, List(45, 499500))
  }
