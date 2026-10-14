package okay.core

import okay.core.std.*

/** `intercept`: an effect already in the row handled again for a body — `local`, `handleWith` */
class TestIntercept extends munit.FunSuite:

  test("intercept: a reader's asks in the body answered anew — the reader answering in place outside is not reached"):
    type R = Reader % Int +: Pure
    assertEquals(run(reader(1)(ask[Int].at[R].flatMap(a => local[Int, (Int, Int), R](_ * 10)(ask[Int].flatMap(b => ask[Int].map(c => (b, c)))).map(bc => (a, bc))))),
      (1, (10, 10)))

  test("intercept: a clause's own operation of the effect goes outside the interceptor — nested ones compose"):
    type R = Reader % Int +: Pure
    // the inner clause's ask is the outer interceptor's: (2 + 1) * 10, as tofu's nested `local`s
    assertEquals(run(reader(2)(local[Int, Int, R](_ + 1)(local[Int, Int, Reader % Int +: R](_ * 10)(ask[Int].perform)))), 30)
    assertEquals(run(reader(2)(local[Int, Int, R](_ * 10)(ask[Int].perform).flatMap(a => ask[Int].at[R].map(b => (a, b))))), (20, 2))

  test("intercept: an operation of another effect goes on outside as it is"):
    type R = Reader % Int +: Writer % String +: Pure
    assertEquals(run(writer[String, Int, Pure](reader(3)(local[Int, Int, R](_ + 4)(tell("in").flatMap(_ => ask[Int].perform))))), (List("in"), 7))

  test("intercept: an operation from a handler nested in the body, at the interceptor's path, intercepted"):
    type R = Reader % Int +: Pure
    assertEquals(run(reader(5)(local[Int, (List[String], Int), R](_ * 2)(writer[String, Int, Reader % Int +: R](tell("x").flatMap(_ => ask[Int].perform))))), (List("x"), 10))

  test("intercept: a raise recovered, the rest of the body dropped; no raise, the value; outside the throws untouched"):
    type R = Throws % String +: Pure
    assertEquals(run(throws[String, Int, Pure](handleWith[String, Int, R]((raise("four").perform[Throws % String +: R]: Eff[Throws % String +: R, Int]).map(_ + 100))(e => pure(e.length)))), Right(4))
    assertEquals(run(throws[String, Int, Pure](handleWith[String, Int, R](pure(7))(e => pure(e.length)))), Right(7))
    assertEquals(run(throws[String, Int, Pure](handleWith[String, Int, R](pure(7))(e => pure(e.length)).flatMap(_ => raise("out").perform[R]))), Left("out"))

  test("intercept: under a state, the state's register one — the interceptor answers only its own effect"):
    type R = State % Int +: Reader % Int +: Pure
    def body: (Int, Int) ! State % Int + Reader % Int = put(5).flatMap(_ => get[Int].perform).flatMap(s => ask[Int].map(a => (s, a)))
    assertEquals(run(reader(1)(state(0)(local[Int, (Int, Int), R](_ + 1)(body)))), (5, (5, 2)))

  test("intercept: 100 000 intercepted operations in constant stack"):
    type R = Reader % Int +: Pure
    def asks(i: Int, acc: Int): Int ! Reader % Int =
      if i == 0 then pure(acc) else ask[Int].flatMap(v => asks(i - 1, acc + v))
    assertEquals(run(reader(1)(local[Int, Int, R](_ + 1)(asks(100000, 0)))), 200000)
