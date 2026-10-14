package okay.one

import okay.one.Cont.*
import okay.one.Handlers.*
import okay.one.row.*
import okay.one.std.PState
import okay.one.std.PState.pstate

/** okay-core's TestPState: the state's type in the answer type, in control */
class TestPState extends munit.FunSuite:
  test("pstate: get after put, the type unchanged") {
    val p = pstate[Pure, Nothing, Int, Int, Int](1)(st ?=> st.put[Int](41).flatMap(_ => st.get[Int]).map(_ + 1))
    assertEquals(p.run, (41, 42))
  }

  test("pstate: the TYPE changed — an Int in, a String put, read back as a String") {
    val p = pstate[Pure, Nothing, Int, String, Int](7)(st ?=>
      st.get[Int].flatMap(n => st.put[Int]("x" * n)).flatMap(_ => st.get[String]).map(_.length))
    assertEquals(p.run, ("xxxxxxx", 7))
  }

  test("pstate: a get of the wrong type does not compile — the answer types do not meet") {
    assert(compileErrors("""pstate[Pure, Nothing, Int, Int, Int](1)(st ?=> st.put[Int]("s").flatMap(_ => st.get[Int]))""").nonEmpty)
  }

  test("pstate under an effect of the row: an operation between the moves, out of the $ to its handler") {
    type R = Ask +: Pure
    val p = pstate[R, Nothing, Int, String, Int](1)(st ?=>
      st.get[Int].flatMap(n => perform[Ask, Delim[R], Int](Ask.Get).cont[st.P[Int]].flatMap(a => st.put[Int]((n + a).toString)))
        .map(_ => 0))
    assertEquals(p.handle(reader(5)).run, ("6", 0))
  }
