package okay.core

import okay.std.*

/** `PState`: the state's type in the answer type, in control; a program of effects by `lower` */
class TestPState extends munit.FunSuite:

  test("pstate: get after put, the type unchanged"):
    val r: (Int, Int) = run(pstate[Pure, Int, Int, Int](1)(st => st.put[Int](41).flatMap(_ => st.get[Int]).map(_ + 1)))
    assertEquals(r, (41, 42))

  test("pstate: the TYPE changed — an Int in, a String put, read back as a String"):
    val r: (String, Int) = run(pstate[Pure, Int, String, Int](7)(st =>
      st.get[Int].flatMap(n => st.put[Int]("x" * n)).flatMap(_ => st.get[String]).map(_.length)))
    assertEquals(r, ("xxxxxxx", 7))

  test("pstate: a get of the wrong type does not compile — the answer types do not meet"):
    assert(compileErrors("""run(pstate[Pure, Int, Int, Int](1)(st => st.put[Int]("s").flatMap(_ => st.get[Int])))""").nonEmpty)

  test("pstate under an effect of the row: an operation between the moves, answered by the handler around"):
    type R = Reader % Int +: Pure
    val r: (String, Int) = run(reader(5)(pstate[R, Int, String, Int](1)(st =>
      st.get[Int].flatMap(n => ask[Int].at[Control.Delim[R]].cont[st.P[Int]].flatMap(a => st.put[Int]((n + a).toString))).map(_ => 0))))
    assertEquals(r, ("6", 0))
