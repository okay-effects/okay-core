package okay.core

import okay.cont.{Cont, Free, At}

import Effects.*
import Control.*
import okay.std.*

/** `PState`: the state's type in the answer type */
/** `PState` on any monad of master's `Effects`: the state's type in the answer type */
abstract class TestPStateOn(on: Backend) extends BackendSuite(on):

  test("pstate: get after put, the type unchanged"):
    val r: (Int, Int) = run(pstate(1)(st => st.put[Int](41).flatMap(_ => st.get[Int]).map(_ + 1)))
    assertEquals(r, (41, 42))

  test("pstate: the TYPE changed — an Int in, a String put, read back as a String"):
    val r: (String, Int) = run(pstate(7)(st => st.get[Int].flatMap(n => st.put[Int]("x" * n)).flatMap(_ => st.get[String]).map(_.length)))
    assertEquals(r, ("xxxxxxx", 7))

  test("pstate: a get of the wrong type does not compile — the answer types do not meet"):
    assert(compileErrors("""val r: (Int, Int) = run(pstate(1)(st => st.put[Int]("s").flatMap(_ => st.get[Int])))""").nonEmpty)

/** on `Cont` */
class TestPState extends TestPStateOn(Backend.cont):

  test("pstate under an effect of the row: an operation between the moves, at the answer there"):
    // in the body the answer moves, so an operation is the answer-polymorphic node, not `ask` at the context's answer
    // the context passed by hand, the same to `pstate` and to the body: `PState` is over that very context
    def body(c: Effects)(using has: Reader % Int :? c.Effs)(using r: c.Reset[St[c.M, c.S, String, (String, Int)]])(st: PState[c.type, (String, Int)]) =
      st.get[Int].flatMap(n => r.perform[Reader % Int, st.At[Int], Int](Reader.Ask(), has).flatMap(a => st.put[Int]((n + a).toString))).map(_ => 0)
    val r: (String, Int) = okay.cont.run(reader(5):
      val c = summon[At[Reader % Int +: Pure, (String, Int)]]
      pstate(1)(using c)(body(c)))
    assertEquals(r, ("6", 0))
