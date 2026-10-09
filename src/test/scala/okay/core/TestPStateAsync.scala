package okay.core

import okay.cont.{Cont, Free, At}

import Effects.*
import Control.*
import okay.std.*
import java.util.concurrent.{Executors, TimeUnit}

/** `PState`: the state's type in the answer type; `Async`: a run that waits on callbacks */
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

/** on `Cont`, and `Async`: a run that waits on callbacks */
class TestPStateAsync extends TestPStateOn(Backend.cont):

  test("pstate under an effect of the row: an operation between the moves, at the answer there"):
    // in the body the answer moves, so an operation is the answer-polymorphic node, not `ask` at the context's answer
    // the context passed by hand, the same to `pstate` and to the body: `PState` is over that very context
    def body(c: Effects)(using has: Reader % Int :? c.Effs)(using r: c.Reset[St[c.M, c.Effs, c.S, String, (String, Int)]])(st: PState[c.type, (String, Int)]) =
      st.get[Int].flatMap(n => r.perform[Reader % Int, c.Effs, st.At[Int], Int](Reader.Ask(), has).flatMap(a => st.put[Int]((n + a).toString))).map(_ => 0)
    val r: (String, Int) = okay.cont.run(reader(5):
      val c = summon[At[Reader % Int +: Pure, (String, Int)]]
      pstate(1)(using c)(body(c)))
    assertEquals(r, ("6", 0))

/** `Async` on any monad that runs it: `runIO` the run, at the top, its failure the `Left` */
abstract class TestAsyncOn extends munit.FunSuite:
  def runIO[A](body: (c: Effects { type Effs = Async +: Async.Fails; type S = Unit }) ?=> c.Prog[A]): Either[Throwable, A]

  test("async: computations run in order, their values bound"):
    assertEquals(runIO[Int](async(20).flatMap(a => async(a + 1)).map(_ * 2)), Right(42))

  test("await: a callback from another thread, the run going on in it"):
    val pool = Executors.newFixedThreadPool(2)
    try
      def later(x: Int)(using c: Effects, has: Async :? c.Effs): c.Prog[Int] =
        await[Int](cb => pool.execute(() => { Thread.sleep(1); cb(Right(x)) }))
      assertEquals(runIO[Int](later(1).flatMap(a => later(2).map(_ + a)).flatMap(a => later(3).map(_ + a))), Right(6))
    finally pool.shutdown()

  test("await: 100 000 callbacks answered at once, in constant stack"):
    def loop(i: Int, acc: Int)(using c: Effects, has: Async :? c.Effs): c.Prog[Int] =
      if i == 0 then c.pure(acc) else await[Int](cb => cb(Right(1))).flatMap(x => loop(i - 1, acc + x))
    assertEquals(runIO[Int](loop(100000, 0)), Right(100000))

  test("async: a failure — a Left at a callback, a throw in a computation — raised, the rest never runs, the rest never runs"):
    var ran = false
    val e1 = runIO[Int](await[Int](cb => cb(Left(IllegalStateException("cb")))).map(x => { ran = true; x }))
    val e2 = runIO[Int](async(1 / 0).map(x => { ran = true; x }))
    assertEquals((e1.left.map(_.getMessage), ran), (Left("cb"), false))
    assertEquals(e2.left.map(_.getMessage), Left("/ by zero"))

  test("async: the program's own raise, a Throws of the row, fails the run"):
    assertEquals(runIO[Int](async(1).flatMap(_ => raise[Throwable, Int](IllegalStateException("own")))).left.map(_.getMessage), Left("own"))

  test("async with state inside: the state's handler goes on through the callbacks"):
    val pool = Executors.newSingleThreadExecutor()
    try
      val r = runIO[(Int, Int)](state[Int](0)(
        await[Int](cb => pool.execute(() => cb(Right(5)))).flatMap(x => modify[Int](_ + x)).flatMap(_ =>
          await[Int](cb => pool.execute(() => cb(Right(7)))).flatMap(y => modify[Int](_ + y)))))
      assertEquals(r, Right((12, 12)))
    finally pool.shutdown()

  def bang: Int ! Reader % Int +: Async +: Pure = ask[Int].flatMap(a => async(a + 1))
  test("async in A ! Es"):
    assertEquals(runIO[Int](reader(41)(bang)), Right(42))

/** on `Cont` */
class TestAsync extends TestAsyncOn:
  def runIO[A](body: (c: Effects { type Effs = Async +: Async.Fails; type S = Unit }) ?=> c.Prog[A]): Either[Throwable, A] =
    okay.cont.run(throws[Throwable, A](okay.cont.Async.run(body(using summon[okay.cont.At[Async +: Async.Fails, Unit]]))))
