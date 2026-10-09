package okay.core

import okay.cont.{Cont, Free, At}

import Effects.handle
import Control.{reset, shift}
import okay.std.*

/** the handlers' semantics, beyond master's tests: a clause outside its handler, the order of handlers with a
 * functional state, two handlers of one effect by path, a program leaving its handler, deep crossings */
abstract class TestDynamicOn(on: Backend) extends BackendSuite(on):
  enum FState[+A]:
    case Get extends FState[Int]
    case Put(n: Int) extends FState[Unit]
  def fget(using c: Effects, in: FState :? c.Effs): c.Prog[Int] = perform(FState.Get)
  def fput(n: Int)(using c: Effects, in: FState :? c.Effs): c.Prog[Unit] = perform(FState.Put(n))
  /** functional state: the answer a function of the state, threaded through `k` — no cell */
  def stating[A](using c: Effects)(s0: Int)(body: c.Body[FState, Int => c.Prog[(Int, A)], A]): c.Prog[(Int, A)] =
    handle(new Handler[c.Prog, FState, A, Int => c.Prog[(Int, A)]]:
      def ret(a: A): Int => c.Prog[(Int, A)] = s => pure((s, a))
      def apply[X](op: FState[X], k: X => c.Prog[Int => c.Prog[(Int, A)]]): c.Prog[Int => c.Prog[(Int, A)]] = op match
        case FState.Get => pure((s: Int) => k(s).flatMap(f => f(s)))
        case FState.Put(n) => pure((_: Int) => k(()).flatMap(f => f(n))))(body).flatMap(f => f(s0))

  def stepped(using c: Effects, s: FState :? c.Effs, ch: Choose :? c.Effs): c.Prog[Int] =
    among(Seq(1, 2)).flatMap(x => fget.flatMap(s => fput(s + x))).flatMap(_ => fget)
  test("the order of handlers is the semantics: functional state per path under choose, one through the paths over it"):
    assertEquals(run(choose[(Int, Int)](stating[Int](0)(stepped))), Seq((1, 1), (2, 2)))
    assertEquals(run(stating[Seq[Int]](0)(choose[Int](stepped))), (3, Seq(1, 3)))

  test("a clause runs outside its handler: an operation in it goes to the handler further out"):
    def loud[A](using c: Effects, w: Writer % String :? c.Effs)(body: c.Body[Reader % Int, A, A]): c.Prog[A] =
      handle(new Handler[c.Prog, Reader % Int, A, A]:
        def ret(a: A): A = a
        def apply[X](op: Reader[Int, X], k: X => c.Prog[A]): c.Prog[A] = op match
          case Reader.Ask() => tell("asked").flatMap(_ => k(7)))(body)
    assertEquals(run(writer[String, Int](loud(ask[Int].flatMap(a => ask[Int].map(_ + a))))), (List("asked", "asked"), 14))

  test("two handlers of one effect: the nearer by default, the further by its path"):
    def outer(using c: Effects { type Effs = Reader % Int +: Reader % Int +: Pure }): c.Prog[Int] =
      perform[Reader % Int, Int](Reader.Ask[Int]())(using c, In.There(In.Here()))
    assertEquals(run(reader[Int](1)(reader[Int](2)(ask[Int].flatMap(a => outer.map(b => a * 10 + b))))), 21)

  test("1 000 operations, each crossing 500 handlers"):
    def loop(n: Int, acc: Int)(using c: Effects, r: Reader % Int :? c.Effs): c.Prog[Int] =
      if n == 0 then pure(acc) else ask[Int].flatMap(x => loop(n - 1, acc + x))
    def deep(d: Int)(using c: Effects, r: Reader % Int :? c.Effs): c.Prog[Int] =
      if d == 0 then loop(1000, 0) else writer[String, Int](deep(d - 1)).map(_._2)
    assertEquals(run(reader[Int](1)(deep(500))), 1000)

  test("a handler resuming twice under 10 000 binds: each path runs the whole rest"):
    def binds(using c: Effects, ch: Choose :? c.Effs): c.Prog[Int] = (1 to 10000).foldLeft(among(Seq(1, 2)))((p, _) => p.flatMap(x => pure(x + 1)))
    assertEquals(run(choose[Int](binds)), Seq(10001, 10002))

  test("100 000 tells, each clause k(()).map(...) — the rest resumed under the maps pending outside"):
    def tells(n: Int)(using c: Effects, w: Writer % String :? c.Effs): c.Prog[Int] = if n == 0 then pure(0) else tell(n.toString).flatMap(_ => tells(n - 1))
    assertEquals(run(writer[String, Int](tells(100000)))._1.length, 100000)

/** on `Cont` */
class TestDynamic extends TestDynamicOn(Backend.cont)
