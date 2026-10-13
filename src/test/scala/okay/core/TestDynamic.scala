package okay.core

import okay.one.handler.Deep
import okay.std.*

/** the handlers' semantics: a clause outside its handler, the order of handlers with a functional state, two
 * handlers of one effect by path, deep crossings */
class TestDynamic extends munit.FunSuite:
  enum FState[A]:
    case Get() extends FState[Int]
    case Put(n: Int) extends FState[Unit]
  def fget: Op[FState, Int] = perform(FState.Get())
  def fput(n: Int): Op[FState, Unit] = perform(FState.Put(n))
  /** functional state written by hand: the answer a function of the state, threaded through `k` — no register */
  def stating[A, T <: Row](s0: Int)(body: Under[FState, T, A]): Eff[T, (Int, A)] =
    handleIn(new Deep[FState, T, A, Int => Eff[T, (Int, A)]]:
      def ret(a: A): Eff[T, Int => Eff[T, (Int, A)]] = pure(s => pure((s, a)))
      def apply[X](op: FState[X], k: X => Eff[T, Int => Eff[T, (Int, A)]]): Eff[T, Int => Eff[T, (Int, A)]] = op match
        case FState.Get() => pure((s: Int) => k(s).flatMap(f => f(s)))
        case FState.Put(n) => pure((_: Int) => k(()).flatMap(f => f(n))))(body).flatMap(f => f(s0))

  def stepped: Int ! FState + Choose = among(Seq(1, 2)).flatMap(x => fget.flatMap(s => fput(s + x))).flatMap(_ => fget.perform)
  test("the order of handlers is the semantics: functional state per path under choose, one through the paths over it"):
    assertEquals(run(choose(stating(0)(stepped))), Seq((1, 1), (2, 2)))
    assertEquals(run(stating(0)(choose(stepped))), (3, Seq(1, 3)))

  test("a clause runs outside its handler: an operation in it goes to the handler further out"):
    def loud[A, T <: Row](body: Under[Reader % Int, T, A])(using In[Writer % String, T]): Eff[T, A] =
      handleIn(new Deep[Reader % Int, T, A, A]:
        def ret(a: A): Eff[T, A] = pure(a)
        def apply[X](op: Reader[Int, X], k: X => Eff[T, A]): Eff[T, A] = op match
          case Reader.Ask() => tell("asked").at[T].flatMap(_ => k(7)))(body)
    assertEquals(run(writer[String, Int, Pure](loud(ask[Int].flatMap(a => ask[Int].map(_ + a))))), (List("asked", "asked"), 14))

  test("two handlers of one effect: the nearer by default, the further by its path"):
    type R2 = Reader % Int +: Reader % Int +: Pure
    val outer: Eff[R2, Int] = ask[Int].at[R2](using Path(In.There(In.Here())))
    assertEquals(run(reader(1)(reader(2)(ask[Int].at[R2].flatMap(a => outer.map(b => a * 10 + b))))), 21)

  test("1 000 operations, each crossing 500 handlers"):
    def loop(n: Int, acc: Int): Int ! Reader % Int =
      if n == 0 then pure(acc) else ask[Int].flatMap(x => loop(n - 1, acc + x))
    def deep[T <: Row](d: Int)(using In[Reader % Int, T]): Eff[T, Int] =
      given Place[T] = Place.at
      if d == 0 then loop(1000, 0) else writer[String, Int, T](deep[Writer % String +: T](d - 1)).map(_._2)
    assertEquals(run(reader(1)(deep[Reader % Int +: Pure](500))), 1000)

  test("a handler resuming twice under 10 000 binds: each path runs the whole rest"):
    def binds[R <: Row: Has[Choose]]: Eff[R, Int] = (1 to 10000).foldLeft(among(Seq(1, 2)).at[R])((p, _) => p.flatMap(x => pure(x + 1)))
    assertEquals(run(choose(binds)), Seq(10001, 10002))

  test("100 000 tells, each a state's step"):
    def tells(n: Int): Int ! Writer % String = if n == 0 then pure(0) else tell(n.toString).flatMap(_ => tells(n - 1))
    assertEquals(run(writer[String, Int, Pure](tells(100000)))._1.length, 100000)
