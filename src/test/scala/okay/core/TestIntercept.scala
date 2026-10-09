package okay.core

import okay.std.*

/** `intercept`: an effect already in the row handled again for a body — on any monad of the interface */
abstract class TestInterceptOn(on: Backend) extends BackendSuite(on):

  /** `local(f)(body)`: every `ask` of the body answered `f` of the reader's value outside */
  def local[A](using c: Effects, in: Reader % Int :? c.Effs)(f: Int => Int)(body: c.Reset[A] ?=> c.Inner[c.Effs, A, A, A]): c.Prog[A] =
    Effects.intercept(new Handler[c.Prog, Reader % Int, A, A]:
      def ret(a: A): A = a
      def apply[X](op: Reader[Int, X], k: X => c.Prog[A]): c.Prog[A] = op match
        case Reader.Ask() => ask[Int].flatMap(v => k(f(v))))(body)

  /** `recover(f)(body)`: a `raise` of the body answered `f` of the error, the rest of the body dropped */
  def recover[A](using c: Effects, in: Throws % String :? c.Effs)(f: String => A)(body: c.Reset[A] ?=> c.Inner[c.Effs, A, A, A]): c.Prog[A] =
    Effects.intercept(new Handler[c.Prog, Throws % String, A, A]:
      def ret(a: A): A = a
      def apply[X](op: Throws[String, X], k: X => c.Prog[A]): c.Prog[A] = op match
        case Throws.Raise(e) => c.pure(f(e)))(body)

  test("intercept: a reader's asks in the body answered anew — the reader answering in place outside is not reached"):
    assertEquals(run(reader(1)(ask[Int].flatMap(a => local[(Int, Int)](_ * 10)(ask[Int].flatMap(b => ask[Int].map(c => (b, c)))).map(bc => (a, bc))))),
      (1, (10, 10)))

  test("intercept: a clause's own operation of the effect goes outside the interceptor — nested ones compose"):
    // the inner clause's ask is the outer interceptor's: (2 + 1) * 10, as tofu's nested `local`s
    assertEquals(run(reader(2)(local[Int](_ + 1)(local[Int](_ * 10)(ask[Int])))), 30)
    assertEquals(run(reader(2)(local[Int](_ * 10)(ask[Int]).flatMap(a => ask[Int].map(b => (a, b))))), (20, 2))

  test("intercept: an operation of another effect goes on outside as it is"):
    assertEquals(run(writer[String, Int](reader(3)(local[Int](_ + 4)(tell("in").flatMap(_ => ask[Int]))))), (List("in"), 7))

  test("intercept: an operation from a handler nested in the body, at the interceptor's path, intercepted"):
    assertEquals(run(reader(5)(local[(List[String], Int)](_ * 2)(writer[String, Int](tell("x").flatMap(_ => ask[Int]))))), (List("x"), 10))

  test("intercept: a raise recovered, the rest of the body dropped; no raise, the value; outside the throws untouched"):
    assertEquals(run(throws[String, Int](recover[Int](_.length)(raise[String, Int]("four").map(_ + 100)))), Right(4))
    assertEquals(run(throws[String, Int](recover[Int](_.length)(c ?=> c.pure(7)))), Right(7))
    assertEquals(run(throws[String, Int](recover[Int](_.length)(c ?=> c.pure(7)).flatMap(_ => raise[String, Int]("out")))), Left("out"))

  test("intercept: under a state, the state's cell one — the interceptor answers only its own effect"):
    assertEquals(run(reader(1)(state[(Int, Int)](0)(local[(Int, Int)](_ + 1)(put(5).flatMap(_ => get[Int]).flatMap(s => ask[Int].map(a => (s, a))))))), (5, (5, 2)))

  test("intercept: a shift in the body reaches the interceptor, as a reset"):
    assertEquals(run(reader(1)(local[Int](_ + 1)(okay.core.Control.shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1)))), 12)

  test("intercept: 100 000 intercepted operations in constant stack"):
    def asks(i: Int, acc: Int)(using c: Effects, in: Reader % Int :? c.Effs): c.Prog[Int] =
      if i == 0 then c.pure(acc) else ask[Int].flatMap(v => asks(i - 1, acc + v))
    assertEquals(run(reader(1)(local[Int](_ + 1)(asks(100000, 0)))), 200000)

class TestIntercept extends TestInterceptOn(Backend.cont)
class TestInterceptOnCps0 extends TestInterceptOn(Backend.cps0)
