package okay.core

import Control.*
import okay.std.*

/** A REFERENCE INSTANCE of `Control`: functions `(A => I) => O`, the textbook CPS of shift and reset
 * (Danvy–Filinski) — no tree, no loop, not stack safe. Programs written against `Control` run on it and on `Cont`,
 * and must agree */
object Cps:
  type F[R <: Row, I, O, A] = (A => I) => O

  final class At[R0 <: Row, S0] extends Control:
    type M[R <: Row, I, O, A] = F[R, I, O, A]
    type R = R0
    type S = S0
    type Under[R2 <: Row, S2] = At[R2, S2]
    def under[R2 <: Row, S2]: At[R2, S2] = At()
    def pure[X <: Row, S2, A](a: A): F[X, S2, S2, A] = k => k(a)
    def delay[X <: Row, I, O, A](m: => F[X, I, O, A]): F[X, I, O, A] = k => m(k)
    extension [X <: Row, I, O, A](m: F[X, I, O, A])
      def flatMap[I2, B](f: A => F[X, I2, I, B]): F[X, I2, O, B] = k => m(a => f(a)(k))
      def map[B](f: A => B): F[X, I, O, B] = k => m(a => k(f(a)))
    /** `shift f = λk. f (λa. λk'. k' (k a)) id` */
    def shift[X <: Row, I, O, A](f: (A => F[X, O, O, I]) => F[X, O, O, O]): F[X, I, O, A] =
      k => f(a => k2 => k2(k(a)))(identity)
    /** `reset e = λk. k (e ret)` */
    def reset[X <: Row, Q, S2, O, A](body: F[X, S2, O, A], ret: A => S2): F[X, Q, Q, O] = k => k(body(ret))

  /** the body at the top: its answer its value */
  def run[A](body: At[Pure, A] ?=> F[Pure, A, A, A]): A = body(using At())(identity)

/** the same programs, written against `Control`, on `Cont` and on the CPS functions */
class TestControlCps extends munit.FunSuite:

  def both[A](onCont: A, onCps: A, expected: A)(using munit.Location): Unit =
    assertEquals(onCont, expected, "on Cont")
    assertEquals(onCps, expected, "on CPS")

  def twice(using c: Control): c.M[c.R, c.S, c.S, Int] =
    reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1))
  test("reset(1 + shift(k => k(k(10)))) is 12"):
    both(Effects.run(twice), Cps.run(twice), 12)

  def modified(using c: Control): c.M[c.R, c.S, c.S, String] =
    reset[Int](shift[Int, String](k => k(5).map(_.toString)).map(_ * 2))
  test("the answer type modified: an Int hole, a String answer"):
    both(Effects.run(modified), Cps.run(modified), "10")

  def atTop(using c: Control { type S = Int }): c.M[c.R, Int, Int, Int] =
    shift[Int, Int](k => k(1).flatMap(k)).map(_ * 3)
  test("a shift at the top: the top is the delimiter"):
    both(Effects.run(atTop), Cps.run(atTop), 9)

  def shots(using c: Control): c.M[c.R, c.S, c.S, List[Int]] =
    reset[List[Int]](shift[Int, List[Int]](k => k(1).flatMap(a => k(2).map(b => a ++ b))).map(x => List(x, x * 10)))
  test("multi-shot: k resumed twice, the answers joined"):
    both(Effects.run(shots), Cps.run(shots), List(1, 10, 2, 20))

  def nested(using c: Control): c.M[c.R, c.S, c.S, Int] =
    reset[Int](shift[Int, Int](k => k(1).map(_ * 100)).flatMap(a => reset[Int](shift[Int, Int](k => k(2).map(_ + 7)).map(_ + a))))
  test("a shift captures up to the nearest reset only"):
    both(Effects.run(nested), Cps.run(nested), 1000)

  def stateful(using c: Control): c.M[c.R, c.S, c.S, (String, Int)] =
    pstate(7)(st => st.get[Int].flatMap(n => st.put[Int]("x" * n)).flatMap(_ => st.get[String]).map(_.length))
  test("pstate, library code over Control: the state's type changed on both"):
    both(Effects.run(stateful), Cps.run(stateful), ("xxxxxxx", 7))
