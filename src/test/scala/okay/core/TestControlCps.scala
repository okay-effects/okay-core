package okay.core

import okay.cont.{Cont, Free, At}

import Control.*
import okay.std.*

/** A REFERENCE INSTANCE of `Control`: functions `(A => I) => O`, the textbook CPS of shift and reset
 * (Danvy–Filinski) — no tree, no loop, not stack safe. Programs written against `Control` run on it and on `Cont`,
 * and must agree */
object Cps:
  /** `(A => I) => O`, as a class */
  final class F[R <: Row, I, O, A](val run: (A => I) => O)

  final class At[R0 <: Row, S0] extends Control:
    type M[I, O, A] = F[R0, I, O, A]
    type Effs = R0
    type S = S0
    type Within[I, O, A] = F[R0, I, O, A]
    type Reset[S2] = At[R0, S2]
    type Answer[S2] = At[R0, S2]
    def answer[S2]: At[R0, S2] = At()
    def pure[S2, A](a: A): F[R0, S2, S2, A] = F(k => k(a))
    def delay[I, O, A](m: => F[R0, I, O, A]): F[R0, I, O, A] = F(k => m.run(k))
    extension [I, O, A](m: F[R0, I, O, A])
      def flatMap[I2, B](f: A => F[R0, I2, I, B]): F[R0, I2, O, B] = F(k => m.run(a => f(a).run(k)))
      def map[B](f: A => B): F[R0, I, O, B] = F(k => m.run(a => k(f(a))))
    /** `shift f = λk. f (λa. λk'. k' (k a)) id` */
    def shift[I, O, A](f: (A => F[R0, O, O, I]) => F[R0, O, O, O], same: (I =:= O) | Null): F[R0, I, O, A] =
      F(k => f(a => F(k2 => k2(k(a)))).run(identity))
    /** `reset e = λk. k (e ret)` */
    def reset[S2, O](body: At[R0, S2] ?=> F[R0, S2, O, S2]): F[R0, S0, S0, O] = F(k => k(body(using At()).run(identity)))

  /** the body at the top: its answer its value */
  def run[A](body: At[Pure, A] ?=> F[Pure, A, A, A]): A = body(using At()).run(identity)

/** the same programs, written against `Control`, on `Cont` and on the CPS functions */
class TestControlCps extends munit.FunSuite:

  def both[A](onCont: A, onCps: A, expected: A)(using munit.Location): Unit =
    assertEquals(onCont, expected, "on Cont")
    assertEquals(onCps, expected, "on CPS")

  def twice(using c: Control): c.Prog[Int] =
    reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1))
  test("reset(1 + shift(k => k(k(10)))) is 12"):
    both(okay.cont.run(twice), Cps.run(twice), 12)

  def modified(using c: Control): c.Prog[String] =
    reset[Int](shift[Int, String](k => k(5).map(_.toString)).map(_ * 2))
  test("the answer type modified: an Int hole, a String answer"):
    both(okay.cont.run(modified), Cps.run(modified), "10")

  def atTop(using c: Control { type S = Int }): c.M[Int, Int, Int] =
    shift[Int, Int](k => k(1).flatMap(k)).map(_ * 3)
  test("a shift at the top: the top is the delimiter"):
    both(okay.cont.run(atTop), Cps.run(atTop), 9)

  def shots(using c: Control): c.Prog[List[Int]] =
    reset[List[Int]](shift[Int, List[Int]](k => k(1).flatMap(a => k(2).map(b => a ++ b))).map(x => List(x, x * 10)))
  test("multi-shot: k resumed twice, the answers joined"):
    both(okay.cont.run(shots), Cps.run(shots), List(1, 10, 2, 20))

  def nested(using c: Control): c.Prog[Int] =
    reset[Int](shift[Int, Int](k => k(1).map(_ * 100)).flatMap(a => reset[Int](shift[Int, Int](k => k(2).map(_ + 7)).map(_ + a))))
  test("a shift captures up to the nearest reset only"):
    both(okay.cont.run(nested), Cps.run(nested), 1000)

  def stateful(using c: Control): c.Prog[(String, Int)] =
    pstate(7)(st => st.get[Int].flatMap(n => st.put[Int]("x" * n)).flatMap(_ => st.get[String]).map(_.length))
  test("pstate, library code over Control: the state's type changed on both"):
    both(okay.cont.run(stateful), Cps.run(stateful), ("xxxxxxx", 7))
