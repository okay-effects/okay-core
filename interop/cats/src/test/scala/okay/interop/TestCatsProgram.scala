package okay.interop

import cats.Monad
import cats.syntax.all.*
import okay.core.*
import okay.core.std.*
import CatsProgram.given

/** cats' monad over `Eff[R, *]`, on every platform: its laws on a sample, `tailRecM` and a left-nested chain in
 * constant stack, code over `F[_]` run on okay's effects */
class TestCatsProgram extends munit.FunSuite:
  type R = Reader % Int +: Pure
  type F[A] = Eff[R, A]
  def run[A](p: F[A]): A = okay.core.run(reader(5)(p))
  def addAsk[G[_]: Monad](ask: G[Int])(n: Int): G[Int] = ask.map(_ + n)

  test("cats: the monad's laws on a sample, code over F[_]"):
    val m = Monad[F]
    def f(i: Int): F[Int] = m.pure(i + 1)
    def g(i: Int): F[Int] = ask[Int].at[R].map(_ * i)
    assertEquals(run(m.pure(3).flatMap(f)), run(f(3)))
    assertEquals(run(g(3).flatMap(m.pure)), run(g(3)))
    assertEquals(run(g(3).flatMap(f).flatMap(g)), run(g(3).flatMap(x => f(x).flatMap(g))))
    assertEquals(run(addAsk[F](ask[Int].at[R])(10)), 15)
  test("cats: tailRecM and 100 000 left-nested binds in constant stack"):
    val m = Monad[F]
    assertEquals(run(m.tailRecM(0)(i => m.pure(if i < 100000 then Left(i + 1) else Right(i)))), 100000)
    assertEquals(run((1 to 100000).foldLeft(m.pure(0))((acc, _) => acc.map(_ + 1))), 100000)
