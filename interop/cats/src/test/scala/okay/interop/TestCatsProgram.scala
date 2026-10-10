package okay.interop

import cats.Monad
import cats.syntax.all.*
import okay.core.*
import okay.std.*
import CatsProgram.given

/** cats' monad over `Program`, on every platform: its laws on a sample, `tailRecM` and a left-nested chain in constant
 * stack, code over `F[_]` run on okay's effects on both monads */
class TestCatsProgram extends munit.FunSuite:
  type R = Reader % Int +: Pure
  type F[A] = Program[R, A]
  def run[A](top: Top)(p: F[A]): A = Program.run(top)(Programs.reader(5)(p))
  def addAsk[G[_]: Monad](ask: G[Int])(n: Int): G[Int] = ask.map(_ + n)

  for (name, top) <- List("Cont0" -> okay.zero.top, "Cont" -> okay.cont.top) do
    test(s"cats on $name: the monad's laws on a sample, code over F[_]"):
      val m = Monad[F]
      def f(i: Int): F[Int] = m.pure(i + 1)
      def g(i: Int): F[Int] = Programs.ask[R, Int].map(_ * i)
      assertEquals(run(top)(m.pure(3).flatMap(f)), run(top)(f(3)))
      assertEquals(run(top)(g(3).flatMap(m.pure)), run(top)(g(3)))
      assertEquals(run(top)(g(3).flatMap(f).flatMap(g)), run(top)(g(3).flatMap(x => f(x).flatMap(g))))
      assertEquals(run(top)(addAsk[F](Programs.ask[R, Int])(10)), 15)
    test(s"cats on $name: tailRecM and 100 000 left-nested binds in constant stack"):
      val m = Monad[F]
      assertEquals(run(top)(m.tailRecM(0)(i => m.pure(if i < 100000 then Left(i + 1) else Right(i)))), 100000)
      assertEquals(run(top)((1 to 100000).foldLeft(m.pure(0))((acc, _) => acc.map(_ + 1))), 100000)
