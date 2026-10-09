package okay.scala2

import cats.Monad
import cats.syntax.all._
import okay.core._
import okay.std._
import tofu.{Errors, WithLocal}
import OnProgram._

final case class Cfg(user: String, depth: Int)

/** code written over `F[_]` with tofu, in Scala 2, knowing nothing of okay */
object Service {
  def describe[F[_]: Monad](implicit ctx: WithLocal[F, Cfg]): F[String] =
    for {
      cfg <- ctx.context
      deeper <- ctx.local(ctx.context.map(c => s"${c.user}@${c.depth}"))(c => c.copy(depth = c.depth + 1))
    } yield s"${cfg.user}@${cfg.depth} $deeper"

  def check[F[_]: Monad](n: Int)(implicit err: Errors[F, String]): F[Int] =
    if (n < 0) err.raise[Int](s"negative $n") else Monad[F].pure(n * 2)

  def safely[F[_]: Monad](n: Int)(implicit err: Errors[F, String]): F[String] =
    err.handleWith(check[F](n).map(v => s"ok $v"))(e => Monad[F].pure(s"recovered: $e"))
}

/** okay-core from Scala 2 with tofu: the same service on Cont0 and on Cont */
class TestTofuFromScala2 extends munit.FunSuite {
  type App = +:[ReaderOf[Cfg]#L, +:[ThrowsOf[String]#L, Pure]]
  type F[A] = Program[App, A]

  def run[A](top: Top, cfg: Cfg)(p: F[A]): Either[String, A] =
    Program.run(top)(Programs.throws[String](Programs.reader(cfg)(p)))

  for ((name, top) <- List("Cont0" -> okay.zero.top, "Cont" -> okay.cont.top)) {
    test(s"Scala 2, tofu on $name: WithLocal — the context and local") {
      assertEquals(run(top, Cfg("ann", 1))(Service.describe[F]), Right("ann@1 ann@2"))
    }
    test(s"Scala 2, tofu on $name: Errors — raise, handleWith, restore") {
      assertEquals(run(top, Cfg("ann", 1))(Service.safely[F](3)), Right("ok 6"))
      assertEquals(run(top, Cfg("ann", 1))(Service.safely[F](-1)), Right("recovered: negative -1"))
      assertEquals(run(top, Cfg("ann", 1))(Service.check[F](-2)), Left("negative -2"))
      assertEquals(run(top, Cfg("ann", 1))(implicitly[Errors[F, String]].restore(Service.check[F](-2))), Right(None))
    }
    test(s"Scala 2 on $name: tailRecM and a left-nested chain of 100 000 binds in constant stack") {
      val m = Monad[F]
      assertEquals(run(top, Cfg("a", 0))(m.tailRecM(0)(i => m.pure(if (i < 100000) Left(i + 1) else Right(i)))), Right(100000))
      val p = (1 to 100000).foldLeft(m.pure(0))((acc, i) => acc.flatMap(a => m.pure(a + i)))
      assertEquals(run(top, Cfg("a", 0))(p), Right(5000050000L.toInt))
    }
  }

  test("Scala 2: a Program over a row of its own, the standard effects by Programs") {
    type R = +:[({ type L[X] = State[Int, X] })#L, +:[({ type L[X] = Writer[String, X] })#L, Pure]]
    val p: Program[R, Int] =
      Programs.get[R, Int].flatMap(s => Programs.put[R, Int](s + 1).flatMap(_ => Programs.tell[R, String](s"was $s").map(_ => s)))
    val run2 = Programs.writer[String](Programs.state(41)(p))
    assertEquals(Program.run(okay.zero.top)(run2), (List("was 41"), (42, 41)))
    assertEquals(Program.run(okay.cont.top)(run2), (List("was 41"), (42, 41)))
  }
}
