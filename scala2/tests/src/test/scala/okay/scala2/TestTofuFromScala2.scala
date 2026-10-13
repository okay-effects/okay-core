package okay.scala2

import cats.Monad
import cats.syntax.all._
import okay.one.Cont0
import okay.core.Ops._
import okay.one.row._
import okay.one.std.{Reader, State, Throws, Writer}
import tofu.{Errors, WithLocal}
import CatsProgram._
import TofuProgram._

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

/** okay-core from Scala 2 with tofu: the same service */
class TestTofuFromScala2 extends munit.FunSuite {
  type App = +:[ReaderOf[Cfg]#L, +:[ThrowsOf[String]#L, Pure]]
  type F[A] = Cont0[App, Nothing, Nothing, A]
  type T1 = +:[ThrowsOf[String]#L, Pure]

  def run[A](cfg: Cfg)(p: F[A]): Either[String, A] =
    okay.core.Ops.run(okay.core.Ops.handle(okay.core.Ops.handle(p)(Reader[Cfg, T1, A](cfg)))(Throws.either[String, Pure, A]))

  test("Scala 2, tofu: WithLocal — the context and local") {
    assertEquals(run(Cfg("ann", 1))(Service.describe[F]), Right("ann@1 ann@2"))
  }
  test("Scala 2, tofu: Errors — raise, handleWith, restore") {
    assertEquals(run(Cfg("ann", 1))(Service.safely[F](3)), Right("ok 6"))
    assertEquals(run(Cfg("ann", 1))(Service.safely[F](-1)), Right("recovered: negative -1"))
    assertEquals(run(Cfg("ann", 1))(Service.check[F](-2)), Left("negative -2"))
    assertEquals(run(Cfg("ann", 1))(implicitly[Errors[F, String]].restore(Service.check[F](-2))), Right(None))
  }
  test("Scala 2: tailRecM and a left-nested chain of 100 000 binds in constant stack") {
    val m = Monad[F]
    assertEquals(run(Cfg("a", 0))(m.tailRecM(0)(i => m.pure(if (i < 100000) Left(i + 1) else Right(i)))), Right(100000))
    val p = (1 to 100000).foldLeft(m.pure(0))((acc, i) => acc.flatMap[Nothing, Int](a => m.pure(a + i)))
    assertEquals(run(Cfg("a", 0))(p), Right(5000050000L.toInt))
  }

  test("Scala 2: a program over a row of its own, the kernel's standard effects and handlers") {
    type R = +:[StateOf[Int]#L, +:[WriterOf[String]#L, Pure]]
    type W = +:[WriterOf[String]#L, Pure]
    val p: Cont0[R, Nothing, Nothing, Int] =
      okay.core.Ops.perform[StateOf[Int]#L, R, Int](State.Get[Int]()).flatMap[Nothing, Int](s => okay.core.Ops.perform[StateOf[Int]#L, R, Unit](State.Put[Int](s + 1))
        .flatMap[Nothing, Int](_ => okay.core.Ops.perform[WriterOf[String]#L, R, Unit](Writer.Tell(s"was $s")).map(_ => s)))
    val run2 = okay.core.Ops.handle(okay.core.Ops.handle(p)(State[Int, W, Int](41)))(Writer.collect[String, Pure, (Int, Int)])
    assertEquals(okay.core.Ops.run(run2), (List("was 41"), (42, 41)))
  }
}
