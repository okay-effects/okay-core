package okay.interop

import cats.Monad
import cats.syntax.all.*
import okay.core.*
import okay.std.*
import tofu.{Errors, WithContext, WithLocal}
import CatsProgram.given
import TofuProgram.given

final case class Cfg(user: String, depth: Int)

/** CODE WRITTEN OVER `F[_]` WITH TOFU, knowing nothing of okay */
object Service:
  def describe[F[_]: Monad](using ctx: WithLocal[F, Cfg]): F[String] =
    for
      cfg <- ctx.context
      deeper <- ctx.local(ctx.context.map(c => s"${c.user}@${c.depth}"))(c => c.copy(depth = c.depth + 1))
      twice <- ctx.local(ctx.local(ctx.context.map(_.depth))(c => c.copy(depth = c.depth * 10)))(c => c.copy(depth = c.depth + 1))
    yield s"${cfg.user}@${cfg.depth} $deeper $twice"

  def check[F[_]: Monad](n: Int)(using err: Errors[F, String]): F[Int] =
    if n < 0 then err.raise(s"negative $n") else Monad[F].pure(n * 2)

  def safely[F[_]: Monad](n: Int)(using err: Errors[F, String]): F[String] =
    err.handleWith(check[F](n).map(v => s"ok $v"))(e => Monad[F].pure(s"recovered: $e"))

  def both[F[_]: Monad](using ctx: WithContext[F, Cfg], err: Errors[F, String]): F[Int] =
    ctx.context.flatMap(c => if c.user.isEmpty then err.raise("no user") else Monad[F].pure(c.depth))

/** tofu over okay, on a monad's top: `Program` built there and run */
abstract class TestTofuOn(top: Top, stackSafe: Boolean = true) extends munit.FunSuite:
  override def munitTestTransforms: List[TestTransform] = super.munitTestTransforms :+ TestTransform("stack", t =>
    if stackSafe then t else t.withBody(() => okay.core.BackendSuite.onLargeStack(t.body())))

  type App = Reader % Cfg +: Throws % String +: Pure
  type F[A] = Program[App, A]
  /** a program over `App` run: the reader inside, the throws outside */
  def run[A](cfg: Cfg)(p: F[A]): Either[String, A] =
    Program.run(top)(Programs.throws[String](Programs.reader(cfg)(p)))

  test("tofu: WithContext and WithLocal — the context, local, nested locals composed"):
    assertEquals(run(Cfg("ann", 1))(Service.describe[F]), Right("ann@1 ann@2 20"))

  test("tofu: Errors — raise, handleWith; an error handled is not seen outside, one not handled is"):
    assertEquals(run(Cfg("ann", 1))(Service.safely[F](3)), Right("ok 6"))
    assertEquals(run(Cfg("ann", 1))(Service.safely[F](-1)), Right("recovered: negative -1"))
    assertEquals(run(Cfg("ann", 1))(Service.check[F](-2)), Left("negative -2"))

  test("tofu: Errors — attempt, restore, recover, a raise inside the handler goes outside"):
    val err = summon[Errors[F, String]]
    assertEquals(run(Cfg("ann", 1))(err.attempt(Service.check[F](-1))), Right(Left("negative -1")))
    assertEquals(run(Cfg("ann", 1))(err.restore(Service.check[F](-1))), Right(None))
    assertEquals(run(Cfg("ann", 1))(err.restore(Service.check[F](4))), Right(Some(8)))
    assertEquals(run(Cfg("ann", 1))(err.recover(Service.check[F](-1)) { case e if e.startsWith("neg") => 0 }), Right(0))
    assertEquals(run(Cfg("ann", 1))(err.handleWith(Service.check[F](-1))(e => err.raise(s"again: $e"))), Left("again: negative -1"))

  test("tofu: context and errors together; the context inside a handled error, an error inside a local"):
    assertEquals(run(Cfg("", 1))(Service.both[F]), Left("no user"))
    assertEquals(run(Cfg("bob", 7))(Service.both[F]), Right(7))
    val ctx = summon[WithLocal[F, Cfg]]
    val err = summon[Errors[F, String]]
    assertEquals(run(Cfg("bob", 7))(err.handleWith(ctx.local(Service.both[F])(_.copy(user = "")))(e => ctx.context.map(c => s"$e for ${c.user}".length))),
      Right("no user for bob".length))

  test("tofu: the monad — its laws on a sample, tailRecM in constant stack"):
    val m = Monad[F]
    def f(i: Int): F[Int] = m.pure(i + 1)
    def g(i: Int): F[Int] = m.pure(i * 2)
    assertEquals(run(Cfg("a", 0))(m.pure(3).flatMap(f)), run(Cfg("a", 0))(f(3)))
    assertEquals(run(Cfg("a", 0))(f(3).flatMap(m.pure)), run(Cfg("a", 0))(f(3)))
    assertEquals(run(Cfg("a", 0))(f(3).flatMap(f).flatMap(g)), run(Cfg("a", 0))(f(3).flatMap(x => f(x).flatMap(g))))
    assertEquals(run(Cfg("a", 0))(m.tailRecM(0)(i => m.pure(if i < 100000 then Left(i + 1) else Right(i)))), Right(100000))

  test("tofu: a left-nested chain of 100 000 binds built and run in constant stack"):
    val m = Monad[F]
    val p = (1 to 100000).foldLeft(m.pure(0))((acc, i) => acc.flatMap(a => m.pure(a + i)))
    assertEquals(run(Cfg("a", 0))(p), Right(5000050000L.toInt))

class TestTofu extends TestTofuOn(okay.cont.top)
class TestTofuOnCont0 extends TestTofuOn(okay.zero.top)
class TestTofuOnRef extends TestTofuOn(okay.zero.Ref.top)
/** the textbook CPS, not stack safe: on a thread of a large stack */
class TestTofuOnCps0 extends TestTofuOn(okay.core.Cps0.top, stackSafe = false)
