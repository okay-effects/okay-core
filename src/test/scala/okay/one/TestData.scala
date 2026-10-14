package okay.one

import okay.one.Cont.{perform, pure, rows}
import okay.one.Handlers.*
import okay.one.handler.*
import okay.one.row.*

enum Emit[X]:
  case Value(v: Int) extends Emit[Unit]

/** a generator: its next value and the rest — `k`, kept in the answer — or done */
enum Gen[T <: Row]:
  case Done()
  case More(v: Int, next: Unit => Gen[T] ! T)

/** `k` AS DATA: a clause's `k` a program over the row outside, kept in the handler's answer, resumed after the handler
 * has answered — anywhere, any number of times */
class TestData extends munit.FunSuite:
  /** each value given back with the rest of the program: `k` not resumed, kept */
  def generate[T <: Row] = new Deep[Emit, T, Unit, Gen[T]]:
    def ret(a: Unit) = pure(Gen.Done())
    def apply[X](op: Emit[X], k: X => Gen[T] ! T) = op match
      case Emit.Value(v) => pure(Gen.More(v, k))

  def emits[R <: Row](vs: Seq[Int])(using In[Emit, R]): Unit ! R =
    vs.foldRight(pure[R, Unit](()))((v, rest) => perform[Emit, R, Unit](Emit.Value(v)).flatMap(_ => rest))

  /** two generators zipped: each resumed by turns, outside its handler */
  def zip[T <: Row](a: Gen[T] ! T, b: Gen[T] ! T): List[(Int, Int)] ! T =
    a.flatMap(ga => b.flatMap(gb => (ga, gb) match
      case (Gen.More(x, nx), Gen.More(y, ny)) => zip(nx(()), ny(())).map((x, y) :: _)
      case _ => pure(Nil)))

  test("generators zipped: each k kept in a value, resumed by the other's turn") {
    val p = zip[Pure](emits[Emit +: Pure](1 to 3).handle(generate), emits[Emit +: Pure](10 to 50 by 10).handle(generate))
    assertEquals(p.run, List((1, 10), (2, 20), (3, 30)))
  }

  test("a kept k resumed under the handlers of the row outside: a state they share") {
    type T = St +: Pure
    def counted(vs: Seq[Int]): Unit ! Emit +: T =
      vs.foldRight(pure[Emit +: T, Unit](()))((v, rest) =>
        rows[Emit +: T].perform(St.Get).flatMap(s => rows[Emit +: T].perform(St.Put(s + 1)))
          .flatMap(_ => rows[Emit +: T].perform(Emit.Value(v))).flatMap(_ => rest))
    val p = zip[T](counted(1 to 2).handle(generate), counted(5 to 9).handle(generate))
    assertEquals(p.handle(state(0)).run, (5, List((1, 5), (2, 6))))
  }

  test("a kept k resumed twice") {
    val g = emits[Emit +: Pure](Seq(1, 2)).handle(generate[Pure]).run
    g match
      case Gen.More(1, next) =>
        val twice = for a <- next(()); b <- next(()) yield (a, b)
        twice.run match
          case (Gen.More(2, _), Gen.More(2, _)) => ()
          case other => fail(s"not resumed twice: $other")
      case other => fail(s"not a first value: $other")
  }

  test("stack safe: a generator of many values, drained") {
    def drain(g: Gen[Pure] ! Pure, acc: Long): Long ! Pure = g.flatMap:
      case Gen.More(v, next) => drain(next(()), acc + v)
      case Gen.Done() => pure(acc)
    def many(i: Int): Unit ! Emit +: Pure =
      if i == 0 then pure(()) else rows[Emit +: Pure].perform(Emit.Value(1)).flatMap(_ => many(i - 1))
    assertEquals(drain(many(100000).handle(generate), 0).run, 100000L)
  }
