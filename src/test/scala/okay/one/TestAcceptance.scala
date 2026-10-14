package okay.one

import okay.one.Cont.{perform, pure, rows}
import okay.one.handler.*
import okay.one.row.*
import okay.one.std.*

/** okay-core's docs/one.md §4: the programs the kernel passes in okay-one first */
class TestAcceptance extends munit.FunSuite:
  type S = State % Int

  def counted[R <: Row](using In[Choose, R], In[S, R]): Int ! R = for
    x <- Choose.among[Int, R](List(1, 2, 3))
    s <- State.get[Int, R]
    _ <- State.put[Int, R](s + x)
    t <- State.get[Int, R]
  yield t

  /** a cell: one for the run, the paths share it */
  def cell[T <: Row, A](s0: Int) = new Answering[S, T, A, (Int, A)]:
    var s = s0
    def ret(a: A) = pure((s, a))
    def value[X](op: State[Int, X]): X = op match
      case _: State.Get[s] => s
      case p: State.Put[s] => s = p.s

  test("D. state outside the choice: threaded through the paths") {
    assertEquals(counted[Choose +: S +: Pure].handle(Choose.all).handle(State(0)).run, (6, List(1, 3, 6)))
  }

  test("D. state inside the choice: each path its own") {
    assertEquals(counted[S +: Choose +: Pure].handle(State(0)).handle(Choose.all).run, List((1, 1), (2, 2), (3, 3)))
  }

  test("D. a cell inside the choice: one for the run") {
    assertEquals(counted[S +: Choose +: Pure].handle(cell(0)).handle(Choose.all).run, List((1, 1), (3, 3), (6, 6)))
  }

  enum Two[X]:
    case Pick() extends Two[Int]

  test("A/D. the rests made first, resumed later and in reverse: each its state as it was when captured") {
    def twice[T <: Row] = new Deep[Two, T, Int, List[Int]]:
      def ret(a: Int) = pure(List(a))
      def apply[X](op: Two[X], k: X => List[Int] ! T) = op match
        case Two.Pick() =>
          val one = k(1)
          val two = k(2)
          two.flatMap(r2 => one.map(r1 => r1 ::: r2))
    def picked[R <: Row](using In[Two, R], In[S, R]): Int ! R = for
      s <- State.get[Int, R]
      x <- perform[Two, R, Int](Two.Pick())
      _ <- State.put[Int, R](s + x)
      t <- State.get[Int, R]
    yield t
    assertEquals(picked[S +: Two +: Pure].handle(State(10)).map(_._2).handle(twice).run, List(11, 12))
  }

  test("F. flatMap and filter over 100 000 empties, in constant stack") {
    type R = Choose +: Pure
    val p = Choose.among[Int, R](1 to 100000).flatMap(x => if x % 1000 == 0 then pure[R, Int](x) else Choose.fail[Int, R])
    assertEquals(p.handle(Choose.all).run.length, 100)
  }

  test("F. binds nested to the left, 100 000, in constant stack") {
    var p: Int ! Pure = pure(0)
    for _ <- 1 to 100000 do p = p.flatMap(x => pure(x + 1))
    assertEquals(p.run, 100000)
  }
