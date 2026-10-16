package okay.one

import okay.one.Cont.*
import okay.one.Handlers.*
import okay.one.handler.*
import okay.one.row.*

/** a run whose top is ONE stateful handler (small-runs): its own loop, the machine entered where the program needs it
 * — the state, the frames, the body as they stand; the values those of the machine's */
class TestStatefulRun extends munit.FunSuite:
  type S = St +: Pure
  def get: Int ! S = perform[St, S, Int](St.Get)
  def put(n: Int): Unit ! S = perform[St, S, Unit](St.Put(n))

  test("the loop alone: binds nested to the left and to the right, 100 000 operations each, constant stack") {
    var left: Int ! S = pure(0)
    for _ <- 1 to 100000 do left = left.flatMap(_ => get.flatMap(s => put(s + 1)).map(_ => 0))
    assertEquals(left.handle(state(0)).run, (100000, 0))
    def right(i: Int): Int ! S = if i == 0 then get else get.flatMap(s => put(s + 1)).flatMap(_ => right(i - 1))
    assertEquals(right(100000).handle(state(5)).run, (100005, 100005))
  }

  test("a handler inside: the machine entered there, the state as it was, the rest of the run on the machine") {
    type SL = Log +: St +: Pure
    val p: Int ! S = get.flatMap(s => put(s + 10)).flatMap { _ =>
      perform[Log, SL, Unit](Log.Tell("in")).flatMap(_ => perform[St, SL, Int](St.Get)).handle(log)
        .flatMap((l, s) => put(s * 2).map(_ => l.length))
    }
    assertEquals(p.handle(state(1)).run, (22, 1))
  }

  test("a throw in a user function: out of the run, as on the machine") {
    val p: Int ! S = get.map(s => if s > 0 then throw IllegalStateException(s"too big: $s") else s)
    intercept[IllegalStateException](p.handle(state(1)).run)
    assertEquals(p.handle(state(0)).run, (0, 0))
  }

  test("a handler's ret a program: run on the machine after the loop") {
    def counter[T <: Row, A](s0: Int) = new Stateful[St, T, Int, A, Int]:
      def initial = s0
      def ret(s: Int, a: A) = Cont.defer(Cont.pure(s * 100))
      def value[X](s: Int, op: St[X]): X = op match
        case St.Get => s
        case St.Put(_) => ()
      def next[X](s: Int, op: St[X]) = op match
        case St.Get => s
        case St.Put(n) => n
    assertEquals(get.flatMap(s => put(s + 1)).handle(counter(4)).run, 500)
  }

  test("a choice inside the state's program: every path, the state threaded — the machine's answer") {
    type CS = Choose +: St +: Pure
    val p: Int ! CS = perform[Choose, CS, Boolean](Choose.Flip).flatMap(b =>
      perform[St, CS, Int](St.Get).flatMap(s => perform[St, CS, Unit](St.Put(s + (if b then 1 else 10))).map(_ => s)))
    assertEquals(p.handle(choose).handle(state(0)).run, (11, List(0, 1)))
  }
