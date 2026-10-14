package okay.one

import okay.one.Cont.*
import okay.one.Handlers.*
import okay.one.handler.*
import okay.one.row.*

/** CONTROL IN THE ROW: a `$` an effect of the row, `Control[T] +: T`; a shift0 to the nearest; the operations of the
 * row outside going out past the `$` to their handlers; handlers inside the `$`, in the `k` of a shift0 past them */
class TestControl extends munit.FunSuite:
  /** a program under a `$` over `R`, its answers `L` unmoved */
  type C[R <: Row, L, A] = Cont[Delim[R], L, L, A]
  /** a shift0 under the `$` over `R`, its rest a program of effects over `R` */
  def sh[R <: Row, X, O](f: (X => Int ! R) => O): Cont[Delim[R], Int ! R, O, X] =
    Shift0[Delim[R], R, Nothing, Nothing, Int, O, X](Nearest.Here(), f)

  def ask[R <: Row, L](using In[Ask, R]): C[R, L, Int] = perform[Ask, Delim[R], Int](Ask.Get).cont[L]
  def flip[R <: Row, L](using In[Choose, R]): C[R, L, Boolean] = perform[Choose, Delim[R], Boolean](Choose.Flip).cont[L]

  test("an operation under a $ and a shift0 resumed twice: out of the $ to its handler") {
    type R = Ask +: Pure
    // `$ (ask + shift0(k => k(1) + k(2)))`, `ask` 10
    val body = sh[R, Int, Int ! R](k => Bind(k(1), (a: Int) => Map(k(2), (b: Int) => a + b)))
    val p: Int ! R = Dollar[R, Int ! R, Nothing, Nothing, Int, Int](
      Bind(ask[R, Int ! R], (n: Int) => Map(body, (x: Int) => n + x)), (a: Int) => Return(a))
    assertEquals(p.handle(reader(10)).run, 23)
  }

  test("a Deep handler's k taken past two $s, resumed twice: each time the $s put back, their shift0s working") {
    type R = Choose +: Pure
    type L = Int ! R
    type M = C[R, L, Int]
    // `$ ($ (flip >>= b => shift0(k => k(b ? 1 : 2) + k(10))) * 100)`: the inner `$` under the outer, a row deeper
    val inner: Cont[Delim[Delim[R]], M, M, Int] = Bind(flip[Delim[R], M], (b: Boolean) =>
      Shift0[Delim[Delim[R]], Delim[R], L, L, Int, M, Int](Nearest.Here(),
        k => Bind(k(if b then 1 else 2), (a: Int) => Map(k(10), (c: Int) => a + c))))
    val outer: M = Dollar[Delim[R], M, L, L, Int, Int](inner, (a: Int) => Return(a * 100))
    val p: Int ! R = Dollar[R, L, Nothing, Nothing, Int, Int](outer, (a: Int) => Return(a))
    assertEquals(p.handle(choose).run, List(1100, 1200))
  }

  test("printf under choose: the answer moved to a function, one a path; each a program over the row, run again") {
    type R = Choose +: Pure
    type K = String ! R
    type S = Int => K
    // `$ (flip >>= b => "got " + b + shift0(k => pure(x => k(x))))`: the answer `String` turned `Int => K`
    val body: Cont[Delim[R], K, S ! R, Int] =
      Shift0[Delim[R], R, Nothing, Nothing, String, S ! R, Int](Nearest.Here(), k => Return((x: Int) => k(x)))
    val p: S ! R = Dollar[R, K, Nothing, Nothing, S, String](
      Bind(flip[R, S ! R], (b: Boolean) => Map(body, (n: Int) => s"$b $n")),
      (s: String) => Return[R, Nothing, String]("got " + s))
    val fs = p.handle(choose).run
    assertEquals(fs.map(f => f(42).handle(choose).run), List(List("got true 42"), List("got false 42")))
  }

  test("a Deep handler's program under the $, its operations of others out of it") {
    type R = Ask +: Pure
    val p: Int ! Choose +: Delim[R] = for b <- perform[Choose, Choose +: Delim[R], Boolean](Choose.Flip)
      n <- perform[Ask, Choose +: Delim[R], Int](Ask.Get) yield if b then n else -n
    assertEquals(reset(p.handle(choose).cont[List[Int]]).handle(reader(3)).run, List(3, -3))
  }

  test("stack safe: a loop of operations under a $, out of it") {
    type R = Ask +: Pure
    def loop(i: Int, acc: Int): C[R, Int, Int] =
      if i == 0 then Return(acc) else Bind(ask[R, Int], (n: Int) => loop(i - 1, acc + n))
    assertEquals(reset(loop(100000, 0)).handle(reader(1)).run, 100000)
  }

  test("stack safe: 100 000 $s in a program of effects, each a shift0 resumed in it") {
    type R = Ask +: Pure
    def one: Int ! R = Dollar[R, Int ! R, Nothing, Nothing, Int, Int](sh[R, Int, Int ! R](k => Map(k(1), (a: Int) => a + 1)),
      (a: Int) => Return(a))
    def many(i: Int, acc: Int): Int ! R = if i == 0 then pure(acc) else one.flatMap(x => many(i - 1, acc + x))
    assertEquals(many(100000, 0).handle(reader(0)).run, 200000)
  }

  // HANDLERS INSIDE THE $

  /** a cell counting its reads: a state, each `Get` the count so far */
  def counter[T <: Row, A](s0: Int) = new Stateful[St, T, Int, A, A]:
    def initial = s0
    def ret(s: Int, a: A) = Cont.pure(a)
    def value[X](s: Int, op: St[X]): X = op match
      case St.Get => s
      case St.Put(_) => ()
    def next[X](s: Int, op: St[X]) = op match
      case St.Get => s + 1
      case St.Put(s1) => s1

  test("printf with a state inside the $: taken into k, each resumption from the state as it was") {
    type S = Int => String
    type Q = St +: Delim[Pure]
    // `$ (handle_counter(get ; x <- shift0(k => pure(x => k(x))) ; get ; s"$n $x $m"))`
    val body: Cont[Q, String ! Pure, S ! Pure, String] =
      perform[St, Q, Int](St.Get).cont[S ! Pure].flatMap(n =>
        Shift0[Q, Pure, Nothing, Nothing, String, S ! Pure, Int](summon, k => Return((x: Int) => k(x).run)).flatMap(x =>
          perform[St, Q, Int](St.Get).cont[String ! Pure].map(m => s"$n $x $m")))
    val f: S = dollar(body.handle(counter(0)))((s: String) => Return("<" + s + ">")).run
    assertEquals(f(42), "<0 42 1>")
    assertEquals(f(7), "<0 7 1>")
  }

  test("a for: operations of the row and a shift0 moving the answer, the operations at the answers of the rest") {
    type T = Ask +: Pure
    type Q = Delim[T]
    type S = Int => String
    val body: Cont[Q, String ! T, S ! T, String] = for
      n <- perform[Ask, Q, Int](Ask.Get)                                 // lifted to the answers of the rest, S ! T
      x <- rows[Q].shift0[String, Int](k => pure((x: Int) => k(x).handle(reader(n)).run))
      m <- perform[Ask, Q, Int](Ask.Get)                                 // lifted to the shift0's rest's, String ! T
    yield s"$n $x $m"
    val f: S = dollar(body)((s: String) => pure("<" + s + ">")).handle(reader(5)).run
    assertEquals(f(42), "<5 42 5>")
  }

  test("a program of effects under a $: its row with the $ in it, no answer of its own; beside a shift0, lifted") {
    type R = Ask +: Delim[Pure]
    def p[R <: Row](using In[Ask, R]): Int ! R = for n <- perform[Ask, R, Int](Ask.Get) yield n + 1
    val under: Int ! R = p[R]
    val body: Cont[R, Int ! Pure, Int ! Pure, Int] = for
      a <- under
      x <- rows[R].shift0[Int, Int](k => k(a).flatMap(b => k(b)))
    yield a + x
    assertEquals(dollar(body.handle(reader(41)))((a: Int) => pure(a)).run, 126)
    assertEquals(reset(under.handle(reader(41)).cont[Int]).run, 42)
  }

  test("an Answering handler inside the $, under a shift0 resumed twice: in k both times") {
    type R = Ask +: Delim[Pure]
    val body: Cont[R, Int ! Pure, Int ! Pure, Int] =
      Shift0[R, Pure, Nothing, Nothing, Int, Int ! Pure, Int](summon, k => Bind(k(1), (a: Int) => Map(k(2), (b: Int) => a + b)))
        .flatMap(x => perform[Ask, R, Int](Ask.Get).cont[Int ! Pure].map(_ + x))
    assertEquals(dollar(body.handle(reader(10)))((a: Int) => Return(a)).run, 23)
  }

  test("a Translating handler inside the $: each operation a program over the row outside, out of the $") {
    type R = Log +: Delim[Ask +: Pure]
    val tell = new Translate[Log, Delim[Ask +: Pure], Int]:
      def apply[X](op: Log[X]) = op match
        case Log.Tell(s) => perform[Ask, Delim[Ask +: Pure], Int](Ask.Get).map(n => if n > 0 then () else ())
    val body: Cont[R, Int ! (Ask +: Pure), Int ! (Ask +: Pure), Int] =
      perform[Log, R, Unit](Log.Tell("a")).cont[Int ! (Ask +: Pure)].flatMap(_ =>
        Shift0[R, Ask +: Pure, Nothing, Nothing, Int, Int ! (Ask +: Pure), Int](summon, k => k(1)).map(_ + 1))
    assertEquals(dollar(body.handle(tell))((a: Int) => Return(a)).handle(reader(5)).run, 2)
  }

  test("a Linear handler inside the $: k once, at any answers — before it and after it a program outside") {
    type S = Int => String
    type Q = Log +: Delim[Pure]
    var log = List.empty[String]
    val once = new Linear[Log, Delim[Pure], String, String]:
      def ret(a: String) = pure(a)
      def apply[I, O, X](op: Log[X], k: X => Cont[Delim[Pure], I, O, String]) = op match
        case Log.Tell(s) =>
          log = s :: log
          k(()).map(r => { log = ("after " + s) :: log; r })
    val body: Cont[Q, String ! Pure, S ! Pure, String] =
      perform[Log, Q, Unit](Log.Tell("x")).cont[S ! Pure].flatMap(_ =>
        Shift0[Q, Pure, Nothing, Nothing, String, S ! Pure, Int](summon, k => Return((x: Int) => k(x).run)).map(_.toString))
    val f: S = dollar(body.handle(once))((s: String) => Return("<" + s + ">")).run
    assertEquals(log, List("x"))
    assertEquals(f(3), "<3>")
    assertEquals(log, List("after x", "x"))
  }

  test("a handler of an effect outside the $ put inside it: from inside the $ the row is seen with it first") {
    type R = Ask +: Delim[Log +: Pure]
    val p: Int ! R = perform[Ask, R, Int](Ask.Get).flatMap(n => perform[Log, R, Unit](Log.Tell(s"n $n")).map(_ => n))
    val q: (List[String], Int) ! Pure = reset(p.handle(reader(4)).cont[Int]).handle(log)
    assertEquals(q.run, (List("n 4"), 4))
  }
