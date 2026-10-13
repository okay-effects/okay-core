package okay.one

import okay.one.Cont0.{perform, pure}
import okay.one.Handlers.*
import okay.one.row.*

class TestCont extends munit.FunSuite:
  def asked[R <: Row](using In[Ask, R], In[Log, R]): Int ! R =
    for
      n <- perform[Ask, R, Int](Ask.Get)
      _ <- perform[Log, R, Unit](Log.Tell(s"got $n"))
    yield n + 1

  test("Answering, Stateful: the handlers registered in order, each operation answered in place") {
    assertEquals(asked[Ask +: Log +: Pure].handle(reader(41)).handle(log).run, (List("got 41"), 42))
  }

  test("a handler of an effect from inside the row: Log handled first, past Ask") {
    assertEquals(asked[Ask +: Log +: Pure].handle(log).handle(reader(41)).run, (List("got 41"), 42))
  }

  test("Aborting: the rest dropped") {
    def p[R <: Row](using In[Throws, R], In[Log, R]): Int ! R =
      for
        _ <- perform[Log, R, Unit](Log.Tell("before"))
        _ <- perform[Throws, R, Nothing](Throws.Raise("no"))
        _ <- perform[Log, R, Unit](Log.Tell("after"))
      yield 1
    assertEquals(p[Throws +: Log +: Pure].handle(attempt).handle(log).run, (List("before"), Left("no")))
    assertEquals(p[Throws +: Log +: Pure].handle(log).handle(attempt).run, Left("no"))
  }

  def flips[R <: Row](using In[Choose, R], In[St, R]): Int ! R =
    for
      _ <- perform[Choose, R, Boolean](Choose.Flip)
      s <- perform[St, R, Int](St.Get)
      _ <- perform[St, R, Unit](St.Put(s + 1))
    yield s

  test("Deep, k twice: a state inside, each path its own — taken with the k, put back each time") {
    assertEquals(flips[Choose +: St +: Pure].handle(state(0)).handle(choose).run, List((1, 0), (1, 0)))
  }

  test("Deep, k twice: a state outside, one for both paths") {
    assertEquals(flips[Choose +: St +: Pure].handle(choose).handle(state(0)).run, (2, List(0, 1)))
  }

  test("Deep: k dropped") {
    def p[R <: Row](using In[Choose, R]): Int ! R =
      for
        b <- perform[Choose, R, Boolean](Choose.Flip)
        n <- if b then pure[R, Int](1) else perform[Choose, R, Nothing](Choose.Fail)
      yield n
    assertEquals(p[Choose +: Pure].handle(choose).run, List(1))
  }

  test("an effect in a clause: the handler of Ask tells") {
    assertEquals(asked[Ask +: Log +: Pure].handle(askLogged[Log +: Pure, Int](41)).handle(log).run, (List("asked 41", "got 41"), 42))
  }

  test("a program of effects in a clause: Ask answered from the state outside") {
    assertEquals(asked[Ask +: Log +: St +: Pure].handle(askState[Log +: St +: Pure, Int]).handle(log).handle(state(7)).run,
      (7, (List("state 7", "got 7"), 8)))
  }

  test("a compiled program run twice: its state each run its own") {
    val p = for s <- perform[St, St +: Pure, Int](St.Get); _ <- perform[St, St +: Pure, Unit](St.Put(s + 1)) yield s
    val c = p.handle(state(0)).cont[(Int, Int)]
    assertEquals(c.run(identity), (1, 0))
    assertEquals(c.run(identity), (1, 0))
  }

  test("answer type modification in Cont0, around compiled programs: a shift0 turning String into Int => String, its k a compiled program, resumed twice") {
    type S = Int => String
    type C = Cont[String, String]
    val body: Cont0[Control[Pure], C, Cont[S, S], String] =
      Cont0.Shift0[Pure, String, String, String, Cont[S, S], Int](k =>
        Cont0.Return((x: Int) => k(x).run(identity) + " / " + k(x + 1).run(identity)))
      .flatMap((x: Int) => asked[Ask +: Log +: Pure].handle(reader(x)).handle(log).cont[C].map(_.toString))
    val p: Cont[S, S] = Cont0.Dollar(body, (s: String) => Cont0.Return[Control[Pure], String, String]("got " + s))
    assertEquals(p.run(identity)(41), "got (List(got 41),42) / got (List(got 42),43)")
  }

  test("a handler in the program: its parameter a value of the program") {
    def p[R <: Row](using In[Ask, R], In[Log, R]): Int ! R =
      for
        n <- perform[Ask, R, Int](Ask.Get)
        m <- perform[Ask, Ask +: R, Int](Ask.Get).map(_ * 10).handle(reader(n + 1))
      yield n + m
    assertEquals(p[Ask +: Log +: Pure].handle(reader(1)).handle(log).run, (Nil, 21))
  }

  test("a handler in the program: a raise caught, the program going on") {
    def step[R <: Row](i: Int)(using In[Throws, R]): Int ! R =
      if i % 2 == 0 then perform[Throws, R, Nothing](Throws.Raise(s"odd $i")) else pure(i)
    def p[R <: Row](using In[Log, R]): Int ! R =
      for
        a <- step[Throws +: R](1).handle(attempt)
        b <- step[Throws +: R](2).handle(attempt)
        _ <- perform[Log, R, Unit](Log.Tell(s"$a $b"))
      yield a.getOrElse(0) + b.getOrElse(0)
    assertEquals(p[Log +: Pure].handle(log).run, (List("Right(1) Left(odd 2)"), 1))
  }

  test("a handler in the program: its effect not seen outside, its state each time its own") {
    /** a counter of its own: `St` used inside, the row outside without it */
    def counted[R <: Row](n: Int)(using In[Log, R]): Int ! R =
      def go(i: Int): Unit ! St +: R =
        if i == 0 then pure(())
        else perform[St, St +: R, Int](St.Get).flatMap(s => perform[St, St +: R, Unit](St.Put(s + 1))).flatMap(_ => go(i - 1))
      go(n).handle(state(0)).map(_._1)
    def p[R <: Row](using In[Log, R]): Int ! R =
      for a <- counted[R](3); b <- counted[R](4); _ <- perform[Log, R, Unit](Log.Tell(s"$a $b")) yield a + b
    assertEquals(p[Log +: Pure].handle(log).run, (List("3 4"), 7))
  }

  test("a handler in the program, under a Deep one outside: each path of k with its own") {
    def p[R <: Row](using In[Choose, R]): Int ! R =
      for
        b <- perform[Choose, R, Boolean](Choose.Flip)
        r <- perform[St, St +: R, Int](St.Get).flatMap(s => perform[Choose, St +: R, Boolean](Choose.Flip)
               .flatMap(c => perform[St, St +: R, Unit](St.Put(s + (if c then 1 else 2))))).handle(state(if b then 10 else 20))
      yield r._1
    assertEquals(p[Choose +: Pure].handle(choose).run, List(11, 12, 21, 22))
  }

  val N = 100000

  test("stack safe: a loop of handlers in the program, each its operations forwarded out") {
    def loop[R <: Row](n: Int, acc: Int)(using In[Log, R]): Int ! R =
      if n == 0 then pure(acc)
      else asked[Ask +: R].handle(reader(n % 2)).flatMap(v => loop(n - 1, acc + v))
    assertEquals(loop[Log +: Pure](N, 0).handle(log).run._2, N + N / 2)
  }

  test("stack safe: a loop of operations, each forwarded past Deep's $ and a state") {
    def loop[R <: Row](n: Int, acc: Int)(using In[Ask, R], In[Choose, R]): Int ! R =
      if n == 0 then pure(acc)
      else perform[Ask, R, Int](Ask.Get).flatMap(x => loop(n - 1, acc + x))
    val p = loop[Choose +: St +: Ask +: Pure](N, 0).handle(choose).handle(state(0)).handle(reader(1))
    assertEquals(p.run, (0, List(N)))
  }

  test("stack safe: binds nested to the left, maps") {
    val p = (1 to N).foldLeft(pure[Ask +: Pure, Int](0))((p, _) =>
      p.flatMap(a => perform[Ask, Ask +: Pure, Int](Ask.Get).map(a + _)))
    assertEquals(p.handle(reader(1)).run, N)
  }

  test("stack safe: a recursion under maps, deferred") {
    def down(n: Int): Int ! Pure = if n == 0 then pure(0) else Cont0.defer(down(n - 1)).map(_ + 1)
    assertEquals(down(N).run, N)
  }
