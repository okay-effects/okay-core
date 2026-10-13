package okay.one

import okay.one.Cont0.*
import okay.one.Handlers.*
import okay.one.row.*

/** effects in control: a program of effects in it (`cont`), its operations going out past the `$`s to the handlers
 * around the `reset` that makes the program of control one of effects again */
class TestControl extends munit.FunSuite:
  type C[R <: Row, L, A] = Cont0[Control[R], L, L, A]

  def ask[R <: Row, L](using In[Ask, R]): C[R, L, Int] = perform[Ask, R, Int](Ask.Get).cont[L]
  def flip[R <: Row, L](using In[Choose, R]): C[R, L, Boolean] = perform[Choose, R, Boolean](Choose.Flip).cont[L]

  test("an operation in control, under a $ and a shift0 resumed twice: out of the reset to its handler") {
    type R = Ask +: Pure
    // `$ (ask + shift0(k => k(1) + k(2)))`, `ask` 10
    val body: Cont0[Control[R], C[R, Int, Int], C[R, Int, Int], Int] =
      Shift0[R, Int, Int, Int, C[R, Int, Int], Int](k => Bind(k(1), (a: Int) => Map(k(2), (b: Int) => a + b)))
    val p: C[R, Int, Int] =
      Dollar[R, C[R, Int, Int], Int, Int, Int, Int](Bind(ask[R, C[R, Int, Int]], (n: Int) => Map(body, (x: Int) => n + x)),
        (a: Int) => Return(a))
    assertEquals(reset(p).handle(reader(10)).run, 23)
  }

  test("a Deep handler's k taken past a reset and a $, resumed twice: each time the $ put back, its shift0 working") {
    type R = Choose +: Pure
    type L = C[R, Int, Int]
    // `reset($ (flip >>= b => shift0(k => k(b ? 1 : 2) + k(10))))`, the `$`'s `ret` times 100
    val body: Cont0[Control[R], L, L, Int] = Bind(flip[R, L], (b: Boolean) =>
      Shift0[R, Int, Int, Int, L, Int](k => Bind(k(if b then 1 else 2), (a: Int) => Map(k(10), (c: Int) => a + c))))
    val p: L = Dollar[R, L, Int, Int, Int, Int](body, (a: Int) => Return(a * 100))
    assertEquals(reset(p).handle(choose).run, List(1100, 1200))
  }

  test("printf under choose: the answer moved to a function, one a path") {
    type R = Choose +: Pure
    type K = C[R, String, String]
    type S = Int => K
    // `reset($ (flip >>= b => "got " + b + shift0(k => pure(x => k(x)))))`: the answer `String` turned `Int => K`
    val body: Cont0[Control[R], K, C[R, S, S], Int] =
      Shift0[R, String, String, String, C[R, S, S], Int](k => Return((x: Int) => k(x)))
    val p: C[R, S, S] = Dollar[R, K, S, S, S, String](
      Bind(flip[R, C[R, S, S]], (b: Boolean) => Map(body, (n: Int) => s"$b $n")),
      (s: String) => Return[Control[R], String, String]("got " + s))
    val fs = reset(p).handle(choose).run
    assertEquals(fs.map(f => reset(f(42)).handle(choose).run), List(List("got true 42"), List("got false 42")))
  }

  test("a Deep handler's program in control, its operations of others out of the reset") {
    type R = Ask +: Pure
    val p: Int ! Choose +: Ask +: Pure = for b <- perform[Choose, Choose +: R, Boolean](Choose.Flip)
      n <- perform[Ask, Choose +: R, Int](Ask.Get) yield if b then n else -n
    assertEquals(reset(p.handle(choose).cont[List[Int]]).handle(reader(3)).run, List(3, -3))
  }

  test("stack safe: a loop of operations in control, out of the reset") {
    type R = Ask +: Pure
    def loop(i: Int, acc: Int): C[R, Int, Int] =
      if i == 0 then Return(acc) else Bind(ask[R, Int], (n: Int) => loop(i - 1, acc + n))
    assertEquals(reset(loop(100000, 0)).handle(reader(1)).run, 100000)
  }

  test("stack safe: 100 000 resets in a program of effects, each a shift0 resumed in it") {
    type R = Ask +: Pure
    type L = C[R, Int, Int]
    def one: Int ! R = reset(Dollar[R, L, Int, Int, Int, Int](
      Shift0[R, Int, Int, Int, L, Int](k => Map(k(1), (a: Int) => a + 1)), (a: Int) => Return(a)))
    def many(i: Int, acc: Int): Int ! R = if i == 0 then pure(acc) else one.flatMap(x => many(i - 1, acc + x))
    assertEquals(many(100000, 0).handle(reader(0)).run, 200000)
  }
