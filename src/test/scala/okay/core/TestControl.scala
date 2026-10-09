package okay.core

import okay.cont.{Cont, Free, At}

import Control.{reset, shift}

/** CONTROL alone, beyond master's: Danvy–Filinski's shift in a shift's body, k lazy, shift0's hierarchy */
abstract class TestControlOn(on: Backend) extends BackendSuite(on):

  test("a body dropping k aborts the reset: the rest never runs"):
    var ran = false
    assertEquals(run[Int](reset[Int](shift[Int, Int](_ => pure(-1)).map(x => { ran = true; x }))), -1)
    assert(!ran)

  test("multi-shot: k resumed twice, the answers joined"):
    assertEquals(run[List[Int]](reset[List[Int]](shift[Int, List[Int]](k => k(1).flatMap(a => k(2).map(b => a ++ b))).map(x => List(x, x * 10)))),
      List(1, 10, 2, 20))

  test("a shift's body runs under the reset: a shift in it captures up to the same reset — Danvy–Filinski's"):
    assertEquals(run[Int](reset[Int](shift[Int, Int](k => shift[Int, Int](k2 => k2(1).map(_ * 100)).flatMap(k)).map(_ + 1))), 200)

  test("a shift captures up to the nearest reset only"):
    assertEquals(run[Int](reset[Int](shift[Int, Int](k => k(1).map(_ * 100))).flatMap(a => reset[Int](shift[Int, Int](k => k(2).map(_ + 7)).map(_ + a)))), 109)

  test("k applied runs nothing of the rest: the rest runs when stepped"):
    var ran = false
    assertEquals(run[Int](reset[Int](shift[Int, Int](k => { k(1); pure(if ran then 1 else 0) }).flatMap(x => { ran = true; pure(x) }))), 0)

  test("100 000 resets and shifts in constant stack"):
    def many(n: Int, acc: Int)(using c: Effects): c.Prog[Int] =
      if n == 0 then pure(acc) else reset[Int](shift[Int, Int](k => k(1)).map(_ + 1)).flatMap(x => many(n - 1, acc + x))
    assertEquals(run[Int](many(100000, 0)), 200000)

/** on `Cont` */
class TestControl extends TestControlOn(Backend.cont)
