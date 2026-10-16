package okay.core

/** delimited control in okay's interface: `reset`, `shift`, `shift0` primitives beside `pure`, `perform`, `flatMap` —
 * a program written once over `RowEffects[M]`, run on the kernel's program, the machine's own delimiter */
class TestRowControl extends munit.FunSuite:
  enum Ask[A]:
    case Number() extends Ask[Int]
  def reader[A](n: Int): Answering[Ask, A, A] = new Answering[Ask, A, A]:
    def ret(a: A): A = a
    def value[X](op: Ask[X]): X = op match
      case Ask.Number() => n

  type T = Ask +: Pure
  type ST = Shifts[Int, T] +: T

  /** `k` twice, an operation of the row outside in the rest: each resumption its own */
  def twice[M[_ <: Row, _]](using E: RowEffects[M]): M[T, Int] =
    import E.*
    reset[Int, T](shift[Int, T, Int](k => k(1).flatMap(a => k(a * 10).map(b => a + b)))
      .flatMap(x => perform[Ask, ST, Int](Ask.Number()).map(n => x + n)))

  /** shift0: its body outside the reset, `k` a program over the row there */
  def outside[M[_ <: Row, _]](using E: RowEffects[M]): M[T, Int] =
    import E.*
    reset[Int, T](shift0[Int, T, Int](k => k(2).flatMap(a => perform[Ask, T, Int](Ask.Number()).map(_ + a))).map(_ * 3))

  /** a shift in a shift's body: to the same reset */
  def nested[M[_ <: Row, _]](using E: RowEffects[M]): M[T, Int] =
    import E.*
    reset[Int, T](shift[Int, T, Int](k => shift[Int, T, Int](k2 => k2(5).flatMap(k)).map(_ + 1)).map(_ * 2))

  val E: RowEffects[Eff] = RowEffects.eff

  test("reset and shift as primitives: k twice, the reader outside each time"):
    assertEquals(E.run(E.handleAt(reader[Int](41))(twice[Eff])), 42 + 461)

  test("shift0: its body outside the reset"):
    assertEquals(E.run(E.handleAt(reader[Int](41))(outside[Eff])), 41 + 6)

  test("a shift in a shift's body: up to the same reset"):
    assertEquals(E.run(E.handleAt(reader[Int](41))(nested[Eff])), (5 + 1) * 2) // k2(5) the rest up to the reset, 6; k(6), 12

  test("100 000 shifts in a loop, each k once: constant stack"):
    import E.*
    def loop(i: Int, acc: Int): Eff[ST, Int] =
      if i == 0 then pure(acc) else shift[Int, T, Int](k => k(1)).flatMap(x => tailcall(loop(i - 1, acc + x)))
    assertEquals(E.run(E.handleAt(reader[Int](0))(reset[Int, T](loop(100000, 0)))), 100000)
