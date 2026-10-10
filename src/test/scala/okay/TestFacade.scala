package okay

/** what a user writes with `import okay.*` alone: `!`, `+`, `%`, the standard effects, `run` */
class TestFacade extends munit.FunSuite:
  def greet: Int ! Reader % String + Writer % String =
    ask[String].flatMap(name => tell(s"hello, $name").map(_ => name.length))

  def counted: Int ! State % Int + Reader % Int + Throws % String =
    ask[Int].flatMap(n => if n < 0 then raise[String, Int]("negative") else modify[Int](_ + n))

  test("a program as its type says it, run on Cont0 and on Cont"):
    assertEquals(run(writer[String, Int](reader("Ann")(greet))), (List("hello, Ann"), 3))
    assertEquals(runOn(okay.cont.top)(writer[String, Int](reader("Ann")(greet))), (List("hello, Ann"), 3))

  test("three effects joined, handled in any order"):
    assertEquals(run(throws[String, (Int, Int)](state[Int](1)(reader(41)(counted)))), Right((42, 42)))
    assertEquals(run(state[Either[String, Int]](1)(throws[String, Int](reader(-1)(counted)))), (1, Left("negative")))
