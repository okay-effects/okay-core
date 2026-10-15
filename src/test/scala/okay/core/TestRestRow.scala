package okay.core

import okay.core.std.*

/** THE PLATFORM'S RUNNER: its effect `E` taken off a program needing `Es` — `E` anywhere among them, in any order —
 * the rest a program needing the rest (`Split`, found where the runner is called) */
class TestRestRow extends munit.FunSuite:
  /** the runner: Reader taken off wherever it is */
  def runReader[R, A, Es[_]](r: R)(p: A ! Es)(using s: Split[Es, Reader % R]): A ! s.Rest =
    (c: Needs[s.Rest]) ?=> reader(r)(s.fit(p))

  def prog: Int ! Reader % Int + Writer % String + State % Int =
    for
      n <- ask[Int]
      _ <- tell(s"n=$n")
      _ <- modify[Int](_ + n)
      s <- get[Int]
    yield s
  def progReaderLast: Int ! State % Int + Writer % String + Reader % Int = prog
  def progReaderMiddle: Int ! Writer % String + (Reader % Int + State % Int) = prog

  test("Reader first among three, left-nested"):
    assertEquals(run(state(1)(writer(runReader(5)(prog)))), (6, (List("n=5"), 6)))

  test("Reader last, Reader in the middle of a nested sum: the same runner, the same answer"):
    assertEquals(run(state(1)(writer(runReader(5)(progReaderLast)))), (6, (List("n=5"), 6)))
    assertEquals(run(state(1)(writer(runReader(5)(progReaderMiddle)))), (6, (List("n=5"), 6)))

  test("a rest named abstractly, Reader % R + F: the rest F itself"):
    def inner[F[_]](p: Int ! Reader % Int + F): Int ! F = runReader(5)(p)
    def outer[Es[_]](p: Int ! Es)(using s: Split[Es, Reader % Int]): Int ! s.Rest = runReader(5)(p)
    assertEquals(run(state(1)(writer(outer(prog)))), (6, (List("n=5"), 6)))

  test("a program needing Reader alone: nothing left, a program needing nothing"):
    val p: Int ! Reader % Int = ask[Int].map(_ + 1)
    assertEquals(run(runReader(41)(p)), 42)

  test("runners in a chain, each taking its own off, any order"):
    def runState[S, A, Es[_]](s0: S)(p: A ! Es)(using sp: Split[Es, State % S]): (S, A) ! sp.Rest =
      (c: Needs[sp.Rest]) ?=> state(s0)(sp.fit(p))
    def runWriter[W, A, Es[_]](p: A ! Es)(using sp: Split[Es, Writer % W]): (List[W], A) ! sp.Rest =
      (c: Needs[sp.Rest]) ?=> writer(sp.fit(p))
    assertEquals(run(runWriter[String, (Int, Int), Writer % String](runState(1)(runReader(5)(progReaderLast)))), (List("n=5"), (6, 6)))

  test("a chain with nothing named: each runner's effect read off the program"):
    def runState[S, A, Es[_]](s0: S)(p: A ! Es)(using sp: Split[Es, State % S]): (S, A) ! sp.Rest =
      (c: Needs[sp.Rest]) ?=> state(s0)(sp.fit(p))
    def runWriter[W, A, Es[_]](p: A ! Es)(using sp: Split[Es, Writer % W]): (List[W], A) ! sp.Rest =
      (c: Needs[sp.Rest]) ?=> writer(sp.fit(p))
    assertEquals(run(runWriter(runState(1)(runReader(5)(progReaderLast)))), (List("n=5"), (6, 6)))
