package okay.one

import okay.one.Cont.*
import okay.one.Handlers.*
import okay.one.row.*
import okay.one.std.PState
import okay.one.std.PState.pstate

/** ATM AT WORK: a state whose type the steps move (`PState`, the type in the answer) — what was done checked by the
 * compiler — in a program of effects of the row beside it, `Log` and `Ask`, which know nothing of it */
class TestAtmUses extends munit.FunSuite:
  type R = Log +: Ask +: Pure
  val r = rows[Delim[R]]

  /** a program refused by the compiler for its answers not meeting, the message naming the states that do not */
  def doesNotMeet(errors: String, states: String*): Unit =
    assert(errors.contains("the answers do not meet"), errors)
    states.foreach(st => assert(errors.contains(st), errors))

  // A PROTOCOL: a connection closed, open, logged in — each step its state before and after in its type

  final case class Closed()
  final case class Open(host: String)
  final case class Authed(host: String, user: String)

  def connect[F](st: PState[R, Nothing, F])(host: String) =
    st.get[Closed].flatMap(_ => st.put[Closed](Open(host)))

  def login[F](st: PState[R, Nothing, F])(user: String) =
    st.get[Open].flatMap(o => st.put[Open](Authed(o.host, user)))

  def query[F](st: PState[R, Nothing, F])(q: String) = for
    a <- st.get[Authed]
    _ <- r.perform(Log.Tell(s"${a.user}@${a.host}: $q"))
  yield q.length

  def close[F](st: PState[R, Nothing, F]) =
    st.get[Authed].flatMap(_ => st.put[Authed](Closed()))

  test("a protocol: connect, login, query, close — the port asked of the row, the query told to it") {
    val session = pstate[R, Nothing, Closed, Closed, Int](Closed())(st => for
      port <- r.perform(Ask.Get)
      _ <- connect(st)(s"db:$port")
      _ <- login(st)("sergii")
      n <- query(st)("select 1")
      _ <- close(st)
    yield n)
    assertEquals(session.handle(reader(5432)).handle(log).run,
      (List("sergii@db:5432: select 1"), (Closed(), 8)))
  }

  test("a protocol: a query before the login does not compile") {
    doesNotMeet(compileErrors("""
      pstate[R, Nothing, Closed, Closed, Int](Closed())(st => for
        _ <- connect(st)("db")
        n <- query(st)("select 1")
        _ <- close(st)
      yield n)"""), "Open", "Authed")
  }

  test("a protocol: a session left open does not compile — it must end closed") {
    doesNotMeet(compileErrors("""
      pstate[R, Nothing, Closed, Closed, Int](Closed())(st => for
        _ <- connect(st)("db")
        _ <- login(st)("sergii")
        n <- query(st)("select 1")
      yield n)"""), "Authed", "Closed")
  }

  // A PIPELINE: the rows' schema the state's type — each stage adds its columns, a stage needing a column runs only
  // after the one adding it

  type Raw = (id: Int, amount: Int)
  type Enriched = (id: Int, amount: Int, country: String)
  type Scored = (id: Int, amount: Int, country: String, risky: Boolean)

  def load[F](st: PState[R, Nothing, F])(rows: Vector[Raw]) =
    st.get[Unit].flatMap(_ => st.put[Unit](rows))

  def enrich[F](st: PState[R, Nothing, F]) = for
    rows <- st.get[Vector[Raw]]
    _ <- r.perform(Log.Tell(s"enriched ${rows.size}"))
    _ <- st.put[Vector[Raw]](rows.map(x => x ++ (country = if x.id % 2 == 0 then "PL" else "UA")))
  yield ()

  def score[F](st: PState[R, Nothing, F]) = for
    limit <- r.perform(Ask.Get)
    rows <- st.get[Vector[Enriched]]
    scored = rows.map(x => x ++ (risky = x.amount > limit && x.country != "PL"))
    _ <- st.put[Vector[Enriched]](scored)
  yield scored.count(_.risky)

  val raw: Vector[Raw] = Vector((id = 1, amount = 50), (id = 2, amount = 900), (id = 3, amount = 700))

  test("a pipeline: load, enrich, score — the limit asked of the row, each stage told to it") {
    val run = pstate[R, Nothing, Unit, Vector[Scored], Int](())(st => for
      _ <- load(st)(raw)
      _ <- enrich(st)
      n <- score(st)
    yield n)
    val (told, (rows, risky)) = run.handle(reader(500)).handle(log).run
    assertEquals(told, List("enriched 3"))
    assertEquals(risky, 1)
    assertEquals(rows.map(_.risky), Vector(false, false, true))
  }

  test("a pipeline: scoring before enriching does not compile — no country yet") {
    doesNotMeet(compileErrors("""
      pstate[R, Nothing, Unit, Vector[Scored], Int](())(st => for
        _ <- load(st)(raw)
        n <- score(st)
      yield n)"""), "Raw] =>", "Enriched] =>")
  }
