package okay

/** THE ANSWER MOVED, IN OKAY'S WORDS: a typestate protocol and a schema-typed pipeline (the kernel's `TestAtmUses`)
 * written with `Atm`, `Hole`, `Atm.get`/`put`/`pstate` — no row named, the effects beside the state `Writer`, `Reader` */
class TestAtm extends munit.FunSuite:
  type Es = Writer % String + Reader % Int

  def doesNotMeet(errors: String, states: String*): Unit =
    assert(errors.contains("the answers do not meet"), errors)
    states.foreach(st => assert(errors.contains(st), errors))

  // A PROTOCOL: a connection closed, open, logged in — each step its state before and after in its type

  final case class Closed()
  final case class Open(host: String)
  final case class Authed(host: String, user: String)

  def connect[F](host: String)(using Atm.Typestate[F]): Atm[Unit, Hole[Open, F], Hole[Closed, F], Es] =
    Atm.get[Closed].flatMap(_ => Atm.put[Closed](Open(host)))

  def login[F](user: String)(using Atm.Typestate[F]): Atm[Unit, Hole[Authed, F], Hole[Open, F], Es] =
    Atm.get[Open].flatMap(o => Atm.put[Open](Authed(o.host, user)))

  def query[F](q: String)(using Atm.Typestate[F]): Atm[Int, Hole[Authed, F], Hole[Authed, F], Es] = for
    a <- Atm.get[Authed]
    _ <- tell(s"${a.user}@${a.host}: $q")
  yield q.length

  def close[F](using Atm.Typestate[F]): Atm[Unit, Hole[Closed, F], Hole[Authed, F], Es] =
    Atm.get[Authed].flatMap(_ => Atm.put[Authed](Closed()))

  test("a protocol: connect, login, query, close — the port asked of the row, the query told to it"):
    val session: (Closed, Int) ! Es = Atm.pstate[Es](Closed())(for
      port <- ask[Int]
      _ <- connect(s"db:$port")
      _ <- login("sergii")
      n <- query("select 1")
      _ <- close
    yield n)
    assertEquals(run(reader(5432)(writer[String, (Closed, Int), Reader % Int +: Pure](session))),
      (List("sergii@db:5432: select 1"), (Closed(), 8)))

  test("a protocol: a query before the login does not compile"):
    doesNotMeet(compileErrors("""
      val p: (Closed, Int) ! Es = Atm.pstate[Es](Closed())(for
        _ <- connect("db")
        n <- query("select 1")
        _ <- close
      yield n)"""), "Open", "Authed")

  test("a protocol: a session left open does not compile — it must end closed, it ends authed"):
    val errors = compileErrors("""
      val p: (Closed, Int) ! Es = Atm.pstate[Es](Closed())(for
        _ <- connect("db")
        _ <- login("sergii")
        n <- query("select 1")
      yield n)""")
    assert(errors.contains("Found:    okay.core.Eff[c.R, (TestAtm.this.Authed, Int)]"), errors)
    assert(errors.contains("Required: okay.core.Eff[c.R, (TestAtm.this.Closed, Int)]"), errors)

  // A PIPELINE: the rows' schema the state's type — each stage adds its columns, a stage needing a column runs only
  // after the one adding it

  type Raw = (id: Int, amount: Int)
  type Enriched = (id: Int, amount: Int, country: String)
  type Scored = (id: Int, amount: Int, country: String, risky: Boolean)

  def load[F](rows: Vector[Raw])(using Atm.Typestate[F]): Atm[Unit, Hole[Vector[Raw], F], Hole[Unit, F], Es] =
    Atm.get[Unit].flatMap(_ => Atm.put[Unit](rows))

  def enrich[F](using Atm.Typestate[F]): Atm[Unit, Hole[Vector[Enriched], F], Hole[Vector[Raw], F], Es] = for
    rows <- Atm.get[Vector[Raw]]
    _ <- tell(s"enriched ${rows.size}")
    _ <- Atm.put[Vector[Raw]](rows.map(x => x ++ (country = if x.id % 2 == 0 then "PL" else "UA")))
  yield ()

  def score[F](using Atm.Typestate[F]): Atm[Int, Hole[Vector[Scored], F], Hole[Vector[Enriched], F], Es] = for
    limit <- ask[Int]
    rows <- Atm.get[Vector[Enriched]]
    scored = rows.map(x => x ++ (risky = x.amount > limit && x.country != "PL"))
    _ <- Atm.put[Vector[Enriched]](scored)
  yield scored.count(_.risky)

  val raw: Vector[Raw] = Vector((id = 1, amount = 50), (id = 2, amount = 900), (id = 3, amount = 700))

  test("a pipeline: load, enrich, score — the limit asked of the row, each stage told to it"):
    val p: (Vector[Scored], Int) ! Es = Atm.pstate[Es](())(for
      _ <- load(raw)
      _ <- enrich
      n <- score
    yield n)
    val (told, (rows, risky)) = run(reader(500)(writer[String, (Vector[Scored], Int), Reader % Int +: Pure](p)))
    assertEquals(told, List("enriched 3"))
    assertEquals(risky, 1)
    assertEquals(rows.map(_.risky), Vector(false, false, true))

  test("a pipeline: scoring before enriching does not compile — no country yet"):
    doesNotMeet(compileErrors("""
      val p: (Vector[Scored], Int) ! Es = Atm.pstate[Es](())(for
        _ <- load(raw)
        n <- score
      yield n)"""), "Raw]", "Enriched]")
