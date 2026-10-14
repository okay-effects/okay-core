package okay.one

import okay.one.Cont.{perform, pure}
import okay.one.handler.*
import okay.one.row.*
import okay.one.std.*
import okay.one.std.Audit.{audit, handleAudited, observe, record}

/** INTERCEPTORS: control and audit of effects — okay-rs's tests/intercept.rs, the same programs. A plain effect
 * intercepted for a part of the program; a sensitive one handled only under its audit, each operation told, passed on,
 * changed or refused */
class TestIntercept extends munit.FunSuite:
  // A SENSITIVE EFFECT: payments
  enum Pay[X]:
    case Transfer(to: String, amount: Int) extends Pay[String]
    case Balance() extends Pay[Int]
  given Sensitive[Pay] = Sensitive()

  def transfer[R <: Row](to: String, amount: Int)(using In[Pay, R]): String ! R =
    perform[Pay, R, String](Pay.Transfer(to, amount))
  def balance[R <: Row](using In[Pay, R]): Int ! R = perform[Pay, R, Int](Pay.Balance())

  /** the bank: a balance, each transfer taken from it */
  def bank[T <: Row, A](funds: Int) = new Stateful[Pay, T, Int, A, A]:
    def initial = funds
    def ret(s: Int, a: A) = pure(a)
    def value[X](s: Int, op: Pay[X]): X = op match
      case Pay.Transfer(to, amount) => s"ok:$to:$amount"
      case Pay.Balance() => s
    def next[X](s: Int, op: Pay[X]) = op match
      case Pay.Transfer(_, amount) => s - amount
      case Pay.Balance() => s

  /** an account as the trail shows it: its last four */
  def masked(account: String): String = "****" + account.takeRight(4)

  /** THE AUDIT OF PAYMENTS: a transfer over the limit refused, raised; the account written one way (changed before it
   * is passed on); each one told, its account masked, with its receipt; the balance told with its value */
  def guarded[T <: Row, A](limit: Int)(using In[Throws % String, T], In[Audit, T]) = new Interceptor[Pay, T, A, A]:
    def ret(a: A) = pure(a)
    def apply[X](op: Pay[X], k: X => A ! T, pass: Pass[Pay, T]): A ! T = op match
      case Pay.Transfer(to0, amount) =>
        val to = to0.trim.toUpperCase
        if amount > limit then
          val why = s"refused: $amount to ${masked(to)} over the limit $limit"
          record[T](why).flatMap(_ => Throws.raise[String, T](why))
        else for
          receipt <- pass(Pay.Transfer(to, amount))
          _ <- record[T](s"transfer $amount to ${masked(to)} → $receipt")
          a <- k(receipt)
        yield a
      case Pay.Balance() =>
        for
          b <- pass(Pay.Balance())
          _ <- record[T](s"balance → $b")
          a <- k(b)
        yield a

  type Outside = Audited[Pay] +: Throws % String +: Audit +: Pure

  def payments[R <: Row](using In[Pay, R]): Int ! R = for
    _ <- transfer[R](" acct-1234 ", 100)
    b <- balance[R]
    _ <- transfer[R]("acct-77", 5000)
  yield b

  test("a sensitive effect under its audit: passed on changed, told masked, refused over the limit") {
    val p = audit(guarded[Outside, Int](1000))(payments[Pay +: Outside])
    val (trail, result) = p.handleAudited(bank(1000)).handle(Throws.either).handle(Audit.trail).run
    assertEquals(trail, List(
      "transfer 100 to ****1234 → ok:ACCT-1234:100",
      "balance → 900",
      "refused: 5000 to ****T-77 over the limit 1000"))
    assertEquals(result, Left("refused: 5000 to ****T-77 over the limit 1000"))
  }

  test("a plain effect intercepted for a part of the program") {
    type T = Writer % String +: Pure
    def shouting[A] = new Deep[Writer % String, T, A, A]:
      def ret(a: A) = pure(a)
      def apply[X](op: Writer[String, X], k: X => A ! T) = op match
        case Writer.Tell(s) => Writer.tell[String, T](s.toUpperCase).flatMap(_ => k(()))
    val p: Unit ! T = Writer.tell[String, T]("a").flatMap(_ =>
      Audit.intercept(shouting[Unit])(Writer.tell[String, Writer % String +: T]("b")))
    assertEquals(p.handle(Writer.collect).run, (List("a", "B"), ()))
  }

  /** a value as the trail shows it, the same on every platform (Scala.js shows `()` as `undefined`) */
  def shown(x: Any): String = x match
    case () => "()"
    case v => v.toString

  /** an observer telling every operation and its answer to the trail */
  def telling[T <: Row, A](using In[Audit, T]) = new Observer[T, A, A]:
    def ret(a: A) = pure(a)
    def apply[E[_], X](op: Seen[E, X, T], k: X => A ! T): A ! T = for
      _ <- record[T](s"${op.effect} ${op.describe}")
      x <- op.proceed
      _ <- record[T](s"  → ${shown(x)}")
      a <- k(x)
    yield a

  test("an observer sees every operation of every effect on its way out, and its answer; the sensitive ones audited") {
    type R = Reader % Int +: State % Int +: Pay +: Audit +: Pure
    type T = Reader % Int +: State % Int +: Audited[Pay] +: Audit +: Pure
    val p: String ! R = for
      n <- Reader.ask[Int, R]
      _ <- State.modify[Int, R](_ + n)
      s <- State.get[Int, R]
      r <- transfer[R]("acct", s)
    yield r
    val (trail, (_, receipt)) =
      observe(telling[T, String])(p).handle(Reader(5)).handle(State(1)).handleAudited(bank(100)).handle(Audit.trail).run
    assertEquals(receipt, "ok:acct:6")
    assertEquals(trail, List(
      "Reader Ask()", "  → 5",
      "State Get()", "  → 1",
      "State Put(6)", "  → ()",
      "State Get()", "  → 6",
      "Pay Transfer(acct,6)", "  → ok:acct:6"))
  }

  test("an observer refusing a sensitive effect: a read-only run") {
    type R = Reader % Int +: Pay +: Throws % String +: Audit +: Pure
    type T = Reader % Int +: Audited[Pay] +: Throws % String +: Audit +: Pure
    def readOnly[A] = new Observer[T, A, A]:
      def ret(a: A) = pure(a)
      def apply[E[_], X](op: Seen[E, X, T], k: X => A ! T): A ! T =
        if op.effect == "Pay" then
          val why = s"read-only: ${op.describe} refused"
          record[T](why).flatMap(_ => Throws.raise[String, T](why))
        else op.proceed.flatMap(k)
    val p: String ! R = Reader.ask[Int, R].flatMap(n => transfer[R]("acct", n))
    val (trail, result) =
      observe(readOnly[String])(p).handle(Reader(5)).handleAudited(bank(100)).handle(Throws.either).handle(Audit.trail).run
    assertEquals(trail, List("read-only: Transfer(acct,5) refused"))
    assertEquals(result, Left("read-only: Transfer(acct,5) refused"))
  }

  test("an answer told each time the rest gets it: a choice resumed twice, two answers") {
    type R = Choose +: Audit +: Pure
    val (trail, paths) = observe(telling[R, Boolean])(Choose.flip[R]).handle(Choose.all).handle(Audit.trail).run
    assertEquals(paths, List(true, false))
    assertEquals(trail, List("Choose Among(List(true, false))", "  → true", "  → false"))
  }

  // the guarantee, by types

  /** a refused program: its message names the reason */
  def refused(errors: String, reason: String): Unit =
    assert(errors.contains(reason), errors)

  test("a sensitive effect is not handled by a plain handle") {
    refused(compileErrors("transfer[Pay +: Pure](\"a\", 1).handle(bank(10))"), "handled only under its audit")
  }

  test("a sensitive effect is not handled by handleAudited with no audit before it") {
    assert(compileErrors("transfer[Pay +: Pure](\"a\", 1).handleAudited(bank(10))").nonEmpty)
  }

  test("a sensitive effect is not intercepted as a plain one") {
    assert(compileErrors(
      "Audit.intercept(new Translate[Pay, Pay +: Pure, String] { def apply[X](op: Pay[X]) = perform[Pay, Pay +: Pure, X](op) })(transfer[Pay +: Pay +: Pure](\"a\", 1))"
    ).nonEmpty)
  }

  test("an observer's row outside: the sensitive effects audited — not handled by a plain handle there") {
    assert(compileErrors(
      "observe(telling[Pay +: Audit +: Pure, String])(transfer[Pay +: Audit +: Pure](\"a\", 1))").nonEmpty)
  }

  test("outside its audit no program performs it: the row there has Audited[Pay], not Pay") {
    assert(compileErrors("transfer[Outside](\"a\", 1)").nonEmpty)
  }
