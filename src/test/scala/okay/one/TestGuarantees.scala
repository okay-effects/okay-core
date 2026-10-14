package okay.one

import okay.one.Cont.{perform, pure}
import okay.one.Handlers.*
import okay.one.row.*

/** what the types refuse, each beside a program like it that is taken */
class TestGuarantees extends munit.FunSuite:
  test("run: only with no effect left") {
    assert(compileErrors("pure[Pure, Int](1).run").isEmpty)
    assert(compileErrors("perform[Ask, Ask +: Pure, Int](Ask.Get).run").nonEmpty)
    assert(compileErrors("perform[Ask, Ask +: Log +: Pure, Int](Ask.Get).handle(reader(1)).run").nonEmpty)
  }

  test("an operation: only of an effect in its row") {
    assert(compileErrors("perform[Ask, Log +: Ask +: Pure, Int](Ask.Get)").isEmpty)
    assert(compileErrors("perform[Ask, Log +: Pure, Int](Ask.Get)").contains("In"))
  }

  test("a handler: only of an effect in its row") {
    assert(compileErrors("perform[Ask, Log +: Ask +: Pure, Int](Ask.Get).handle(reader(1))").isEmpty)
    assert(compileErrors("perform[Log, Log +: Pure, Unit](Log.Tell(\"\")).handle(reader(1))").contains("Removed"))
  }

  test("a handler over the row outside it: only where its effects are there") {
    assert(compileErrors("perform[Ask, Ask +: Log +: Pure, Int](Ask.Get).handle(askLogged[Log +: Pure, Int](1)).handle(log).run").isEmpty)
    assert(compileErrors("perform[Ask, Ask +: Pure, Int](Ask.Get).handle(askLogged[Pure, Int](1))").nonEmpty)
  }

  test("shift0: only under a $ in its row — the nearest, never past one") {
    assert(compileErrors(
      "val c: Cont[Reset, Int ! Pure, Int ! Pure, Int] = Cont.shift0(k => k(1))").isEmpty)
    assert(compileErrors(
      "val p: Int ! Ask +: Pure = Cont.shift0[Pure, Nothing, Nothing, Int, Int ! Pure, Int, Ask +: Pure](k => k(1))").nonEmpty)
    assert(compileErrors(
      "perform[Ask, Ask +: Pure, Int](Ask.Get).flatMap(n => Cont.shift0[Pure, Nothing, Nothing, Int, Int ! Pure, Int, Ask +: Pure](k => k(n)))").nonEmpty)
    assert(compileErrors(
      "Cont.shift0[Pure, Nothing, Nothing, Int, Int ! Pure, Int, Delim[Reset]](k => k(1))").contains("Nearest"))
  }

  test("a program of effects under a $: at any answer by cont; back by reset, its effects handled around it") {
    assert(compileErrors("Cont.reset(perform[Ask, Delim[Ask +: Pure], Int](Ask.Get).cont[Int]).handle(reader(1)).run").isEmpty)
    assert(compileErrors("perform[Ask, Delim[Ask +: Pure], Int](Ask.Get).cont[Int].run(identity)").nonEmpty)
  }

  test("a handler under a $: one leaving the rest at any answers; one taking it only on a program of effects") {
    assert(compileErrors("perform[Choose, Choose +: Reset, Boolean](Choose.Flip).cont[Int].handle(choose)").nonEmpty)
    assert(compileErrors("perform[Throws, Throws +: Reset, Nothing](Throws.Raise(\"\")).cont[Int].handle(attempt)").nonEmpty)
    assert(compileErrors("perform[Ask, Ask +: Reset, Int](Ask.Get).cont[Int].handle(reader(1))").isEmpty)
    assert(compileErrors("perform[St, St +: Reset, Int](St.Get).cont[Int].handle(state(1))").isEmpty)
    assert(compileErrors("Cont.reset(perform[Choose, Delim[Choose +: Pure], Boolean](Choose.Flip).cont[Boolean]).handle(choose).run").isEmpty)
    assert(compileErrors("Cont.reset(perform[Choose, Choose +: Reset, Boolean](Choose.Flip).handle(choose).cont[List[Boolean]]).run").isEmpty)
  }

  test("a clause: a program over the row outside, not of its own effect") {
    assert(compileErrors("""new okay.one.handler.Deep[Ask, Log +: Pure, Int, Int]:
        def ret(a: Int) = pure(a)
        def apply[X](op: Ask[X], k: X => Int ! Log +: Pure) = op match
          case Ask.Get => perform[Log, Log +: Pure, Unit](Log.Tell("")).flatMap(_ => k(1))""").isEmpty)
    assert(compileErrors("""new okay.one.handler.Deep[Ask, Log +: Pure, Int, Int]:
        def ret(a: Int) = pure(a)
        def apply[X](op: Ask[X], k: X => Int ! Log +: Pure) = op match
          case Ask.Get => perform[Ask, Log +: Pure, Int](Ask.Get).flatMap(k)""").nonEmpty)
  }
