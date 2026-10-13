package okay.one

import okay.one.Cont0.{perform, pure}
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

  test("shift0 and $: only in a program of control, none in a program of effects") {
    assert(compileErrors(
      "val c: Cont[Int, Int] = Cont0.Dollar(Cont0.Shift0[Pure, Int, Int, Int, Cont[Int, Int], Int](k => k(1)), (a: Int) => Cont0.Return[Control[Pure], Int, Int](a))").isEmpty)
    assert(compileErrors(
      "val p: Int ! Ask +: Pure = Cont0.Shift0[Pure, Int, Int, Int, Cont[Int, Int], Int](k => k(1))").nonEmpty)
    assert(compileErrors(
      "perform[Ask, Ask +: Pure, Int](Ask.Get).flatMap(n => Cont0.Shift0[Pure, Int, Int, Int, Cont[Int, Int], Int](k => k(n)))").nonEmpty)
  }

  test("a program of effects in control: back by reset, its effects handled around it; run with none left") {
    assert(compileErrors("Cont0.reset(perform[Ask, Ask +: Pure, Int](Ask.Get).cont[Int]).handle(reader(1)).run").isEmpty)
    assert(compileErrors("perform[Ask, Ask +: Pure, Int](Ask.Get).cont[Int].run(identity)").nonEmpty)
  }

  test("reset of a handler in control: refused — a handler is around a reset, not in control") {
    assert(compileErrors("perform[Choose, Choose +: Pure, Boolean](Choose.Flip).cont[Int].handle(choose)").nonEmpty)
    assert(compileErrors("Cont0.reset(perform[Choose, Choose +: Pure, Boolean](Choose.Flip).cont[Boolean]).handle(choose).run").isEmpty)
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
