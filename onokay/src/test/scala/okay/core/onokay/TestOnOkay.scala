package okay.core.onokay

import okay.cont.{+:, Member, Pure}
import okay.Effects

/** okay's program over `Effects[M]`, generic in `M`, run on the one monad: the rows mapped, the paths the same walk */
class TestOnOkay extends munit.FunSuite:
  enum Ask[+X]:
    case Number extends Ask[Int]
  enum Say[+X]:
    case Line(s: String) extends Say[Unit]

  /** okay's program, as okay writes one over any encoding: an operation by its path, binds, a tail call */
  def program[M[_ <: okay.cont.Row, _]](using E: Effects[M]): M[Ask +: Say +: Pure, Int] =
    E.perform[Ask, Ask +: Say +: Pure, Int](Ask.Number).flatMap(n =>
      E.perform[Say, Ask +: Say +: Pure, Unit](Say.Line(s"got $n")).flatMap(_ => E.pure(n + 1)))

  test("the paths: okay's Member is the kernel's In by the same walk") {
    assertEquals(OnOkay.in(summon[Member[Ask, Ask +: Say +: Pure]]), okay.one.row.In.Here())
    assertEquals(OnOkay.in(summon[Member[Say, Ask +: Say +: Pure]]), okay.one.row.In.There(okay.one.row.In.Here()))
  }

  test("okay's program on the one monad: its operations answered by the kernel's handlers at the mapped row") {

    import okay.one.handler.{Answering, Stateful}
    val p: OnOkay.P[Ask +: Say +: Pure, Int] = program[OnOkay.P](using OnOkay.effects)
    // the kernel's handlers over the mapped row, Ask +: Say +: Pure in the kernel's own rows
    type T1 = okay.one.row.+:[Say, okay.one.row.Pure]
    val asking = new Answering[Ask, T1, Int, Int]:
      def ret(a: Int) = okay.one.Cont.pure(a)
      def value[X](op: Ask[X]): X = op match { case Ask.Number => 41 }
    val saying = new Stateful[Say, okay.one.row.Pure, List[String], Int, (List[String], Int)]:
      def initial = Nil
      def ret(s: List[String], a: Int) = okay.one.Cont.pure((s.reverse, a))
      def value[X](s: List[String], op: Say[X]): X = op match { case Say.Line(_) => () }
      def next[X](s: List[String], op: Say[X]) = op match { case Say.Line(l) => l :: s }
    assertEquals(p.handle(asking).handle(saying).run, (List("got 41"), 42))
  }
