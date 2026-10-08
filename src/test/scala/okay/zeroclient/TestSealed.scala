package okay.zeroclient

import okay.zero.*

/** from outside okay.zero, what a program cannot do: every way an operation could miss its handler — a request
 * made up, taken apart, caught by a raw delimiter; a context made up; an effect with no handler — a compile error.
 * Each beside the same program written as it may be, which compiles */
class TestSealed extends munit.FunSuite:
  enum Ask[+A]:
    case Number extends Ask[Int]
  def asking[R <: Row, S, A](using Ctx[R, S])(n: Int)(body: Ctx[Ask +: R, Inner[Ask, R, S, A]] ?=> Eff[Ask +: R, Inner[Ask, R, S, A], A]): Eff[R, S, A] =
    handle(new Handler[[X] =>> Eff[R, S, X], Ask, A, A]:
      def ret(a: A): A = a
      def apply[X](op: Ask[X], k: X => Eff[R, S, A]): Eff[R, S, A] = op match
        case Ask.Number => k(n))(body)
  type I = Inner[Ask, Empty, Int, Int]

  test("the program as it may be written compiles"):
    assert(compileErrors("""run(asking(1)(perform(Ask.Number).map(_ + 1)))""").isEmpty)

  test("a request cannot be made up: Done forged up to the handler"):
    assert(compileErrors("""val r: Res[Ask +: Empty, Int, Int] = Res.Done(-99)""").nonEmpty)
    assert(compileErrors("""val r: Res[Ask +: Empty, Int, Int] = Res.Req(Ask.Number, In.Here(), (x: Int) => ???)""").nonEmpty)

  test("a request cannot be taken apart"):
    assert(compileErrors("""def peek(r: Res[Ask +: Empty, Int, Int]): Int = r match { case Res.Done(a) => a; case _ => 0 }""").nonEmpty)

  test("a context cannot be made up: an operation performed outside every handler"):
    assert(compileErrors("""given Ctx[Ask +: Empty, Int] = Ctx.Under(Ctx.Top())""").nonEmpty)

  test("no raw shift0 or $ in or around a program with effects: it is not a Cont0"):
    assert(compileErrors("""
      run(asking(1)(Cont0.shift0[Int, Int, Res[Ask +: Empty, Int, Int], I, Int](k => k(1)).flatMap(_ => perform(Ask.Number))))""").nonEmpty)
    assert(compileErrors("""
      run(asking(1)(Cont0.dollar[I, Int, Int, Res[Ask +: Empty, Int, Int], Int](x => ???)(perform(Ask.Number))))""").nonEmpty)
    assert(compileErrors("""
      def raw[R <: Row, S](p: Eff[R, S, Int]): Cont0[S, S, Int] = p""").nonEmpty)

  test("an effect with no handler in force: no path to it, a compile error"):
    enum Say[+A]:
      case Line(s: String) extends Say[Unit]
    assert(compileErrors("""run(asking(1)(perform(Say.Line("x")).map(_ => 1)))""").nonEmpty)
