package okay.zeroclient

import okay.zero.*

/** from outside okay.zero, what a program cannot do: every way an operation could miss its handler or a shift its
 * reset — the kernel reached, a request made up or taken apart, a context made up, a shift with no reset or a
 * handler for one, an effect with no handler — a compile error. Each beside the lawful program, which compiles */
class TestSealed extends munit.FunSuite:
  enum Ask[+A]:
    case Number extends Ask[Int]
  def asking[R <: Row, J, T, A](using Ctx[R, J])(n: Int)(body: Body[Ask, R, J, T, A]): Eff[R, J, T, T, A] =
    handle(new Handler[[X] =>> Eff[R, J, T, T, X], Ask, A, A]:
      def ret(a: A): A = a
      def apply[X](op: Ask[X], k: X => Eff[R, J, T, T, A]): Eff[R, J, T, T, A] = op match
        case Ask.Number => k(n))(body)

  test("the lawful programs compile"):
    assert(compileErrors("""run[Int](asking(1)(perform(Ask.Number).map(_ + 1)))""").isEmpty)
    assert(compileErrors("""run[Int](asking(1)(reset(shift[Int, Int, Int](k => k(1)).flatMap(x => perform(Ask.Number).map(_ + x)))))""").isEmpty)

  test("the kernel is not reached: no Cont0, no raw shift0 or $"):
    assert(compileErrors("""val k = okay.zero.Cont0.shift0""").nonEmpty)
    assert(compileErrors("""def raw[R <: Row, J, T](p: Eff[R, J, T, T, Int]): okay.zero.Cont0[Int, Int, Int] = ???""").nonEmpty)

  test("a request cannot be made up or taken apart"):
    assert(compileErrors("""val r: Res[Ask +: Empty, Int, Int] = Res.Done(-99)""").nonEmpty)
    assert(compileErrors("""def peek(r: Res[Ask +: Empty, Int, Int]): Int = r match { case Res.Done(a) => a; case _ => 0 }""").nonEmpty)

  test("a context cannot be made up"):
    assert(compileErrors("""given Ctx[Ask +: Empty, Int] = new Ctx[Ask +: Empty, Int]()""").nonEmpty)
    assert(compileErrors("""given Reset[Empty, Int, Int] = new Reset[Empty, Int, Int]()""").nonEmpty)

  test("a shift needs a reset: none, or a handler between, is a compile error"):
    assert(compileErrors("""run[Int](shift[Int, Int, Int](k => k(1)))""").nonEmpty)
    assert(compileErrors("""run[Int](reset(asking(1)(shift[Int, Int, Int](k => k(1)))))""").nonEmpty)

  test("an effect with no handler in force: no path to it"):
    enum Say[+A]:
      case Line(s: String) extends Say[Unit]
    assert(compileErrors("""run[Int](asking(1)(perform(Say.Line("x")).map(_ => 1)))""").nonEmpty)
