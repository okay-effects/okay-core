package okay.zeroclient

import okay.zero.*
import okay.zero.std.*

/** from outside okay.zero, what a program cannot do: the kernel reached, a request made up or taken apart, a
 * context made up, a shift0 with no reset or across a handler, an effect with no handler — compile errors, each
 * for its own reason, beside the lawful programs, which compile */
class TestSealed extends munit.FunSuite:

  test("the lawful programs compile: effects alone, control alone, both"):
    assert(compileErrors("""run(reader[Int](1)(ask[Int].map(_ + 1)))""").isEmpty)
    assert(compileErrors("""run[Int](reset[Int](shift[Int, Int](k => k(1).flatMap(k)).map(_ + 1)))""").isEmpty)
    assert(compileErrors("""run(reader[Int](1)(reset[Int](shift[Int, Int](k => k(1)).flatMap(x => ask[Int].map(_ + x)))))""").isEmpty)
    assert(compileErrors("""run[Int](shift[Int, Int](k => k(1)))""").isEmpty)
    assert(compileErrors("""run(reader[Int](1)(shift[Int, Int](k => k(1))))""").isEmpty)

  test("the kernel is not reached"):
    assert(compileErrors("""val k = okay.zero.Cont0.shift0""").nonEmpty)
    assert(compileErrors("""def raw(p: Eff[Empty, Int, Int, Int, Int]): okay.zero.Cont0[Int, Int, Int] = ???""").nonEmpty)

  test("a request cannot be made up or taken apart"):
    assert(compileErrors("""val r: Res[Empty, Int, Int] = Res.Done(-99)""").nonEmpty)
    assert(compileErrors("""def peek(r: Res[Empty, Int, Int]): Int = r match { case Res.Done(a: Int) => a; case _ => 0 }""").nonEmpty)

  test("a context cannot be made up, nor one of its own kind"):
    assert(compileErrors("""given At[Empty, Int, Int] = new At[Empty, Int, Int]()""").nonEmpty)
    assert(compileErrors("""given InReset[Empty, Int, Int, Int] = new InReset[Empty, Int, Int, Int]()""").nonEmpty)
    assert(compileErrors("""class Mine extends Ctx""").nonEmpty)

  test("a shift0 needs a reset: none, or a handler between, is a compile error"):
    assert(compileErrors("""run[Int](shift0[Int, Int](k => k(1)))""").nonEmpty)
    assert(compileErrors("""run[Int](reset[Int](reader[Int](1)(shift0[Int, Int](k => k(1)))))""").nonEmpty)

  test("an effect with no handler in force: no path to it"):
    assert(compileErrors("""run(reader[Int](1)(tell("x").map(_ => 1)))""").nonEmpty)
