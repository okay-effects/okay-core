package okay.zeroclient

import okay.core.{Effects, pure}
import okay.core.Control.{reset, shift}
import okay.std.*
import okay.zero.{Cont0, Eff, Level, At, Res, shift0}
import okay.core.Pure

/** from outside okay.zero, what a program cannot do: the kernel's loop reached, a request made up or taken apart, a
 * context made up, a shift0 with no reset or across a handler, an effect with no handler — compile errors, each
 * for its own reason, beside the lawful programs, which compile */
class TestSealed extends munit.FunSuite:

  test("the lawful programs compile: effects alone, control alone, both"):
    assert(compileErrors("""okay.zero.run(reader[Int](1)(ask[Int].map(_ + 1)))""").isEmpty)
    assert(compileErrors("""okay.zero.run[Int](reset[Int](shift[Int, Int](k => k(1).flatMap(k)).map(_ + 1)))""").isEmpty)
    assert(compileErrors("""okay.zero.run(reader[Int](1)(reset[Int](shift[Int, Int](k => k(1)).flatMap(x => ask[Int].map(_ + x)))))""").isEmpty)
    assert(compileErrors("""okay.zero.run[Int](shift[Int, Int](k => k(1)))""").isEmpty)
    assert(compileErrors("""okay.zero.run[Int](reset[Int](shift0[Int, Int](k => k(1))))""").isEmpty)

  test("the kernel is open as a monad of its own: raw shift0 and $, the levels in the types"):
    assert(compileErrors("""
      type P = Cont0[Int, Int, Int]
      val p: P = Cont0.shift0[Int, Int, Int, P, Int](k => k(10).flatMap(k)).map(_ + 1).reset0
      assert(p.value == 12)""").isEmpty)
    // its loop's machinery is not
    assert(compileErrors("""Cont0.step(Cont0.pure[Int, Int](1), ???)""").nonEmpty)

  test("no bridge between Eff and Cont0: a program with effects is not a kernel program, nor made of one"):
    assert(compileErrors("""def raw(p: Eff[Pure, Int, Int, Int, Int]): Cont0[Int, Int, Int] = p""").nonEmpty)
    assert(compileErrors("""def cooked(p: Cont0[Level[Pure, Int, Int], Level[Pure, Int, Int], Int]): Eff[Pure, Int, Int, Int, Int] = p""").nonEmpty)
    assert(compileErrors("""okay.zero.run(reader[Int](1)(Cont0.dollar[Int, Int, Int, Int, Int](identity)(ask[Int])))""").nonEmpty)

  test("a request cannot be made up or taken apart"):
    assert(compileErrors("""val r: Res[Pure, Int, Int] = Res.Done(-99)""").nonEmpty)
    assert(compileErrors("""def peek(r: Res[Pure, Int, Int]): Int = r match { case Res.Done(a: Int) => a; case _ => 0 }""").nonEmpty)

  test("a context of Cont0 cannot be made up"):
    assert(compileErrors("""given At[Pure, Int, Int] = new At[Pure, Int, Int]()""").nonEmpty)

  test("a shift0 needs a reset: none, or a handler between, is a compile error"):
    assert(compileErrors("""okay.zero.run[Int](shift0[Int, Int](k => k(1)))""").nonEmpty)
    assert(compileErrors("""okay.zero.run[Int](reset[Int](reader[Int](1)(shift0[Int, Int](k => k(1)))))""").nonEmpty)

  test("an effect with no handler in force: no path to it"):
    assert(compileErrors("""okay.zero.run(reader[Int](1)(tell("x").map(_ => 1)))""").nonEmpty)
