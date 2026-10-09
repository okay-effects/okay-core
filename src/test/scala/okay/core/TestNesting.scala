package okay.core

import okay.std.*
import okay.core.Control.reset

/** DELIMITERS NESTED DEEP — a handler in a handler in a handler…, as a recursive function installing one each call
 * makes. okay's rule is no unbounded stack recursion (okay/scripts/recscan.py found these on okay-core): today a
 * delimiter at a body's head is entered by a recursive call — `Cont`'s fold of a handler folds the handler at the
 * head of its body, `Cont0`'s machine and kernel run a delimiter's body in a loop of their own — a frame or a few a
 * level — a thousand deep runs, a hundred thousand overflows — where the loop has no metacontinuation, the
 * delimiters a stack of its own (docs/stack-safety.md). `Cont0`'s machine has: `deep`, run to any depth; the others
 * pinned as the known failure they are */
abstract class TestNestingOn(on: Backend, deep: Boolean = false) extends BackendSuite(on):
  def readers(n: Int)(using c: Effects): c.Prog[Int] = if n == 0 then c.pure(0) else reader(n)(readers(n - 1).map(_ + 1))
  def resets(n: Int)(using c: Effects): c.Prog[Int] = if n == 0 then c.pure(0) else reset[Int](resets(n - 1).map(_ + 1))

  test("1 000 handlers nested, each installed by a recursive call; 1 000 resets"):
    assertEquals(run(readers(1000)), 1000)
    assertEquals(run(resets(1000)), 1000)
  /** whether `a` overflows the stack */
  def overflows(a: => Any): Boolean = try { a; false } catch case _: StackOverflowError => true

  if deep then
    test("100 000 handlers nested, each installed by a recursive call; 100 000 resets — in constant stack"):
      assertEquals(run(readers(100000)), 100000)
      assertEquals(run(resets(100000)), 100000)
  else
    test("100 000 handlers nested, 100 000 resets: the stack overflows — the known limit, pinned"):
      assert(overflows(run(readers(100000))), "nested handlers no longer overflow: the limit is gone, unpin it")
      assert(overflows(run(resets(100000))), "nested resets no longer overflow: the limit is gone, unpin it")

class TestNesting extends TestNestingOn(Backend.cont)
