# Stack safety

okay's rule, which okay-core keeps: no unbounded recursion on the JVM's stack — a recursive method is tail
(`@tailrec`), trampolined (deferred into a program's node: `Delay`, `Bind`, a lazy rest), or bounded by a limit
written beside it. Checked by okay's scanner over the compiled classes:

```sh
sbt compile && python3 ../okay/scripts/recscan.py . scala2
```

It finds every cycle in the call graph left after tail calls; a cycle through a node it knows defers its body is
dropped — it knows okay's, not okay-core's, so a cycle through `Cont.Delay`, `Cont0.Bind` or a stream's lazy rest is
reported and is not stack recursion. The inventory, 2026-10-09 (30 rows):

| Where | What | Why it is safe — or not |
|---|---|---|
| `cont/Cont.scala` `intercepted` | lambda through `Cont.Delay` | trampolined: the rest re-intercepted when the loop gets to it |
| `zero/Eff.scala` `atTop`, `read`, `readHand`, `readIntercept`, `passAll`, `again`, `inside` | lambdas through `Cont0.Bind`, `flatMap` | trampolined: the kernel's nodes, run by its loop |
| `std/Stream.scala` `map`, `flatMap`, `filter`, `from`, `concat` | lambdas through `Step.Later` | lazy: each a function of the rest, run when pulled; their loops (`flatMap`, `filter` skipping) are `while` |
| `core/Program.scala` `flatMap`, `map` | the composed bind | bounded: `Program.Fused`, 64 binds composed, then a bind on the bind, its left built by `delay` |
| `zero/Machine.scala` `masked` | direct | bounded: the path's length, the row's at most |
| `zero/Machine.scala` `deliver` ↔ `shiftAtReset`, `shiftAtHandler` | mutual | bounded: a resumption run nested in a shift's body up to `Nested`, 64, then out and back |
| `cont/Cont.scala` `step` ↔ `loop0` ↔ `loop` ↔ `delimited` | mutual | **not bounded**: a `reset` at a body's head stepped in a loop of its own |
| `cont/At.scala`, `cont/Cont.scala` `handle` → `fold0` | (through the body's building) | **not bounded**: a handler is a fold made at once, so one at the head of another's body is folded inside it |
| `zero/Cont0.scala` `step` ↔ `delimit` | mutual | **not bounded**: a `$` at a body's head stepped in a loop of its own |
| `zero/Machine.scala` `deliver` ↔ `deliver0`, `enter`, `enterHandle`, `enterIntercept`, `enterReset` | mutual | **not bounded**: a delimiter at a body's head entered in a loop of its own |

## The known limit: delimiters nested deep

The rows marked not bounded are one thing: a delimiter — a handler, a reset, an interceptor — at the head of
another's body is entered by a call, a few frames a level. A program nesting delimiters by recursion — a function
that installs a handler and calls itself inside it — runs a thousand deep and overflows at about ten thousand, on
both monads and on `Ref` (`TestNestingOn` pins it: 1 000 run; 100 000 overflow, asserted, so the test says when it
is gone). Operations, binds, tail calls, resumptions, handlers one after another and handlers nested as deep as a
program's text are all constant in stack; only nesting by recursion is not.

The way out is a METACONTINUATION: the loop keeps the delimiters it is inside as a stack of its own — each with
its frames — instead of a JVM frame each; entering a delimiter pushes, its value pops and goes on with the frames
under it, an operation walks the stack to its handler (as `Ev` already walks the handlers in force). It is Materzok
and Biernacki's abstract machine for shift0/$ (a "trail" of contexts), and Effekt's and Koka's runtime stacks of
handlers. For `Cont0` it replaces `deliver`'s nesting (and the kernel's `delimit`) by one loop over the
metacontinuation; for `Cont` it means a handler a node the loop enters, not a fold made at once — which `Cont`
was by design. Not done yet: it changes both monads' cores.
