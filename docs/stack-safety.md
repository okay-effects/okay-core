# Stack safety

okay's rule, which okay-core keeps: no unbounded recursion on the JVM's stack — a recursive method is tail
(`@tailrec`), trampolined (deferred into a program's node), or bounded by a limit written beside it. Checked by
okay's scanner over the compiled classes:

```sh
sbt compile && python3 ../okay/scripts/recscan.py . scala2
```

On the one monad the account is short:

| Where | What | Why it is safe |
|---|---|---|
| `okay.one.machine.Machine` `eval` | one loop | `@tailrec`; the levels it is inside — handlers, views, `$`s — are its `Meta`, on the heap, never a JVM frame: handlers nested to any depth, resets, resumptions, in constant stack (`TestNesting`: 100 000 handlers installed by a recursive call, 100 000 resets) |
| `okay.one` `Cont.Defer` | a program built when the run gets to it | a tail call: `defer(p)` in a loop of programs costs no frame |
| `okay.core.handleIn` | a handler's body | `defer`red: built when the run enters the handler — a function installing a handler and calling itself inside it recurses in the machine, not on the stack |
| `okay.std.Stream`, `Step` | `map`, `flatMap`, `filter`, `from`, `concat` | lazy: each a function of the rest, run when pulled; their loops (`flatMap`, `filter` skipping) are `while`s |
| a generator's rest (`generate`, `stream`) | `k` as data | each pull a run of the rest on the machine, a run of its own |
| `okay.core.kernel.OnCore` | the bridge's delimiters | `Deep` handlers of the kernel's, their forwarding a `perform` and a `flatMap` — the machine's |

okay-one's own suites run on a 512k stack, forked (its `build.sbt`): a regression in the machine fails loudly there
before it is imported here.
