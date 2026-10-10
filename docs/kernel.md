# okay-core as an implementation of okay-kernel

okay-kernel is a protocol: what lets implementations of effects and delimited control work with each other in one
process (okay-kernel's `docs/design.md`). okay-core implements it **natively**: the core depends on okay-kernel's API
(`dev.okay:okay-kernel-api` on the JVM, a jar of interfaces and records; its Scala mirror `okay-kernel-scala` on
Scala.js and Native) and its machines run the kernel's programs themselves. `okay.core.kernel.OnCore.zero` and
`OnCore.cont` are okay-kernel's `Effects` on Cont0's machine and Cont's loop.

```scala
import okay.core.kernel.OnCore
import okay.kernel.std.Handlers

OnCore.zero.computation(program, row).start(host)   // export: run on Cont0, stepping out where it needs what it cannot answer
OnCore.cont.foreign(otherComputation)               // import: another implementation's program, answered here
```

## Native

**Nothing wrapped, nothing translated.** `Cont0` and `Cont` extend `okay.kernel.Prog`: a kernel program is the
machine's own nodes, built by `OnCore` directly — `pure` a `Return`, `flatMap` a `Bind`, `delay` a `Delay`. The
kernel's functions (`Fn`, `Thunk`) are called guarded: an exception of the program's is a failure where it is called.

**Operations by identity, in the machines.** The kernel's operations carry no path — the kernel's programs are
context-free, written once for every implementation — so the machines find their handler as they run, by the
effect's `Type`. Two nodes each, matched after every typed one: Cont0's `KOp` (`perform`: its value read for a signal
where it was performed) and `KOpK` (its rest bound, the value as it is); Cont's `KInject` and `KSuspend`. The handlers
in force — the machines' evidence `Ev`, which Cont0's machine and Cont's loop already keep to answer typed
operations in place — hold the kernel's handlers too: the nearest that takes the operation answers it in place if it
can (a cell's `get`, a reader's `ask`, the host's), else the operation goes to the delimiter as an operation does,
each delimiter passing it on unless it is a kernel handler that takes it. Nothing is known (`Ev.Unknown`, past a
delimiter the machine did not enter): routed, the same answer, slower. A `k` resumed anywhere — under other handlers,
in another computation — finds the handlers in force where it runs, as the reference machine does: no context
captured when a program was built.

**The handler shapes as the machines' handlers.** A kernel handler is the machine's own handler node (Cont0's
`Handle`, Cont's `Fold`), its clauses a `KHandler` — an okay-core `Answering`, so the machines keep it among the
handlers in force whatever its shape; okay-core's typed operations never reach it (their paths go to their own).
`Answering` is answered in place; `Aborting` is answered in place by a signal that unwinds to it (its finalizers run
on the way); `General` is a clause with the machine's `k`; `Returning`'s return is the machine's `ret` itself, a value,
its failure a signal read where the handler is left. A `General` return (a program) makes the handler's answer that
program, run when it is left.

**Control.** `$` is a handler node whose return is `k`; `shift0` an operation (`Shift0Op`) to the nearest
delimiter — a `$` or a kernel handler — whose clause runs the body in the delimiter's place with the machine's `k`,
the delimiter included. Not okay-core's `shift`/`shift0`: the native failures, finalizers and regions below are
handler nodes too, and okay-core's control would stop at them; the kernel's passes them.

**Failures, finalizers, cancellation**, which okay-core has no primitive for, at the least cost: an operation
`Unwind` to the nearest handler taking it — `recover` (a failure), `ensuring` (any: its finalizer, then on), an
`uncancelable` region (any: it ends the region; a cancellation waiting there is taken then), an `Aborting` handler
(its own abort), the top. What an operation is answered can be a `Signal` — a failure (`k.fail`, a throwing
`Answering`), a cancellation (`k.cancel`), an abort, a value given inside a region a cancellation waits for — read
where the operation was performed. A program that uses none of them pays for none: a `try` around the kernel's
functions, an `instanceof` on an operation's value.

**The protocol.** Export: the program under a top handler (`Host`) on the machine's top. An operation no handler of
which is in force is answered in place by the host if it answers (the host is among the handlers in force: no
capture), else handed out — `Step.Yield` with the machine's `k` as the `Resume`, each resumption a run of it on the
machine's top: multi-shot, from any thread. Import: another's computation, offered in place what the handlers in
force answer so (the machine's walk of its evidence), its yields performed here as operations; an abort or a
cancellation crossing it cancels it, its finalizers' operations answered here, then goes on.

**The typed front, untouched.** `A ! E1 + E2`, paths, `Effects`' contexts, the standard effects: the same code, the
same nodes; the kernel's nodes are matched after every typed one in each loop (`deliver`, `go`, `read`; `run`) and
their handling is out of the loops, in methods of its own. Measured below.

**Not done: okay-core's own operations as the kernel's vocabulary.** okay-core's `Get`, `Ask`, `Tell`… find their
handler by path, and carry no `Type`; to implement `State.Ops` and the rest they would need one — a `TypeOf` at
every `get[S]`, `ask[E]`, which would change the typed front's signatures (every generic caller would need it too)
and cost each typed operation a field. The two worlds meet at the border (`computation`, `foreign`) and by the
kernel's own vocabulary inside kernel programs; an okay-core typed program is not exported as it is.

## What is exact, what is approximated

Exact — the reference machine's semantics (okay-kernel's `Steps`), checked by okay-kernel's TCK (all 13 checks,
the 1 000 random programs' digest) on both machines: the standard handlers, `shift0`/`$` (through `recover` and
`ensuring`, handlers as delimiters, the body outside its delimiter), failures and `recover` (a `Returning`'s answer's
too), finalizers after a value, a failure, an abort and a cancellation, `uncancelable` regions (one that fails with a
cancellation waiting cancels), export with multi-shot `k` from any thread, the host answering in place, `k.fail`,
`k.cancel`, import (a foreign program crossed by an abort is cancelled, its finalizers' operations answered here), a
`k` resumed under other handlers, across implementations.

Approximated: nothing known to the TCK or to these suites. The bridge's one approximation — what was decided in place
when a program was translated kept that place — is gone with the translation.

Cost: a kernel operation answered in place walks the handlers in force to the nearest of its effect (no cache, as a
typed path has); an `Answering` handler's operations otherwise cost what okay-core's do.

## Tests

- `src/test/scala/okay/core/kernel/TestNative.scala` — on the JVM, Scala.js and Scala Native: a kernel program is the
  machine's nodes; state 100 000 deep; reader, writer, a raise with its finalizer, choose; `shift0`/`$` through
  `recover`; export, multi-shot `k`, `k.cancel`; import from the other machine — 6 on each machine.
- `kernel/` (the JVM's, tests only): the reference machine's twelve checks, a `k` under other handlers, a round trip
  through the reference machine (`Steps`) — 14 on each machine; okay-kernel's TCK (`TckSuite`, nesting through
  `Steps`) — 13 on each machine. 54 in all.

## Measured

`KernelMixedBenchmark` (src/jmh): okay-kernel's mixed program, written once over the kernel's `Effects` (okay-kernel's
bench/MixedBenchmark, the same code), on okay-core's machines natively — against `MixedBenchmark`, the same program in
okay-core's own words. See the numbers in changelog.d/kernel-native.md.
