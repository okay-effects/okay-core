# okay-core as an implementation of okay-kernel

okay-kernel is a protocol: what lets implementations of effects and delimited control work with each other in one
process (okay-kernel's `docs/design.md`). The module `kernel/` (`okay-core-kernel`, package `okay.core.kernel`, the
JVM's) makes okay-core one of them: `OnCore.zero` and `OnCore.cont` implement the kernel's `Effects` and `Direct` on
okay-core's machines — Cont0's and Cont's. The core itself names no library; this module depends on the kernel's API
(`dev.okay:okay-kernel-api`, pure Java).

```scala
import okay.core.kernel.OnCore
import okay.kernel.std.Handlers

val c = OnCore.zero.computation(program, row)   // export: run on Cont0, steps out where it needs what it cannot answer
OnCore.cont.foreign(otherComputation)           // import: another implementation's program, answered by okay-core's handlers
```

## How

A kernel program (`Prog`) is a small tree of the bridge's nodes — context-free, as the kernel's programs are — and is
translated into okay-core's program where it is placed: at okay-core's context there, with the bridge's handlers in
force there. A bind's function is translated when the bind runs, a handler's body when the handler is entered, so a
run is okay-core's own loop, in constant stack.

- **An operation** goes to the nearest handler of its effect by identity (`Type`). All the kernel's operations are
  one okay-core effect, `Dyn[X] = Op[X]`; the bridge finds the handler and gives okay-core its path, `In`.
- **Handlers, by shape.** `Answering` is okay-core's `Answering`: answered in place by its machine. `Aborting` is an
  okay-core `Answering` too, its value a signal that unwinds to it. `General` is an okay-core handler, its clause the
  kernel's, `k` okay-core's (deep, multi-shot). `Returning`'s return is okay-core's `ret` itself — a value, nothing
  flattened; a failure of it is a signal read where the handler is left, so `recover` catches it. A `General`
  return (a program) makes the okay-core answer that program, run when the handler is left.
- **`shift0` and `$`** are the bridge's: `$` an okay-core handler whose return is `k`; `shift0` an operation to the
  nearest delimiter — a `$` or a kernel handler — whose clause runs the body in the delimiter's place with okay-core's
  `k`. okay-core's own `shift`/`reset`/`shift0` are not used: the bridge's own delimiters (`recover`, `ensuring`,
  `uncancelable`, an abort's catcher) are okay-core handlers, and okay-core's control would stop at them; the kernel's
  passes them. An `Answering` handler has a delimiter of the bridge around it for `shift0`: one okay-core handler
  more, transparent to everything else.
- **Failures, aborts, cancellation** are not okay-core primitives, so they are the bridge's effect: an `Unwind`
  operation to the nearest `recover`, `ensuring`, abort catcher or the top, each taking what concerns it and
  performing the rest again outward. An exception of the program's functions (a bind's, a map's, a delay's, a clause,
  an `Answering`'s value, a `Program.in`) is caught where the bridge calls it and unwinds from there. What a handler
  or the host answers an operation can be a signal — a failure (`k.fail`), a cancellation (`k.cancel`), an abort, a
  cancellation pending inside an `uncancelable` region — read where the operation was performed. A region takes an
  unwinding too: ended so, it cancels if a cancellation waits, else lets it go on.
- **Export** (`computation`): the program under the bridge's top handler, on okay-core's top. An operation with no
  handler of it in force is answered by the host in place if it answers (no okay-core operation at all), else
  performed to the top handler, whose clause answers `Step.Yield` with okay-core's `k` — each resumption a run of it
  on okay-core's top: multi-shot, from any thread. **Import** (`foreign`): another's computation started where it is
  placed, offered in place the answers of the `Answering` handlers in force (else the host's), its yields performed
  here; an abort or a cancellation crossing it cancels it, its finalizers' operations answered here, then goes on.
- **A `k` resumed under other handlers** than it was made under — a `shift0`'s `k` used further out, a clause
  resuming `k` under a handler of its own, a generator's rest in another computation — runs under forwarding
  handlers that perform again, by the kernel's rules, what reaches them: in place where the handler found answers
  in place (an okay-core `Answering` forwarder), else by a clause.
- **`Direct`**: the same nodes, untyped. A kernel `Prog` already is the bridge's node — there is no wrapper around
  okay-core's program to skip — so `Direct` saves only the typed signatures.

## What is exact, what is approximated

Exact — the reference machine's semantics (okay-kernel's `Steps`), checked by the same suites and all of the TCK, on
both machines: the standard handlers, `shift0`/`$` (through `recover` and `ensuring`, handlers as delimiters, the
body outside its delimiter), failures and `recover` (a `Returning`'s answer's too), finalizers after a value, a
failure, an abort and a cancellation, `uncancelable` regions (a region that fails with a cancellation waiting
cancels), export with multi-shot `k`, the host answering in place, `k.fail`, `k.cancel`, import (a foreign program
crossed by an abort is cancelled, its finalizers' operations answered here), a `k` resumed under other handlers,
across implementations.

Approximated, the one thing: what is decided **in place** when a node is translated keeps the place it was
translated at. A `k` resumed somewhere else carries, already translated, the operations the host it was made under
answered in place, and a foreign computation started inside it is offered in place the answers of the handlers it was
made under. Its operations that reach its handlers' places are performed again where it runs (the forwarders); those
answered in place at translation are not.

Cost: every node is translated when placed, every bind guarded (a `try`), every operation's value read for a signal
(one bind), a `Returning` handler's answer read (one bind); an `Answering` handler is two okay-core handlers and an
`Aborting` one two; a `k` resumed under other handlers goes through a forwarder for each handler it may reach.

## Tests

`kernel/src/test/scala/okay/core/kernel/TestOnCore.scala`, `sbt kernel/test`: 54 tests, all passing —

- the reference machine's twelve checks (okay-kernel's `TestSteps`: state 100 000 operations in constant stack,
  writer + state + reader, raise + finalizer + `handleWith`, choose all / first, the `shift0`/`$` laws, a generator,
  failure and `recover`, export multi-shot, the host answering in place, `k.fail`, `k.cancel` and `uncancelable`,
  import), plus a `k` resumed under other handlers and a check across implementations — a computation of the
  reference machine imported under OnCore's handlers and one of OnCore's under the reference's, the same results:
  14 on `OnCore.zero`, 14 on `OnCore.cont`;
- okay-kernel's TCK (`TckSuite`, nesting through the reference machine `Steps`): its 13 checks — among them a resume
  from another thread, a `k` resumed under other handlers, a shift0's body outside its delimiter, a failure of a
  handler's answer, a region failing with a cancellation waiting, a foreign program ended by an abort, and 1 000
  random programs with the reference's digest — on each machine.
