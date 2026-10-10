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
  flattened; a `General` return (a program) makes the okay-core answer that program, run when the handler is left.
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
  cancellation pending inside an `uncancelable` region — read where the operation was performed.
- **Export** (`computation`): the program under the bridge's top handler, on okay-core's top. An operation with no
  handler of it in force is answered by the host in place if it answers (no okay-core operation at all), else
  performed to the top handler, whose clause answers `Step.Yield` with okay-core's `k` — each resumption a run of it
  on okay-core's top: multi-shot, from any thread. **Import** (`foreign`): another's computation started where it is
  placed, offered in place the answers of the `Answering` handlers in force (else the host's), its yields performed
  here.
- **`Direct`**: the same nodes, untyped. A kernel `Prog` already is the bridge's node — there is no wrapper around
  okay-core's program to skip — so `Direct` saves only the typed signatures.

## What is exact, what is approximated

Exact — the reference machine's semantics (okay-kernel's `Steps`), checked by the same suites and the TCK, on both
machines: the standard handlers, `shift0`/`$` (through `recover` and `ensuring`, handlers as delimiters), failures
and `recover`, finalizers after a value, a failure, an abort and a cancellation, `uncancelable` regions, export with
multi-shot `k`, the host answering in place, `k.fail`, `k.cancel`, import, across implementations.

Approximated or slower:

- an exception of a `Returning` handler's `answer` is okay-core's `ret`'s — a value, not a program — so it is not
  catchable by `recover`: it escapes okay-core's run and is the step's `Failed`;
- a foreign computation crossed by an abort or a cancellation is cancelled, and its cancellation's own steps are not
  answered — as the reference machine does;
- a `k` resumed under other handlers than it was made under (a `shift0`'s `k` used further out, a clause resuming
  `k` under a handler of its own) runs under forwarding handlers that perform again, by the kernel's rules, what
  reaches them: exact, but its operations through them are not answered in place, and a foreign computation inside
  it is offered in place the answers of the handlers `k` was made under;
- cost: every node is translated when placed, every bind is guarded (a `try`), every operation's value is read for
  a signal (one bind), an `Answering` handler is two okay-core handlers and an `Aborting` one two.

## Tests

`kernel/src/test/scala/okay/core/kernel/TestOnCore.scala`, `sbt kernel/test`: 44 tests, all passing —

- the reference machine's twelve checks (okay-kernel's `TestSteps`: state 100 000 operations in constant stack,
  writer + state + reader, raise + finalizer + `handleWith`, choose all / first, the `shift0`/`$` laws, a generator,
  failure and `recover`, export multi-shot, the host answering in place, `k.fail`, `k.cancel` and `uncancelable`,
  import), plus a `k` resumed under other handlers and a check across implementations — a computation of the
  reference machine imported under OnCore's handlers and one of OnCore's under the reference's, the same results:
  14 on `OnCore.zero`, 14 on `OnCore.cont`;
- okay-kernel's TCK (`TckSuite`, nesting through the reference machine `Steps`): its 8 checks — among them a resume
  from another thread and 1 000 random programs with the reference's digest — on each machine.
