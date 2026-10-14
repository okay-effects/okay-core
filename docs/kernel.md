# okay-core as an implementation of okay-kernel

okay-kernel is a protocol: what lets implementations of effects and delimited control work with each other in one
process (okay-kernel's `docs/design.md`). okay-core implements it: the core depends on okay-kernel's API
(`dev.okay:okay-kernel-api` on the JVM, a jar of interfaces and records; its Scala mirror `okay-kernel-scala` on
Scala.js and Native) and the one monad's machine runs the kernel's programs. `okay.core.kernel.OnCore` is
okay-kernel's `Effects` and `Control` on it — okay-kernel's own `one/` module (`OnOne`), the same code.

```scala
import okay.core.kernel.OnCore
import okay.kernel.std.Handlers

OnCore.computation(program, row).start(host)   // export: run here, stepping out where it needs what it cannot answer
OnCore.foreign(otherComputation)               // import: another implementation's program, answered here
```

## How

**A kernel program is a program of the one monad** over one row, `Req +: Pure` — `P`, a `Cont` in a box: `pure` a
`Return`, `flatMap` a `Bind`, `delay` a `Defer`. Nothing of the bridge's own runs a program; the machine's loop does,
in constant stack, `k` data.

**Operations by identity, as requests.** The kernel finds a handler at run time, by the operation's key, where the
operation runs — a `k` resumed under other handlers sees those. So every operation of the kernel is a REQUEST to the
nearest delimiter of the bridge (`Req`: `Perform`, `Unwind`, `Mark`, `Env`, `Shift`), and every delimiter of the
bridge — a kernel handler, a `$`, `recover`, `ensuring`, `uncancelable` — is a `Deep` handler of `Req` over the same
row outside: it takes the requests it concerns and FORWARDS the rest outward, `perform(request).flatMap(k)`, its `k`
the rest under it with itself put back (the kernel's `k` is data: the levels passed, put back when resumed).

**The handler shapes.** `Answering` resumes `k` with `value(op)`; `Aborting` resumes it with an ABORT, a signal
unwinding from where the operation was to the handler, its finalizers run on the way; `Translating` runs its program
in the handler's place, the rest resumed with its value; `General` runs the clause with `k` deep; a return
`Returning`'s `answer`, else `General`'s `ret`, else the value.

**Control** — the API's `Control`, apart from `Effects`; `OnCore` implements both. `$` is a delimiter whose return
is `k`; `shift0` a request only a handler's delimiter or a `$`'s takes — the bridge's others forward it.

**Failures, aborts, cancellation** are requests too — `Unwind`, `k` dropped — to the nearest delimiter taking them:
`recover` (a failure), `ensuring` (any: the finalizer, then on), an `Aborting` handler (its own abort), a region (a
cancellation pending there turns any into a cancellation), the top (the step). An exception of the program's
functions is caught where the bridge calls it and unwinds from there — never a `Raise` of the machine's, which the
bridge's delimiters would not see. What an operation is answered can be a SIGNAL (`k.fail`, `k.cancel`, an abort, a
value given inside a region a cancellation waits for): the operation reads its value and unwinds from where it was
performed.

**The protocol.** Export: the program stepped (the kernel's `Step`): a request come to the top is the host's in
place if it answers its effect, else `Step.Yield` with the rest as the `Resume` — multi-shot, from any thread.
Import: the handlers in force asked for (`Env`, each delimiter adding itself on the way out), the computation
started with them as its host in place, its yields performed here as operations of this program; an abort or a
cancellation crossing it cancels it, its finalizers' operations answered here.

**Two trusted steps**, each one method: `trust` (a value given where the program, built typed, expects it — the
protocol is untyped by design) and `inside` (the row is phantom to the machine: a body of the bridge under one more
delimiter of the bridge is the same program, its operations of the nearest delimiter, `Here`, as before).

**The typed front, untouched.** `A ! E1 + E2`, paths, the standard effects: the same nodes; the bridge is a library
over the kernel's `Deep`, nothing of it in the machine.

**Not done: okay-core's own operations as the kernel's vocabulary.** okay-core's `Get`, `Ask`, `Tell`… find their
handler by path, and carry no `Type`; the two worlds meet at the border (`computation`, `foreign`) and by the
kernel's own vocabulary inside kernel programs; a typed program is not exported as it is.

## What is exact

The reference machine's semantics (okay-kernel's `Steps`), checked by okay-kernel's TCK (16 checks, the 1 000 random
programs' digest): the standard handlers, `shift0`/`$` (through `recover` and `ensuring`, handlers as delimiters,
the body outside its delimiter), failures and `recover` (a `Returning`'s answer's too), finalizers after a value, a
failure, an abort and a cancellation, `uncancelable` regions (one that fails with a cancellation waiting cancels),
export with multi-shot `k` from any thread, the host answering in place, `k.fail`, `k.cancel`, import (a foreign
program crossed by an abort is cancelled, its finalizers' operations answered here), a `k` resumed under other
handlers, effects from effects (`Translating` outside, a hidden key inside, interceptors nested), across
implementations (okay-kernel's `integration/`: six implementations in one process, 96 checks).

## Tests

- `src/test/scala/okay/core/kernel/TestNative.scala` — on the JVM, Scala.js and Scala Native: a kernel program is the
  machine's nodes; state 100 000 deep; reader, writer, a raise with its finalizer, choose; `shift0`/`$` through
  `recover`; export, multi-shot `k`, `k.cancel`; an import — 6.
- `kernel/` (the JVM's, tests only): the reference machine's checks, a `k` under other handlers, a round trip
  through the reference machine (`Steps`) — 14; okay-kernel's TCK (`TckSuite`, nesting through `Steps`) — 16. 30.

## Measured

`KernelMixedBenchmark` (src/jmh): okay-kernel's mixed program, written once over the kernel's `Effects`, on `OnCore`
and on okay-kernel's reference machine — against `MixedBenchmark`, the same program in okay-core's own words. JDK 26,
`-f 2 -wi 3 -i 5`, µs for 1000 orders (bench.d/2026-10-13T19-45_one-monad-5486b17.csv):

| | µs |
|---|---|
| okay-core's own program (`MixedBenchmark`), the total a state / a cell | 162 / 214 |
| the kernel's program on `OnCore`, the first cut | 419 |
| the kernel's program on `Steps` | 547 |
| before the one monad (bench.d/2026-10-10): own 127, native 231–255, `Steps` 523 | |

The bridge is the first cut: every kernel handler a `Deep` clause and a level of `k`, every operation a request and
a read of its answer. What the native path on the two monads went through (474 → 231) is ahead of it.
