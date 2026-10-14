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
`Return`, `flatMap` a `Bind`, `delay` a `Defer`. Nothing of the bridge's own runs a program, forwards a request or
unwinds a failure; the machine's loop does it all, in constant stack, `k` data.

**Operations at a place by key.** The kernel finds a handler at run time, by the operation's key, where the
operation runs — a `k` resumed under other handlers sees those. The one monad's rows have a place for that:
`In.At(ref)`, A PLACE BY KEY — beside `Here` and `There`, the paths the compiler finds. An operation performed at it
goes to the nearest handler in force that says the key is its own (`Handler.at`, the evidence a `Key`'s): the machine
walks the levels as it walks a path (`level`, `outE`), the place the same at every level — answered in place, an
abort, or out to the handler with the rest as `k`, the levels passed in it. So every operation of the kernel is
`Perform(op)` at `In.At(op.key())`, and every delimiter of the bridge is a handler of the one effect `Req` at its
own keys:

- a kernel handler, by its shape: `Answering` and `Translating` a `Linear` of the machine's — the rest given once,
  through it (`k(value(op))`; the translation outside, then `k`); `General` a `Deep`, `k` data; `Aborting` the
  machine's `Aborting`, its abort unwinding through the guards between. Each at its effect's key; the first two also
  at a shift0's key (a handler is a delimiter) and at the key the handlers in force are asked by;
- `$` a `Deep` at a shift0's key alone, its `ret` the `k` given;
- `recover`, `ensuring`, `uncancelable`: the machine's guards (`Recovering`, a finalizer, a mask) — no delimiter of
  the bridge's own.

The requests: `Perform` (a kernel operation), `Shift` (a shift0's body, to the nearest handler or `$`), `Env` (the
handlers in force asked for: each adds itself and asks on; the top adds the host).

**Failures, aborts, cancellation are the machine's.** An exception of the program's functions is a `Raise` at the
guarded call, unwound by the machine: `recover`'s guard, `ensuring`'s finalizer, an `Aborting` handler's level
(its abort pending there, the finalizers between run first), the top (`Step.Failed`). A `k` handed out is resumed
with a value, failed (`Fail`: the error raised inside its levels) or cancelled (the machine's cancel: under a mask
the same step given back, the cancel fired when the mask is left — a region that fails with a cancel waiting cancels
still). No signal read by an operation: nothing of the bridge's between an operation and its answer.

**The protocol.** Export: the program stepped (the kernel's `Step`): a request come to the top is the host's in
place if it answers its effect, else `Step.Yield` with the rest as the `Resume` — multi-shot, from any thread.
Import: the handlers in force asked for (`Env`), the computation started with them as its host in place, its yields
performed here as operations of this program, each under a guard of its `k` (`Driven`): its value to `k`, a failure
to `k.fail`, an abort or a cancellation crossing to `k.cancel` — its finalizers' operations performed here — then
the unwinding on.

**Trusted steps.** The bridge's two, each one method: `trust` (a value given where the program, built typed, expects
it — the protocol is untyped by design) and `inside` (the row is phantom to the machine: a body of the bridge under
one more delimiter of the bridge is the same program). The kernel's own two, in `okay.one.row`, the price of a
place found at run time: `Key.at` (one ref names one effect — as okay-kernel's `Key.of`) and `In.At.again` (the row
of a place by key is phantom: the same place in every row).

**The typed front, untouched.** `A ! E1 + E2`, paths, the standard effects: the same nodes, the same `level` and
`outE` with a case more each — `eval` untouched.

**Found on the way: an abort whose finalizer takes the aborting level into a `k`.** A finalizer between an abort
and its handler performing an operation that goes out past the handler's level and comes back resumed — the level
made again — lost the abort: it was pending on the level object. The abort's program now goes with the level into
the `Rest` and back (`Rest.Under.pending`), and the abort is known by it, not by the level. okay-core's own programs
had it too (`TestGuard`: an abort under `either` with a finalizer choosing under `all`).

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
— against `MixedBenchmark`, the same program in okay-core's own words. JDK 26, `-f 1 -wi 3 -i 5 -prof gc`, µs for
1000 orders, two rounds each, interleaved with the master of the day (c7672da), 2026-10-14:

| | before (`Deep` and requests) | a place by key, `Cont` a `Prog` |
|---|---|---|
| the kernel's program on `OnCore` | 450 / 452 (2.92 MB) | 294 (2.51 MB); 391 / 410 with the box still |
| okay-core's own program (`MixedBenchmark`), state / cell | 222 / 221, 231 / 222 (1.430 MB) | 231 / 231, 234 / 238 (1.445 MB) |
| the kernel's program on `Steps` (the reference), okay-java (okay-bench) | 535; 237–253 | |

The own program's +15 B an order is `Rest.Under`'s field for the abort pending (the bug above); its time within the
runs' noise. What is left between 294 and okay-java's 237: a kernel `Fn` adapted to a function at each bind and each
`k` handed to a clause (okay-java's `k` is the kernel's `Fn` itself), an `Answering` handler as a `Linear` (the rest
captured and resumed, not answered in place — the price of a shift0 to it), an `In.At` and a request an operation.
