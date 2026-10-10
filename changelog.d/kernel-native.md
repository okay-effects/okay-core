# kernel-native — okay-core implements okay-kernel's API directly

okay-kernel 0.2 dropped `Direct` and opened its vocabulary: implementations implement the API, no wrapper. okay-core
now depends on `okay-kernel-api` (the JVM's jar; on JS and Native its Scala mirror, `okay-kernel-scala`) and its
machines run the kernel's programs themselves. The bridge (`kernel/`, a translation of the kernel's nodes into
okay-core's at each place) is gone; `kernel/` holds the TCK and the checks against the reference machine.

- `Cont0` and `Cont` are `okay.kernel.Prog`s: `OnCore.zero` / `.cont` build them directly.
- Kernel operations carry no path; the machines find their handler by the effect's identity as they run: new nodes
  `KOp`/`KOpK` (Cont0) and `KInject`/`KSuspend` (Cont), matched after every typed node; answered in place by the
  nearest kernel handler in force (the machines' `Ev`, where kernel handlers sit as okay-core `Answering`s), else
  routed to the delimiters (`kernelRoute`, `kernelPerformed`, Cont0's definitions `read` / `inside`).
- The shapes, `shift0`/`$`, `recover`, `ensuring`, `uncancelable`, the top: handler nodes with a `KHandler` each
  (okay.core.kernel), as the bridge had them, minus the context: a `k` resumed anywhere finds the handlers where it
  runs, so the bridge's rebasing and its one approximation are gone.
- okay-core's own typed operations do not implement the kernel's vocabulary (they carry no `Type`; a `TypeOf` at
  every `get`/`ask` would change the typed front): docs/kernel.md.

Tests: TestNative (6 on each machine, every platform), kernel/ 54 (TCK 13 and 14 own on each machine); JVM 426 → 438,
JS 330 → 342, Native 407 → 419, the rest unchanged. `KernelMixedBenchmark` (src/jmh): okay-kernel's mixed program
written once over `Effects`, against `MixedBenchmark`.

## Measured

JDK 26, JMH `-f 2 -wi 5 -i 5 -prof gc`, against the bridge's base (2fa6876, worktree), µs (cont / zero):

| | base | native |
|---|---|---|
| MixedBenchmark.orders | 142.3 / 127.4 | 142.3 / 124.5 |
| choose | 26.0 / 24.1 | 26.3 / 24.1 |
| general | 10.0 / 8.3 | 9.9 / 8.1 |
| reads | 5.1 / 4.7 | 4.8 / 4.9 |
| state | 14.1 / 12.9 | 14.6 / 13.6 (rerun; the first 32.6 ± 32 was noise) |
| stateBang | 13.8 / 13.1 | 14.1 / 13.2 (rerun) |
| stateProgram | 23.8 / 23.1 | 25.4 / 23.6 |
| stateReaderBang | 20.3 / 17.7 | 20.7 / 18.8 |
| throwing | 16.4 / 14.1 | 16.6 / 14.5 (rerun) |

The typed front unchanged — the allocations byte for byte the same, the times within a few per cent. The mixed
program written once over the kernel's `Effects` (KernelMixedBenchmark): 316 / 348 — slower than the translating
bridge with `Direct` (219 / 248, okay-kernel's bench at 4ee71a0): the native path is to be made fast (a lookup by
Type each operation, no cache; a `try` a bind; a signal check an operation).
