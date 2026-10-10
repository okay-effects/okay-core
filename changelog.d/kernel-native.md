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
