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

## The native path made fast, round 1

Profiled (JMH `-prof gc`, async-profiler, JDK 26): of the mixed program written once over `Effects`, the program's
own code and its lambdas' captures were half the time; okay-core's machines and `OnCore` about a third. Changed, the
kernel's path only:

- `guard` and `in` inline: no thunk a bind;
- a bind (and a map) over a kernel operation one node, `KOpK`/`KSuspend` with its value read first (`checked`), as
  Cont0's typed `bind` fuses an `Op` — no `Bind`, no frame an operation;
- the walk of the handlers in force by fields (`KHandler.key eq` the effect, `decide`), the operation's `effect()`
  asked once, not a virtual `accepts` and an interface call a handler;
- the kernel's nodes one case each loop (`KNode` in Cont0; Cont's last case): Cont's `run` 2991 → 2680 bytes (2515
  before the kernel), Cont0's `deliver` 3347 → 3035;
- a failure of a `Returning`'s answer, or of a map's function, a signal VALUE raised at the next bind or handler's
  return (`recover` reads its body's value; `ensuring`, a region, `$`, the top read theirs): no `Bind` and `Return`
  a handler — and a map Cont's own `Map` node; on Cont0 a bind, measured faster there (250 against 337 µs);
- the machine's nodes typed `Prog[Any]`, a user's `Fn`'s result untyped: no `checkcast` to the kernel's interface a
  call.

JDK 26, `-f 2 -wi 5 -i 5 -prof gc`, µs (cont / zero), B an order:

| | before | now |
|---|---|---|
| KernelMixedBenchmark.orders | 316 / 348 | 229.1 / 250.5 (2122 / 1934 B) |
| the reference machine `Steps`, the same program | | 522.9 (2414 B) |
| MixedBenchmark.orders (typed) | 142.3 / 124.5 | 141.9 / 127.2 |
| MonadsBenchmark choose, general, reads | 26.3 / 24.1, 9.9 / 8.1, 4.8 / 4.9 | 26.4 / 24.1, 9.9 / 8.1, 4.6 / 4.6 |
| state, stateBang, stateProgram | 14.6 / 13.6, 14.1 / 13.2, 25.4 / 23.6 | 14.1 / 12.8, 13.7 / 13.1, 23.8 / 23.2 |
| stateReaderBang, throwing | 20.7 / 18.8, 16.6 / 14.5 | 20.1 / 17.8, 16.3 / 13.8 |

The typed front unchanged. The kernel's program at the bridge's (219 / 248) — and bimodal by build: a change the
JIT takes differently moves one machine by ±35% (cont 246 or 318, zero 250 or 340 across builds), each build steady
across forks.

## Round 2: a kernel handler one object

A kernel handler's clauses (`KHandler`) are its node's body thunk and its return function too (`Function0`,
`Function1`: Cont's `Fold` takes a `ret`), its return's shape fields of its own — no closure for its body, its
return, Cont's `ret` a handler. 100 KB (cont), 69 KB (zero) less a run: 2022 / 1864 B an order; the time the same,
231.1 / 255.0 µs (`-f 2`) — allocation is not what bounds it. The kernel's files only: the typed loops' bytecode the
same. Profiled after it (zero, inclusive): the kernel's standard handlers' own code 14%, interface dispatch (itable
stubs) 11%, lambda captures the JIT did not inline (`allocateInstance`, in the program's own lambdas) 7%; the walk of
the handlers in force 5%, routing an operation to a clause 9% with the clause's own code, building handlers 6%.

## The bridge, measured on the same harness

The bridge's 219 / 248 µs came from okay-kernel's harness (bench/, okay-kernel 4ee71a0); okay-core's
`KernelMixedBenchmark` — the same program — run on the bridge itself (a worktree, the module `kernel` given JMH), JDK
26, `-f 2 -wi 5 -i 5 -prof gc`, µs (cont / zero), B an order:

| | µs | B an order |
|---|---|---|
| the bridge with `Direct` (63e3438) | 474.0 / 457.7 | 4003 / 3709 |
| the bridge at its last (2fa6876) | 489.9 / 470.5 | 4103 / 3781 |
| native, first (7596437) | 316 / 348 | 2395 / 2089 |
| native now | 231.1 / 255.0 | 2022 / 1864 |
| the reference machine `Steps` | 522.9 | 2414 |

On one harness the native path is half the bridge's time and half its allocation. Between the two harnesses the same
code differs ~2× (JIT profiles; the programs' lambdas), so a number is compared only with one from its own harness.
