## kernel-bridge-deep - a place by key at run time: the kernel's protocol native

- `In.At(ref)`, A PLACE BY KEY, a case of `In` beside the paths: an operation performed at it goes to the nearest
  handler in force that says the key is its own (`Handler.at`, the evidence a `Key[E]`'s), the machine walking the
  levels as it walks a path (`level`, `outE`: a case each; `eval` untouched). Two trusted steps, in `okay.one.row`:
  `Key.at` (one ref names one effect) and `In.At.again` (the row of a place by key is phantom).
- `OnCore` rewritten on it: a kernel handler a handler of the machine's by its shape (`Answering`/`Translating` a
  `Linear`, `General` a `Deep`, `Aborting` the machine's), `$` a `Deep` at a shift0's key, the guards the machine's,
  failures and cancellation the machine's, nothing forwarded; `Cont` is a kernel `Prog` — nothing boxed.
- Two bugs of the machine found by okay-kernel's TCK on the native route, both of okay-core's own programs too:
  an abort whose finalizer's operation took the aborting level into a `k` lost the abort (pending on the level
  object; now with the level into the `Rest` and back, the abort known by its program); a masked region left by an
  error with a cancel waiting recovered instead of cancelling. `TestGuard` +2, both failing on the machine before.
- Measured (docs/kernel.md): the kernel's program on `OnCore` 450 → 294 µs (2.92 → 2.51 MB); the own program within
  noise, +15 B an order.
