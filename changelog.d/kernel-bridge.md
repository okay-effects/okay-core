# kernel-bridge — okay-core as an implementation of okay-kernel

okay-kernel became a protocol (`Effects`, `Computation`, `Step`, `Resume`, `Answers`, the handler shapes, `Direct`),
and its old adapter (`legacy/okaycore/OnCore.scala`, on the old `Monad`) came back here as a module of its own,
`kernel/` (`okay-core-kernel`, the JVM's, depending on `okay-kernel-api`): `OnCore.zero` and `OnCore.cont`.

From the old adapter: the kernel's operations one okay-core effect, `Dyn`, their paths found by the effect's identity;
`Answering` answered in place as okay-core's own, `Returning` okay-core's `ret`, a `General` return flattened. New:

- a kernel `Prog` is a node of the bridge, translated where it is placed — so `shift0`'s body, written with the same
  `Effects` as the code around it, runs one delimiter out and is translated there;
- `shift0` and `$` the bridge's own (okay-core's control would stop at the bridge's own delimiters);
- failures, aborts and cancellation an effect of the bridge, `Unwind`, signals read where an operation was
  performed; `uncancelable` a region a pending cancellation is marked to;
- export with the host answering in place and okay-core's `k` as the protocol's multi-shot `Resume`; import;
- a `k` resumed under other handlers than it was made under runs under forwarders that perform again what reaches
  them (found by a check: a `shift0`'s `k` used further out).

Tests: the reference machine's twelve checks, a `k` under other handlers, and a round trip through the reference
machine (`Steps`), on both machines; okay-kernel's TCK on both — 44, all green. docs/kernel.md says what is exact
and what is approximated.
