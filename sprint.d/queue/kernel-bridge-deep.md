# kernel-bridge-deep

`KernelMixedBenchmark.orders backend=one` 440–459 µs against 231 on okay-core-zero (native): `OnCore`, the
kernel protocol's bridge, is a library over `Deep` — every delimiter of the bridge a `Deep` handler of one request
effect, `k` as data at every operation, exported through `Step`. What needs `k` as data is the export only (a
host answering later); a request the host answers in place could go through a `Passing` level. Follows
mixed-abort-road; docs/kernel.md the design.
