# queens-deep-road

`queens` (92 solutions of 8 queens by `choose`, `k` called once per column tried): 209 µs against 160 on
okay-core-zero (+31%); `collect` 13.4 against 11.1 (+20%). The `Deep` road of mixed-abort-road (`Meta.Under`,
`Rest.Under`, `Resume`, `HandleK`) with `k` many times. Measure it after mixed-abort-road lands, on its own lane,
before trying anything here: likely the same cause.
