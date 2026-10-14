# mixed-abort-road

`MixedBenchmark.orders` (state) 221 µs since cec1b6a, 159 on okay-one c7af924 (docs/one.md §8, the account in full).
Not the facade's handlers: in this program `k` is taken by `first` (0–3 calls an order) and `throws` (`Aborting`)
only, and both need it; `ask`, `modify`, `tell` answer in place. What is left to try, each an A/B on the lane:
the abort road — an `Aborting` clause per order raises `AbortTo` (an exception made, `unwind`, `guardsTo` at every
abort) where a value could go straight to the handler's frames when no guard is between (`guardsTo` says so, but
after the exception is made); the levels and rests on the `Deep` road (20% of the allocation: `Meta.Under`,
`Rest.Under`, `Resume`, `HandleK`, `Frame.End`, `Defer`), a `Resume` with no `Rest` object for a `k` called once
at once; the facade's fitting (10%: `handleIn`'s closure, `Needs`, `Paths.One`, `Removed.Head`, `Req`, `In.Here`),
a `Needs` and its paths made once per handler site rather than per fitting. Tried and dropped already (§8): a
look-up with no `Found` on a miss, the loop's arms reordered. Co-ordinate with the session on one.md §8 first.
