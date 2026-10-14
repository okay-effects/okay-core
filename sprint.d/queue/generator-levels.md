# generator-levels

A generator pulled / mapped: 20.6 / 24.1 µs against 14.9 / 16.6 on okay-core-zero (+40%); `captureDeep` 8.4 µs,
256 maps under a reset. The cost: a `k` as data put back one level at a time at each resumption (`resumed`,
`Rest` by `Rest`), and the `Lifted` level at every program of effects bound at an answer. To try: a chain of
levels put back as one (a `Rest` holding the levels inside it flattened, one `Meta` rebuilt), and for a `k`
resumed at once in the clause that got it — the generator's case — no `Rest` made: the clause's `k` a frame.
Two to three days; the generator lanes and `captureDeep` the measure.

2026-10-14: the run's fixed cost per element measured — 336 B an element, of it the run's own 104 (`Meta.Top` 32,
`Eff` 24, `Frame.End` 16, `Done` 16, `Out.None` 16); the machine's `Rest.Under`, `Meta.Under`, `Resume`, `Return`
the `k`'s, the program's `Step.Next`, its lambda, the `Long`, `Yield`, `InjectHere`. Landed: no `Eff` at the entry
(changelog.d/run-entry-no-eff.md). Dropped: a run's own top (`Meta.Run`), slower. Left: `Done` (a result slot on
the top needs a `Z` before there is one), `Out.None` (invariant in its answer: one per run), `Frame.End` (invariant);
the levels put back as one, for `k`s of more than one level (not the generator's: one).
