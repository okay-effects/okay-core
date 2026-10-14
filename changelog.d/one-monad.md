# one-monad (landed 2026-10-14)

okay-core on one monad, merged to master: the kernel okay-one's (`okay.one`, imported as it is by
`scripts/import-one.sh` — at 71ddc6a, `Cont[R, I, O, A]`, the machine with `k` as data), `okay.core` the facade over
it (`A ! E1 + E2` by `Needs`/`Place`, `Path` found from the place first, `Op`, the handler sugar, control's two
faces, `RowEffects[Eff]`), one std, `OnCore` the kernel bridge over `Deep`, async over `Step`, cats and tofu over
`Eff`, Scala 2 through `Ops`, JS and Native cross-built; the two monads of before (`okay.zero`, `okay.cont`) and
their references deleted. The steps and their gates: docs/one.md §5, the step entries beside this one
(one-monad-step*.md). Open at the landing: §4 H — the mixed program 220 against okay-core-zero's 130 (the kernel's
Deep road since okay-one cec1b6a, and the facade's own fitting per order), captures 2×, generate +40%, the bridge
459 against 231; docs/one.md §8.
