# removed-again (2026-10-14)

`Removed.Again[E, R](at: In[E, R]): Removed[E, R, R]` — a handler of `E` at any place in the row, the row kept: `E`
taken from `at` and left there, so the handler's own clauses' operations of `E` go to the handler of `E` outside — an
interceptor's (`local`, `handleWith` on an effect anywhere in a row). In the kernel's own way: a `View` whose
relocation compares a place with `at` (`In.same`: two places in one row the same are one effect's — a row is a list,
a place names its effect — by the compiler's reading of the two paths, no cast); the effect's places become the
head, every other place is past it as it was; a shift0's path one deeper. `Removed.again(at)`, by hand — not a given.
`okay.core.Intercept.at`/`by` are a `Deep` handler over it now; the stepping road (a run of the machine an operation)
is gone. `TestAgain` (the kernel), `TestIntercept`, tofu's suite, Scala 2's: as before.
