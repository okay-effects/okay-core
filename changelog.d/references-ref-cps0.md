## references-ref-cps0 - two references in the tests: Ref and Cps0

- `Ref` (tests, okay.zero): Cont0 by the kernel alone — a handler and a reset their shift0 programs read
  (`handleRead`, `resetRead`), no machine; what `okay.zero.At` runs fast, slow (2–25×) and the definition.
- `Cps0` (tests, okay.core): shift0 and `$` in CPS from the textbook, a program a function `(A => I) => O`, no code of
  Cont nor of Cont0; resumptions lazy, as the interface's are. Not stack safe: `BackendSuite` runs its tests on a
  thread of a 2 GB stack — all of them, none skipped.
- Cont, Cont0, Ref and Cps0 agree on every suite written once; where Cont0's machine differs by design (a raise in `k`
  directly under a handler) TestMonads says so.
