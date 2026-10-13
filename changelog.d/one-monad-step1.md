## one-monad, step 1 - okay-one's kernel imported as the package `okay.one`, beside the two monads

- `scripts/import-one.sh`: okay-one's `src/main` and `src/test` copied as they are (the package `okay.one`: monad,
  rows, handler forms, the machine of one `Meta`, `Step`, std with `Shift % S`, `TestReference`), the commit written to
  `docs/one-imported.txt`. Mechanical, re-run on each commit of okay-one until its machine settles (docs/one.md §5 step
  1; its gate §4 H, the lanes, still open there).
- Gate of the step here: okay-one's suites green in this build — 66, among the JVM's 521 (455 before); the package
  compiles on Scala.js and Scala Native as it is.
- Nothing of `okay.core`, `okay.cont`, `okay.zero` touched: step 2 (the facade over `okay.one`) is next on this branch.
