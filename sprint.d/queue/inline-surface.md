# inline-surface

`flatMap`, `handle`, `guard`, `ensure`, `uncancelable`, `Op.flatMap` are `inline` with `summonFrom`: the answers
met and a handler's kind decided where the code is written. Each inline expands into user code and into every
test. Measure: compile time of `words`' and the kernel's suites with and without (`-Yprofile-trace`), the size of
user classes; keep inline where `Lifts` or the kind needs it (`flatMap`, `handle`, `guard`), consider a plain
`ensure`/`uncancelable` with the evidence by a given instead if the difference shows. An hour to measure.
