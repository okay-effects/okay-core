## kernel-control - okay-kernel's `Control` apart from `Effects`: `OnCore` implements both

- The API's `shift0` and `$` moved out of `Effects` into `Control`, an interface of its own an implementation has or
  not (`Control.of(e)`); `OnCore` implements both. Why apart (okay-kernel's design.md §3a): a program of effects carries
  no answer types, which is what lets it run under any handlers; answer-type modification is a front's own, beside.
- The tests ask `KControl.of(e)` for it (the API's `Control` aliased: okay-core's `Control` is the front's).
- kernel/ 60, core JVM 455 green; okay-kernel's integration 80 — and, with an okay-java published without `Control`,
  its control checks skipped and the rest green: the optional path works.
