- [ ] needs-as-rows — the effects a program needs, `Es` in `A ! Es`, a row:
      several always end in `+: Pure` (`Int ! Reader % Int +: Writer % String
      +: Pure`), one stays sugar (`Int ! Reader % Int`). Then `Req` is `In`
      and `Paths` an inclusion of rows; `IfPure`, `vacuous` and the
      `@unchecked`s go. (A match type, `Under[Es, R, S, A]` unfolding into
      nested context functions, was tried 2026-10-08 and fails: the
      dependent `c.R` is widened to `? <: Row` in the subtype check.)
