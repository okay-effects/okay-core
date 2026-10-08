## needs-as-rows - the effects a program needs: one, or a row ending in Pure; Sub, an inclusion of rows

- `+:[E[_], T <: Row]`: a row's tail is a row, so several effects end in
  `Pure` — `Int ! Reader % Int +: Writer % String +: Pure`; one stays sugar,
  `Int ! Reader % Int`. A list not ending in `Pure` does not compile (tested).
- `Paths` is gone: `Sub[Es, R]`, the row `Es` in the row `R`, two cases, both
  total with no annotation — the last effect (`E0 +: Pure`, where a path can
  only be `Here`: the compiler sees `In[E, Pure]` has no value) and an effect
  in front of more. `IfPure` and `vacuous` are gone.
- `Req[E, Es]` keeps two cases, `Only` (one effect) and `Path(in)` (in a row),
  chosen by the compiler's search by the kind of `Es`. The two matches on it —
  one in `Needs.one`, one in `Needs.row` — keep `@unchecked`: each excludes
  the case of the other kind, which the exhaustivity checker cannot tell
  apart (tried: a match on `Req[E, E0]` with `E0` an effect warns of `Path`).
  The user's choice: the sugar, with those annotations.
- `Needs.Forward[Es, C <: Effects & Singleton](c)`: every operation `c`'s,
  the singleton type so that `R` is the given's `c.R`.
- Measured, full run: allocation unchanged in all 34. Back to back against
  75a3ddf (two rounds): equal within noise except tailcallHandled, 54.5 /
  54.8 µs against 67.1 / 66.3 µs — from final-tagless-effects-control, not
  from here (67.1 there too); stateGetPutBang 17.1 against 18.0–18.5 µs,
  most of that step's +16% won back. Cont.flatMap made smaller (61 → 32 bytes
  of bytecode, to be inlined) did not move tailcallHandled, and was reverted.
