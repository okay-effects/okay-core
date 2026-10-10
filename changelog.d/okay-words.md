# okay-words — okay's program code on `import okay.*`

- `effect(op)` and `op.perform`: one's own operation a program, okay's words.
- `A ! F` with `F` an effect not yet known — alone or joined, `A ! Choose + F`, okay's "and whatever else": `!` is
  `Needs[F]` itself, no match type. `Req` finds an effect among those joined (`Left`, `Right` of a `+`, the left
  first), `Paths.Both` keeps each side's paths, `Paths.Keep.both` an instance's context of them (`NeedyBoth` in
  `Cont` and `Cont0`). `Needed` and `Append` gone. `F` is inferred at the call: okay-agent's
  `samples[A, F[_]](n)(gen: A ! Choose + F)` and okay-std's runner shape `A ! Reader % R + F => A ! F` compile and
  run as written there (`TestFacade`).
- docs/okay.md: okay's words against okay-core's — the same, renamed, absent (the last runner: okay's `F` is `Nothing`
  by `Free`'s variance, here the standard handler; `A ! Pure`, `p.value`: `run(p)`).

Measured, JDK 26, A/B against the match type (`-f 2 -wi 5 -i 5 -prof gc`), the new `stateReaderBang` — two effects
joined, each path found through `+` per call: cont 20.24 → 20.34 µs, zero 17.75 → 17.70 µs, +16 B in 246 KB;
`stateBang`, `stateHoisted` unchanged. The full run is the sprint's wrap-up.
