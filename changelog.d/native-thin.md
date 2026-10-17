# native-thin — control's own `$` and shift0 on the thin path

Measured first: the machine's own `$`/`shift0` was the slow face of delimited control — 8 resumptions of a shallow
`k`, 0.256 µs and 1736 B against std's `shift0` 0.066 µs and 904 B. Merging the two faces into one machine level
does not type without casts: a `$` moves the answer, its `k` a program with answers of its own, a handler's level
has none (docs/one.md's two faces, each where its typing is free). What is one now is the path:

- `Shift0K(at, f, g)`: a shift0 and its rest, as `Inject` is an operation's — `shift0(…).map(f)` and `.flatMap(f)`
  make one, a map of it fused into its `MapK`; the machine gives `g` to the capture as the innermost of `k`, no frame.
  `TestReference` reads it as the shift0 bound to `g`.
- `eval`: a `$` resumed and bound, its frames none (`k(x).flatMap(f)`), the std delimiter's own fast case for it —
  the rest a value, the `$`'s `ret` its answer, the program bound to `f` there; no `$` entered (`resumedDollar`).
  A resume of a `$` moved into eval's fast case was tried and not kept: no change.

KernelBenchmark's `captureControlWords`/`…Shallow`: the native lanes written with the words (`map`, `flatMap`), as
programs are; the raw-node lanes kept. Measured, JDK 26: captureControlWordsShallow 0.285 → 0.105 µs (1704 → 976 B),
captureControlWords 0.645 → 0.462; under `-wm BULK`, the variant with the eval case against the one without:
native words −38%/−23%, std capture0Shallow −19%, captureShallow −12%, state −3% — the case kept. Against master
under another BULK set the std lanes read +3–6% — that set's profile, not the work (master itself moves as much).

Rebased on c1f75fc (`Cont.map` small, its fusions in `mapped` — the shift0 cases there): `-wm BULK` on one lane set,
that master → this: capture 0.708 → 0.716, captureShallow 0.257 → 0.272, generate 35.4 → 35.7, state, stateSmall,
choose within ±2.5%, captureControl 0.819 → 0.762. The fast case kept in eval: out of it, in `resumed`, was no
better (generate +4%, captureShallow +12%). A bulk run over different lane sets is not a comparison: each fork's
profile is its set's (generate read +19% so, master's own runs 26.9–34.6).
