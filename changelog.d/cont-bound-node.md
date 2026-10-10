## cont-bound-node - Cont: a delimiter bound entered at once, its value bound with no frame — throws −12%, choose −11%, captures under a handler −6–16%

- `Bind(node, f)`, a handler, a reset or an interceptor bound: the node entered at once, nested, and its value — the
  usual — given to `f`, no `Then` pushed; anything else, or past `Deep`, as before. Cont0's machine does so.
- The nested entry one function, `entered`, out of the loop: the four node cases call it.
- Tested against the four node classes, final — an interface test (`Delim`) cost the state loop 4%.
- Measured, Cont, before / after: throwing 16.36 / 14.47 µs (264 → 240 B), choose 25.78 / 23.08 (346 → 322 B),
  chooseNested 27.28 / 24.62, captureHandledShallow 171 / 145 ns, captureShallow 159 / 144, captureHandled
  531 / 500, general 9.90 / 9.40; state 12.88 / 12.96, the rest within noise.
