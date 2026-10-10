## zero-one-node - one `Node` for the delimiter nodes, one depth counter — −30 lines, chooseNested 38.6 → 34.5 µs

- `Handle`, `Icpt` and `Reset` extend one `Node`: its body and `inside` (the handlers in force within it: the handler
  added, the target masked, or as they are). The kernel and the machine have one case for them, `enterNode` replaces
  `enterHandle`, `enterIntercept` and `enterReset`.
- The machine counted two depths: `depth` for nodes entered nested (up to `Deep`, 32) and `n` for resumptions run
  nested in a shift's body (up to `Nested`, 64). Both are JVM frames; one counter, `depth`, bounds both.
- Measured, Cont0, all of MonadsBenchmark, before / after: chooseNested 38.60 / 34.52 µs, readsLocal 15.50 / 15.00,
  state 13.11 / 12.75; the rest within noise.
