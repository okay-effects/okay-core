## one-tailrecm - tailRecM, and recursion in constant stack checked on a million steps

- `Cont.tailRecM(s0)(step)` (the kernel), `okay.core.tailRecM` (the facade, over `Eff[R, A]`, exported by `okay.*`):
  cats' loop — `step` a program of the next state (`Left`) or the value (`Right`), any number of steps, each its
  effects; mutual recursion a loop whose state names the function.
- Tests: `TestRecursion` — `isEven`/`isOdd` by `defer` a million steps; with a state at each (two million
  operations); by `tailRecM`, a reader at each; a loop under a choice, each path its own. `TestFacadeRecursion` — the
  same in the facade: mutual recursion with an `ask` at each, `tailRecM` with a state at each. On the suites' 512k
  stack. okay-rs's `tests/recursion.rs` the same programs (`call!`, `tail_rec`).
