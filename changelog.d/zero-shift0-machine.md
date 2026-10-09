## zero-shift0-machine - okay.zero: effects and control on shift0 and $, a machine to run them

- `Cont0[I, O, A]`, Materzok–Biernacki's shift0 and `$` with the answers modified: four nodes (`Return`, `Bind`,
  `Shift0`, `Dollar`), open as a monad of its own; under a delimiter the answers are programs of the level outside —
  the levels in the types, no cast. Every node Effects and Control build is a shift0 program: an operation
  `shift0(k => Req(op, k))`, a handler a `$` whose answers it reads (`readHand`), a reset one passing every
  operation on (`passAll`).
- The machine (Machine.scala) runs them: each handler's and reset's body in a loop of its own, an operation answered
  in place by the handler at its path if it answers in place — across any handlers and resets between, the
  handlers in force known at run time (`Ev`); a `shift` and a `shift0` at their delimiter; a resumption entered with
  the value going through its frames, nested in the shift body's loop up to 64 deep. What it does not take goes to
  the delimiter's definition (`bail`: `handleRead`, `resetRead`).
- A shift directly under a handler: `k` is the rest WITH the handler (Danvy–Filinski's, the handler the delimiter) —
  a raise in `k` is that `k`'s `Left`; on Cont the rest under the handler's `ret` only. Pinned in TestMonads.
- Measured against master's Cont (the same programs, MonadsBenchmark): an operation across 1 / 16 handlers 5.5 / 17
  ns against 11.7 / 94; the general clause 7.5 against 8.8; state, writer, collect, deep captures faster; choose
  32 against 29 µs, a shallow shift under a handler 139 against 122 ns slower.
