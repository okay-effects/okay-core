## choose-loop - `choose`'s clause: element by element, one closure each — choose 35.3 → 22.6 ns on Cont0, 29.1 → 24.4 on Cont

- The clause walked the elements by a fold that bound them all before the first ran: two closures and a frame an
  element, the frames as many as the elements. Now a loop by index: the next element's program made when the one
  before has ended, one closure an element. No iterator — the clause may run again, `choose` is multi-shot.
- `ret`: `Vector.empty :+ a`, not `Vector(a)` — that made an array of `A` first, its class tag looked up each time.
- Measured, ns an element (queens µs a run), before / after: choose Cont 29.1 / 24.4, Cont0 35.3 / 22.6; queens
  Cont 260 / 231, Cont0 176 / 145; allocation 394 → 338 B and 386 → 330 B. Cont0 now ahead in `choose` too.
- Under one more handler (chooseNested) Cont 28.8 / 21.6, Cont0 32.7 / 33.6: there the clause's loop is the
  machine's, the same work (330 B both) — slower by how the JIT compiles `deliver` (1513 bytes, never inlined)
  serving the clause's loop and each resumption's at once; at the top the kernel's `step` takes the first.
- Tried and not taken: the top run in the machine's loop under a delimiter of its own (branch `top-in-machine`) —
  choose 36.8 → 32.5 but generate, general, captureOnce 12–14% slower; a resumption at the top takes the same path
  in the kernel as in the machine, nothing to save there.
