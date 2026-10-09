## zero-op-value-to-frame - an operation answered in place, its value straight into the frame bound to it — state 14.5 → 13.1 ns

- The machine's `Op` answered in place made a `Return` to carry the value to the next turn of its loop, where the
  frame bound to it took it. Now a `Then` on top takes it at once; anything else as before.
- Measured, Cont0, ns an operation, before / after: state 14.54 / 13.05 (214 → 198 B), `!` 14.56 / 12.99
  (198 → 182 B); answered, forwarded, general, reads, throws, writer unchanged. Against Cont's state (13.4):
  ahead now.
- Profiled, where Cont is still ahead: answered in place (4.97 against 5.45) — the same nodes and work on both, the
  difference the loop's code; throws (16.0 against 16.4) — a handler installed each time on both, Cont0's node, its
  metacontinuation's top and the path `Here` each an allocation.
