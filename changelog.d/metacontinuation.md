## metacontinuation - delimiters nested by recursion in constant stack, Cont0 and Ref

okay's rule (no unbounded stack recursion) checked by okay's recscan over okay-core (docs/stack-safety.md): a
delimiter at a body's head was entered by a call, a few frames a level, so a function installing a handler and
calling itself inside it overflowed at ~10 000 on both monads (`TestNestingOn`).

- The machine's METACONTINUATION (`Meta`): the delimiters a loop is inside a stack of its own — a delimiter or a
  resumption entered past `Deep` (32) nested is pushed, a program leaving one popped into the loop outside, a shift's
  body run in its delimiter's place. Up to `Deep` entered nested as before, a JVM frame each: the common paths pay
  nothing for it.
- The kernel's loop a metacontinuation of its own (`KMeta`): a `$` pushed, its body's head through its `ret` popped
  into the frames outside; `delimit` gone. `Ref`, the kernel alone, 100 000 deep too.
- A delimiter's body built when the machine enters it — a by-name kept in the node (`Handle.mk`, `Reset.mk`,
  `Icpt.mk`), no `Delay`; each delimiter's `Meta.Top` made once with it, not an object each entry.
- `TestNestingOn`: 100 000 handlers nested by recursion, 100 000 resets — run on Cont0 and Ref; Cont still overflows,
  pinned (its handler a fold made at once: next).
- Measured, Cont0, against the commit before (ns an operation): general 8.47 → 8.20, 118 B the same; an ask under
  `local` 16.3 → 15.2; answered 5.48 → 5.47; a shallow shift under a handler 142 → 149 ns a run (+5%); throwing 15.2 →
  16.5, 200 → 232 B — the by-name body a handler installed each operation there. The first cut, every entry pushed,
  was general +25%, local +38%: hence `Deep`.
- Then `Cont` the same: a handler and an interceptor nodes (`Fold`, `Icept`, `FoldK` for a body resumed — a clause's
  `k`, a forwarded operation's rest — no closure), bodies built when entered; one loop with a metacontinuation
  (`Meta`: `Top`, `InReset`, `InFold`, `InIcept`) in place of `loop0`/`loop`/`delimited`, `fold0`/`fold`,
  `intercepted`. The handlers in force being that stack, an operation whose handler further out answers in place is
  answered where it is, found by a walk of it — as `Cont0` does through `Ev`. 100 000 nested on Cont too: the limit
  is gone on every monad, `TestNestingOn` runs deep on all three.
- Measured, Cont, against the commit before (µs a 1 000 operations): across 1 / 4 / 16 handlers 11.7 / 22.8 / 93.5 →
  5.9 / 8.6 / 21.7; queens 216 → 166; state 15.0 → 14.0 (214 B the same); answered 5.55 → 6.04 (86 B the same);
  general 8.85 → 9.2–10.5, generate 14.6 → 16.5, throwing 13.9 → 16.9 (192 → 288 B: a `Fold`, its body's closure and
  an entry each install), choose 24.8 → 26.4, a shallow shift under a handler 123 → 152 ns. The first cut, without the
  walk and with a rest made for an operation answered in place, was 2× slower across handlers.
