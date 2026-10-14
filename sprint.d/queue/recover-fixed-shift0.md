# recover-fixed-shift0

`recover` at answers unchanged, `Cont[R, S, S, A]`, a shift0 passing it into its `k`. Today `recover` is on `!`
only (guard-at-answers, 2026-10-14): what it recovers with is a value, and a value sits at a level only where the
answer in is the answer out, `J <: O`; a shift0 taking the guard into its `k` re-types the level — its answer out
becomes the shift0's own (`Cont[T, I1, O1, X1]`), and `J <: O` says nothing of it. The way: a `fixed: (I =:= O) |
Null` on the `Shift0` node, set by `rows[R].shift0` when `X = O` (as `ensure` sets `none`); in `shifted`, at
`UnderGuard`, the level's `same: J <: O` and the node's `fixed: I = O` give `J <: I` — the evidence for the `Rest`;
`Recovering` then on `Cont[R, S, S, A]`, `guard` asking for `I <:< O`. Half a day.

What stays: a body with two shift0s compensating (`S → P`, then `P → S`) types, the first is not `fixed`, no
evidence, and after a resumption the recover is lost — the error goes on as with no guard. Not an exception, a
silence to document as the limit: under a `$` a value recovers only where the rest from the error to the level's
bottom moves no answer, and the compiler does not know that ahead.

Wanted when someone needs `recover` around a step holding a shift0; until then the guard outside the `$`,
`Atm.dollar(body)(ret).recover(…)`, is the way — a session fails, not a step. Noted 2026-10-14; not picked.
