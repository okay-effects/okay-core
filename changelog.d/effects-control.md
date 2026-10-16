# effects-control — reset and shift primitives of okay's interface

`RowEffects[M]` (okay's `Effects[M]`) has delimited control beside `pure`, `perform`, `flatMap`: `reset[S, T](body)`,
`shiftAt`/`shift0At` by path, and the words `shift`/`shift0` — a shift not yet at a row, its path found where it is
bound, as `effect(op)`'s. The answer fixed, an effect of the row (`Shifts[S, T]`), under any handlers, in any order.
For `Eff` they are the machine's own delimiter (shift-thin): std `reset` a `Delimiting`, a shift's body on its
level, each `k(x)` one node. A program written once over the interface (`TestRowControl`: `k` twice with an
operation of the row outside in the rest, shift0, a shift in a shift's body, 100 000 shifts in constant stack).
