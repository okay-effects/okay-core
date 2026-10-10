## cont-faster - Cont's loop in Cont0's shape

The innermost delimiter a parameter of the loop — the node itself (`Reset`, `Fold`, `FoldK`, `Icept`; `Rs`, `Hd`, `Ic`
made for a shift's body; `TopOf`) — not an entry pushed; one entered nested up to `Deep` (32), its leaving program
the nested call's result (`P <:< Z`, `<:<.refl`, no object), pushed past it (`Meta.Under`); the handlers in force a
list of their own (`Ev`, as Cont0's: in-place answering across handlers through nested entries too); an answering
handler's `Value` found once, kept in its node (`an`).

A/B, the commit before against this, two forks each, µs a thousand (Cont): across 16 handlers 22.3 → 17.4, choose
28.2 → 24.5, capture 524 → 500 ns, general 10.2 → 10.0 (166 → 134 B), generate 16.7 → 17.2 (272 → 240 B), throwing
16.1 → 16.4 (288 → 264 B), state 14.6 → 14.8, queens 170 → 176 (1059 → 993 B), answered 4.92 → 5.53. Against the
commit before the metacontinuation (051d8a0) what is left above it: throwing (13.9) and generate (14.6) — a handler
installed or resumed each operation is a node and its body's closure now, where a fold made at once was none; a
shallow shift under a handler (123 → 165 ns, a delimiter made for its body).
