# okay's code on okay-core

okay-core is meant to become okay's kernel. Two ways in, both checked here:

1. **okay's program code compiles on `import okay.*`**: the words below are the same;
2. **okay-core is an instance of okay's `Effects[M]`**: programs with their row in their type, an operation by its
   path, `handle` taking an effect off wherever it is (`okay.core.RowEffects`, see below).

## The words

| okay | okay-core, `import okay.*` | |
|---|---|---|
| `A ! State % Int` | `A ! State % Int` | the same |
| `A ! Choose + F`, `F` the rest | `A ! Choose + F` | the same, `F` inferred at the call (`TestFacade`) |
| `effect(op)`, `op.perform` | `effect(op)`, `op.perform` | the same: an operation, a program where it is bound |
| `Reader.run(r)(p): A ! F` | `reader(r)(p)` | okay's runner signature is one line over it: `def run[R, A, F[_]](r: R)(p: A ! Reader % R + F): A ! F = reader(r)(p)` |
| the last runner, `F = Nothing` | `state(s)(p)` | okay's `F` is `Nothing` by `Free`'s variance; a program here is fitted to a place, not covariant in its effects — the last handler is the standard one, or `F` given |
| `A ! Pure` | a value, `run(p)` | `Pure` is a row here, not an effect |
| `p.value` | `run(p)` | a program needing effects is a function of its place: no method on it; a program over a row named, `Eff[R, A]`, has `.run` at `Pure` |
| `F[+_]` | `F[_]` | a covariant effect is one too |
| `Handler`, `Answering` | `Handler`, `Answering` | over any row outside; made of other effects, the kernel's `Deep` over a row that has them |

Not here: okay-std's handlers by their okay names (`runChoice`, `Writer.fold`, …) — they are okay's library, written
over the kernel; the kernel's part is that their signatures compile.

An operation of an effect of two parameters, `effect(State.Put(1))`, names its effect —
`effect[State % Int, Unit](State.Put(1))`: from a case's type the compiler abstracts more than `State[Int, _]`; the
standard effects' own operations (`get`, `put`) need nothing.

## Programs with the row in their type: `RowEffects`

okay's `Effects[M[_ <: Row, _]]` — `M[R, A]`, `R` a row (`Reader % Int +: Writer % String +: Pure`) — is the
interface okay's own facade stands on. `okay.core.RowEffects[M]` is it, its signatures okay's, and okay's words over
them as members: `A ! R`, `effect(op)` and `op.perform` (the row the one the operation is bound into), `p.handle(h)`,
`p.value`; okay's other row syntax, `Pure + Ask + Say`, is `RowEffects.+`. The instance is `RowEffects.eff`, over
the kernel's program itself, `Eff[R, A]` — no `Program` built at a row under it. Its handlers are the facade's
`Handler` (`ret` a value, clauses over any row outside) and `Answering`, answered in place as the kernel's is; a
handler taking its effect off wherever it is in the row is the kernel's `Removed`.

```text
val E: RowEffects[Eff] = RowEffects.eff
import E.*
val counting: Int ! (State % Int +: Say +: Pure) =
  for
    n <- effect[State % Int, Int](State.Get())
    _ <- Say.Line(s"$n").perform
  yield n
Cont0.handle(counting)(State[Int, Say +: Pure, Int](41)).handle(writer[(Int, Int)]).value   // (List("41"), (41, 41))
```

okay's TestEffectsRows runs as it is written there (`TestAnywhere`). The interface costs nothing over the program
itself: `E.perform` is the kernel's `perform`, `E.flatMap` its `flatMap`.

okay names this instance beside its own (the machine, the tree) when it chooses to; okay's repository is not changed
by okay-core.
