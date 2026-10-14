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
Cont.handle(counting)(State[Int, Say +: Pure, Int](41)).handle(writer[(Int, Int)]).value   // (List("41"), (41, 41))
```

okay's TestEffectsRows runs as it is written there (`TestAnywhere`). The interface costs nothing over the program
itself: `E.perform` is the kernel's `perform`, `E.flatMap` its `flatMap`.

okay names this instance beside its own (the machine, the tree) when it chooses to; okay's repository is not changed
by okay-core.

## okay's `Effects[M]` over okay's own rows: `onokay/` (2026-10-14)

The other way in, tried as asked: okay keeps its vocabulary — `okay.cont.Row`, `Member`, `Removed`, `Handler` — and
okay-core gives the instance `okay.Effects[[R, A] =>> Eff[ToOne[R], A]]` by a MAPPING of rows (`okay.core.onokay.OnOkay`,
a module of tests: okay's jars as built from its repository in `onokay/lib`, not published — the coordinates
`dev.okay::okay-cont` are another library's in the local repository). `ToOne` is a match type, okay's list to the
kernel's; `Member` walks to `In`. okay's program over `Effects[M]`, generic in `M`, runs on the one monad and its
operations are answered by the kernel's handlers at the mapped row (`TestOnOkay`). What it found:

1. **Match types do not reduce under a GADT bound.** `ToOne[R]` with `R` known to be `E +: T` only by a match on
   the `Member` stays stuck, so the walk from `Member` to `In` — typed on each concrete shape — is given to the
   abstract row by ONE TRUSTED STEP (`OnOkay.trust`, the only cast, named), as okay-kernel's bridge has `trust` for
   the protocol's values. A mapping without it needs okay's rows to be the kernel's rows.
2. **okay's `Removed[E, R]` named no place** — its `Out` a type member, its witness of `E`'s place `route`, a
   function on an operation with its path, nothing a type reads. Mended in okay (branch `on-core`, 4abfcada2, one
   trait in okay-cont and its two givens): `Removed` carries `pos: Pos[E, R, Out]` — at the head, or past an effect
   — and the instance walks it to the kernel's `Removed[E, ToOne[R], ToOne[Out]]` as it walks `Member` (the same
   trusted step). A handler ANSWERING IN PLACE runs at any place in the row through okay's own `handle` now
   (`TestOnOkay`, the third).
3. **okay's `Handler` clauses are okay-cont's `Cont` programs** (`k: X => Cont[o.Here, o.Here, Ans]`): a machine of
   the one monad cannot run them; only an `Answering` (`value(op)`, no continuation) is expressible. The clauses
   written over `Effects[M]`'s `M` — `apply[R <: Row, X](op: E[X], k: X => M[R, Ans]): M[R, Ans]`, the facade's
   `Handler` here — is the change that lets okay's handlers (okay-std's) run on any encoding; it is the real
   migration, whichever way in is taken.

So the mapping stands for programs, operations and handlers answering in place, with one trusted step. What is
left is 3 — and 3 is not a tweak: okay-cont's `Free` is a function of a context into `Cont`, a clause's `k` is a
continuation of that context's stack, and a `Free` cannot wrap it losslessly (the context is its own). So clauses
over `Effects[M]` cannot run on okay-cont's machine either: writing okay's `Handler` over `M` means okay's programs
run on the one monad and okay-cont's machine is kept for its own tests, if at all — the switch of the platform's
kernel, not a step before it. Its size: 16 files define handlers in okay's own code (okay-cont 9, okay-std 4,
okay-async 2, okay-freer 1; three clause bodies use okay-cont's own words), plus okay's control interface
(`shift`/`reset` over `M[A, S, R]`, `Shift`, `replay`) and okay-cont's `Dialogue`, `Bang` to be given on `Shift % S`,
`Atm` and `Step`; the gate, okay's suites (1603 test files, every platform).
