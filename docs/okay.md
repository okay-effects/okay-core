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
| `effect(op)`, `op.perform` | `effect(op)`, `op.perform` | the same |
| `Reader.run(r)(p): A ! F` | `reader(r)(p)` | okay's runner signature is one line over it: `def run[R, A, F[_]](r: R)(p: A ! Reader % R + F): A ! F = reader(r)(p)` |
| the last runner, `F = Nothing` | `state(s)(p)` | okay's `F` is `Nothing` by `Free`'s variance; a program here is a function of its context, not covariant in its effects — the last handler is the standard one, or `F` given |
| `A ! Pure` | a value, `run(p)` | `Pure` is a row here, not an effect |
| `p.value` | `run(p)` | a program here is a context function: no method on it |
| `F[+_]` | `F[_]` | a covariant effect is one too |
| `Handler`, `Answering` | `Handler`, `Answering` | written in context, `c.Prog` the programs outside |

Not here: okay-std's handlers by their okay names (`runChoice`, `Writer.fold`, …) — they are okay's library, written
over the kernel; the kernel's part is that their signatures compile.

## Programs with the row in their type

okay's `Effects[M[_ <: Row, _]]` — `M[R, A]`, `R` a row (`Reader % Int +: Writer % String +: Pure`) — is the
interface okay's own facade stands on. okay-core's `Program[R, A]` is such a program, run on any of its monads:
okay's TestEffectsRows runs as it is written there, on `Cont`, `Cont0` and both references (`TestAnywhere`).
