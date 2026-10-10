# okay-interface - okay-core as an instance of okay's `Effects[M]`

okay's kernel `Effects[M[_ <: Row, _]]` — a program with its row in its type,
an operation by its path, `handle` taking an effect off wherever it is
(`Removed`). Its prototype over `Program` lives in TestAnywhere; it moves to main:

- `okay.core.RowEffects[M]`: okay's signatures (`pure`, `perform`, `defer`,
  `tailcall`, `flatMap`, `map`, `handle`, `run`) and okay's words over them
  (`A ! R`, `effect`, `op.perform`, `p.handle(h)`, `p.value`);
- `RowEffects.on(top)` over `Program`, a given on `Cont0`;
- okay's TestEffectsRows ported as it is written there, run on every monad;
- the bench: a program over the interface against the same in context.

okay's facade can then name okay-core's instance beside the machine and the
tree; okay's repository is not touched here.
