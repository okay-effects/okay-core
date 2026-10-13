## one-monad, steps 6 and 7 - cats and tofu over Eff, Scala 2.13 reading the kernel

- `okay.interop.CatsProgram`: cats' `StackSafeMonad` over `Eff[R, *]`. `okay.interop.TofuProgram`: `WithLocal` and
  `Errors` from an effect anywhere in the row — by `okay.core.Intercept.at`: the effect handled again where it is,
  the row kept — the program STEPPED, an operation of the effect (the two paths the same, `Same` the proof, no cast)
  to the clause, any other performed again outside, the rest resumed; `k` data. The slow road, a run of the machine
  an operation, until the kernel has a relocation of its own for it (`Removed.Again`, a `View` keeping the row).
  TestTofu 7 (100 000 operations under a `local` and a `handleWith` in constant stack), TestInteropDoc, TestCatsProgram.
- Scala 2.13 (`scala2/`): `okay.core.Ops` — `perform`, `handle`, `run` and the paths as implicits, what the TASTy
  reader can read (not the kernel's inline `perform` and `In.here`, not its extension `handle` with unmergeable
  type parameters); the rows aliases, not exports (an export of a sealed trait crashes the reader); `Eff` written
  over `Cont0` (a top-level alias of okay.one's is invisible). `CatsProgram` and `TofuProgram` for Scala 2 over
  `Of[R]#L`; `TestOwnHandlerFromScala2` (the kernel's `Deep` written in Scala 2), `TestTofuFromScala2` — 6.
