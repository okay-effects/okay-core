## optional-modules - cats, tofu, and Scala 2's short names, modules of their own

- `interop/cats` (okay-core-cats, JVM, JS, Native): `okay.interop.CatsProgram`, cats' `StackSafeMonad` over
  `Program.Of[R]`; `TestCatsProgram` on every platform (laws on a sample, tailRecM, 100 000 left-nested binds).
- `interop/tofu` (okay-core-tofu, the JVM's — tofu has no JS or Native artifacts): `okay.interop.TofuProgram`,
  `WithLocal` and `Errors`; tofu's suites moved here from the core's tests (`TestTofuOn` on the four monads), and
  docs/guide.md's tofu example to docs/interop.md, checked by its `TestInteropDoc`. The core names no library and has
  no library in its tests now.
- `scala2/prelude` (okay-core-scala2-prelude): a package object compiled by scalac 2.13 — `Of[R]#L`, `ReaderOf[E]#L`,
  `StateOf`, `WriterOf`, `ThrowsOf`, `EmitOf`; `scala2/cats`, `scala2/tofu`: the same instances in Scala 2 against the
  2.13 artifacts; `scala2/tests` their suites. The two stdlibs' arrangement one function, `scala2Settings`.
