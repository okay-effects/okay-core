# ref-rename — `okay.async.Ref`, the reference monad `Spec`

- `okay.async.Swap` is `Ref`: a mutable cell, `get` and `compareAndSet` at once — the name every effect library
  gives it. Not an effect, and no STM over it: not needed yet.
- The test reference that had the name, `okay.zero.Ref` (`Cont0` by the kernel alone, each delimiter its
  definition), is `okay.zero.Spec`: the specification the machine is checked against — `SpecBackend`,
  `TestAnywhereOnSpec`, `TestNestingOnSpec`.

Names only, no code changed: no bench run. Tests green on the JVM, Scala.js, Scala Native and Scala 2.
