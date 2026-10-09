## intercept-program-tofu-scala2 - one design for okay's kernel, Scala 2, tofu and a small core

The requirements (docs/architecture.md, "What it is for"): a small core usable on its own; okay's kernel, whose
programs have the row in the type and whose handlers take an effect anywhere in a row; Scala 2.13; code written
over `F[_]` (tofu). They meet in two additions:

- `intercept`, in `Effects` (`Effects.intercept(h)(body)`): an effect already in the row handled again for a
  body — paths compared by `In.same`, whose `Same` proves the effects the same, no cast. A delimiter as a reset; a
  clause runs outside it. On `Cont` a fold at the same row; on `Cont0` a `$` whose answers are read
  (`interceptRead`), the machine not entered — `Ev.Unknown` inside it, so no operation is answered in place past it;
  on `Ref` and `Cps0` the same reading. `local` (Reader) and `handleWith` (Throws) on it. `TestInterceptOn` on the
  four monads: nested ones compose, an answering handler outside is not reached past it, 100 000 intercepted
  operations in constant stack.
- `Program[R, A]`: a program over the row `R` not yet built, `on(c)` in any context of that row — so built again
  under a delimiter, which an already built `c.Prog` cannot be (its answer type is the one outside). `F[A]` as
  `Program.Of[R]`; okay's `M[R, A]`; what Scala 2 writes. `flatMap` builds its left side by `delay`: a chain of
  100 000 left-nested binds in constant stack. `okay.std.Programs`: the standard effects as `Program`s.
- tofu 0.14.3 and cats 2.13.0 in the test scope: `Monad` (stack safe), `WithLocal`, `Errors` over `Program`
  (`okay.tofu.OnProgram`); `TestTofuOn` on Cont, Cont0, Ref and Cps0 — context, nested locals, raise, handleWith,
  attempt, restore, recover, monad laws on a sample, tailRecM.
- Scala 2.13.18: `scala2/`, a project compiled by scalac 2.13 with `-Ytasty-reader` against the core, two stdlibs
  in okay's order; the same tofu instances written in Scala 2 against tofu's 2.13 artifacts, on Cont0 and Cont.
  Found: Scala 2 reads the classes and objects, not top-level definitions; a type lambda alias (`%`) is read as a
  proper type, so an effect of two parameters is a Scala 2 type lambda; infix types of one precedence; okay.zero's
  `export` of the rows crashed the reader (`addChild inapplicable for type Row`) — imported instead. `sbt test`
  runs both.
