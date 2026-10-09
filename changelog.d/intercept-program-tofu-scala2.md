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
- Then measured and made faster (MonadsBenchmark, ns an operation, Cont / Cont0):
  - an interceptor on Cont0 a node of the machine (`Icpt`, `ByIntercept`): `n` asks under `local` 47.6 → 14.6
    (Cont 15.9–17.9; asks under the reader alone 4.4 / 4.6). At its path its clause; another operation on as a
    reset's; the handler at its path masked in `Ev`, so not answered in place past it; a shift up to it by the
    definition. `TestAgree` makes interceptors too — over 300 in its 1 000 programs — and the four monads agree.
  - `Program`: a leaf (a value, an operation) built at once, not delayed; a bind on a bind reassociated as it is
    made, up to 64 deep (`Program.Fused` — past it the closures composed would call each other a frame each: a
    left-nested chain of 100 000 overflowed without the bound). `state`'s loop as a `Program` 32.3 → 22.7 / 23.0,
    502 → 342 B, against 15.4 / 14.1 written in context.
- Then a handler of an effect anywhere in a row, okay's: `Removed[E, R]` (`Out`, and a translation `Tr` of the
  paths of `R` into `E +: Out`), a `View` — a context seen over another row, each operation's path translated, its
  delimiters' contexts views too. `Programs`' handlers take their effect off wherever it is
  (`Programs.reader(e)(p)`, `throws[E](p)`, `writer[W](p)`, `state(s0)(p)`), `Program.handle(h)(p)` for one's own.
  The interface's `Inner[X <: Row, …]` became `Within` and `Handling[E]`, so a view takes its context's monad and
  keeps its row. okay's `TestEffectsRows`, over okay's interface `Effects[M[_ <: Row, _]]` written over `Program`,
  and the handlers in every order, nested ones translated through both, `local` and `handleWith` in a view, 100 000
  operations deep in a row: `TestAnywhereOn` on Cont, Cont0, Ref and Cps0. Read by Scala 2 too.
- Measured, ns an operation (Cont / Cont0): `state`'s loop as a `Program`, the state first in the row 22.6 / 23.3
  (a view at the head translates nothing); third in the row, handled first 42.9 / 43.9, 679 B → 39.4 / 39.7, 487 B
  with a view's translations kept by the path's depth, checked by `In.same` — no object an operation. Kept by the
  path's identity it was worse (52 ns): a path is the compiler's given, made anew each call. What is left is the
  paths themselves, `There(There(Here()))` made by the given each call.
