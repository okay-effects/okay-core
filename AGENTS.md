# okay-core — notes for agents

Scala 3 (`build.sbt`: the version), sbt; tests with munit (`sbt test`: every suite on every platform — JVM, JS,
Native; `sbt coreJVM/test` the JVM's alone). Always read this file first; the design is in `docs/` (architecture.md
the map, one.md the decision and the gates, kernel.md the bridge, guide.md what a user reads — its `scala` blocks
are compiled and checked by `TestGuide`).

## The kernel, `okay.one` — THE KERNEL LIVES HERE (since 2026-10-14; it came from the okay-one repository, last at 45189cb)

- An effect framework and its kernel, the boundary between them DATA: a program is a tree of `Cont` nodes, a
  handler a value of one of its forms; the kernel an interpreter of them (`machine`: the fast one; `TestReference`:
  the meaning, the specification). Rows (`row`) and handlers (`handler`) know no interpreter.
- `Cont[R, I, O, A]`: `R` a row; `Control[T]` an effect in it, a `$` over `T` (`Control[T] +: T` = `Delim[T]`;
  `Reset` = `Delim[Pure]`); `I`, `O` the answers of the nearest `$` in `R` — a program of effects (`A ! R`) has none,
  `Nothing`. A shift0 is a node with its path to the `$` (`Nearest`), never past a `$`; a `$` over effects is a
  `reset`. One `Meta` (handlers' levels and `$`s), one loop over it, typed by the answers: a frame
  `Frame[R, X, I, V, J]`, a level `Meta[R, V, J, O, Z]`; the rest as data, `Rest` (`k`).
- Handlers leaving the rest (`Passing`: `Answering`, `Translating`, `Linear`; `Stateful`) are at any answers — their
  levels `Through`, `UnderState`; so is a guard (`UnderGuard`, as `Through`; a `Recovering` one on `!` only, `none`
  its evidence — what it recovers with is a program of effects, and a shift0 cannot carry the evidence past it);
  those taking it (`Taking`: `Aborting`, `Deep`) on `!` only — level `Under`, its
  answer `N` with `none: N =:= Nothing` (a literal `Nothing` there made the checker call reachable cases
  unreachable, under a GADT bound and a row). `handle` is `inline`, the kind by the static type of the handler,
  `summonFrom` for the `!` of a `Taking` one: overloads broke the inference of the handler's `T`. `flatMap` is
  `inline` too, the answers met by an inline given `Lifts` (`Down` first: a program of effects as the continuation —
  a bind as it was, one call, `down`): an overload broke the inference of `pure`'s row, a `Conversion` needs the
  feature import at its use.
- User code is called inside a `try` at its call, never around the loop: a throw is a `Raise` node, unwound to the
  top (`Step.Failed`). Keep it so for any new call of user code.
- The loop (`Machine.eval`) is JIT-sensitive: C2 inlines into it until its budget (eval's own bytecode plus the
  callees, ~8000) is spent — what comes later in the bytecode is left out of line, accessors included. Keep eval
  small (3693 bytes at the landing; 4362 at okay-one c7af924; 7033 was 60% slower), its cases inline, `found` at
  each operation case, state answered in the loop; a handler of a guarded call one call (`raised`), never
  `case NonFatal(e)`. Moving hot cases out to helpers made it slower; cold ones (`rare`, `foundOff`, `aborted`) out
  is right. Check with `-XX:+UnlockDiagnosticVMOptions -XX:+LogCompilation` (the inline tree of eval: what was
  clipped by `size > DesiredMethodLimit`) and A/B every change on `MixedBenchmark` (state) and the lanes.
- A handler is over the row `T` outside it; a `Deep` clause's `k` data, a program over `T`, kept and resumed anywhere.
  `handle`'s `T` unbounded (`& Row` at its uses): a bounded one is fixed to `Row` before `Removed` is searched.
- `k.failed(e)` (`Cont`): a clause answering an operation with a FAILURE, raised inside `k`'s levels — the program's
  own `recover` around the operation sees it (a transaction's handler, in okay's okay-tx, uses it). One type test on a `k`,
  `Machine.Fails`, of the class: the machine's `k` is one; a function (the reference's) raises where the clause is.
- No casts; no exception at run time for a program that type checks (`In.absurd` matches only `null`). The one
  exception, named: A PLACE BY KEY, `In.At(ref)` — the protocol's (okay-kernel's handlers are found by identity at
  run time), a case of `In` beside the paths; a handler says which keys are its own (`Handler.at`), `level` and
  `outE` ask it. Its two trusted steps are in `okay.one.row`: `Key.at` (one ref names one effect) and `In.At.again`
  (the row of a place by key is phantom). Nothing of the typed front makes one; the bridge (`okay.core.kernel`) does.
- `TestReference` checks the machine against the meaning on random programs: run it after any change to the machine.
- INTERCEPTORS — control and audit of effects (`TestIntercept`; okay-rs's `tests/intercept.rs` the same programs,
  keep them in step): an effect's author gives `Sensitive[E]`; `handle` asks `Plain[E]`, so a sensitive effect is
  handled only by `okay.one.std.Audit.handleAudited`, which needs `Audited[E]` in the row — opaque, the same
  operations at run time, no `In[E, …]` for it, so no program outside the audit performs `E`. `Audit.audit(i)(body)`:
  an `Interceptor`, its clauses given `Pass`, the only way on. `Audit.observe(o)(body)`: an `Observer` of every
  operation of every effect — a level of no effect (`Meta.UnderObserve`, taking the rest as `Under` does) that
  `level` does not pass, so the operation goes out by `outE` and stops there; `Audited.Marks` the row outside, its
  sensitive effects audited. `eval` untouched by it. `Intercept.at` (a plain effect anywhere, `Removed.Again`) is
  beside it, unchanged.
  okay-one's suites run on a 512k stack (`build.sbt`): a regression in the machine's stack use fails loudly.

## The facade, `okay.core`, `okay.core.std`

- `A ! Es` is `(c: Needs[Es]) ?=> Eff[c.R, A]`: a program fitted where it runs (`Place`, `Needs`, `Paths`); `Path`
  is found from the place first (docs/architecture.md). Two `@unchecked` matches, named there — no others.
  A rest named abstractly (`F` in `A ! Reader % R + F`) is fitted by the paths it came with (`Paths.rest`, under
  one to three handlers inside; `Needs.part` for `F` needed alone among `Choose + F`; `Paths.sub`), never by an
  `In` for `F`: `Needs.found` with `Req.Only` on a sum is a `MatchError` at the run (2026-10-14, `TestFacade`). A
  `Paths` given with a free `Fs` (`Needs[Fs]` searched with nothing in scope) is ambiguous at any place — keep the
  effects a given fits tied to its result type. `F`'s effects go to `F`'s handlers, the paths it came with: a
  handler the runner puts inside does not catch them.
- Handler bodies are deferred (`handleIn`): a handler installed by a recursive call costs no stack (`TestNesting`).
- The kernel's `flatMap` and `handle` are inline and decide the answers met (`Lifts`) and the handler's kind where
  they are written: never call them from a facade body whose answers or handler type are abstract — resolved there,
  only `Lifts.Up` fits and every bind gets a `Lift` (8333bf3: the mixed program 222 → 353 µs). Forward the evidence
  as an `inline` using parameter instead (`Op.flatMap`).
- The answer moved with no row named: `Atm[A, X, Y, Es]` is `(c: Delimited[Es]) ?=> Cont[c.R, Eff[c.T, X[c.T]], Eff[c.T, Y[c.T]], A]`
  — one context parameter (a function type may not have one parameter depend on another); `Delimited` a `Needs`
  with the row outside the nearest `$`, `T` — a `Needs` itself, so inside a `$` it is the one found, nearer and more
  specific than the place's outside (a `Needs.delimited` given beside `under` was ambiguous). `X` and `Y` are shapes
  over `T` (`Hole`, `Fixed`): a state in the answer needs both sides over the row outside. `Op.flatMap` binds into a
  rest at any answers (`Cont[R, I, O, B]`): a member that applies is never passed over for an extension, even when
  its lambda fails inside. `handleAt(h)(body)`: `Passing`/`Stateful` at answers, two overloads by the handler's
  class. `Atm.get`/`put` are steps not yet at their `$`, bound where used, as `Op`: a step's `F` (the run's value)
  from `Atm.Typestate[F]` given by `pstate`, never inferred — a receiver's type variables are instantiated before
  `.flatMap` is selected. `Atm.pstate` builds its `Delimited` by hand (`Delimited.under`, `Paths.under`): a search for
  `Paths[Es, Delim[c.R]]` with `Es` abstract would build `One` for a sum (`find` on it a `MatchError`).
  `Atm.pstate[Es](s0)(body)` names the effects: with `Es` inferred from the body scalac 3.9.0 dies (`wildApprox`
  failed to remove uninstantiated Es, in `ImplicitRunInfo.collectParts`) — on a member or a local, with or without
  the ascription; with `Es` given it does not.
  Tried and not kept: the `$` among the effects, `A ! Atm[X, Y] + Es` (2026-10-14)
  — `Y` must be a constructor over the row outside, and a match type on `Es` cannot return one; a match type on `c.T`
  instead (`Answer[Y, c.T]`) is not reduced where the body is checked against the alias (the compiler's stand-in for
  `c` is approximated to `Row` in it); and `!` itself cannot carry answers at all — `A ! Choose + F` with `F` abstract
  has no answers to be at, so `pure`/`ask` in it would not type. `Hole[X, A]`, `Fixed[A]` name the two shapes.
- Scala 2 reads this artifact through TASTy (`scala2/`): no inline, no opaque types in what it reads — `Ops`; no
  union in the result of a member of a class it extends (`Handler.at`: `In.Same[E, F]`, `null` all the same).

## `words/`

- `import okay.*` is the artifact `okay-core-words`, one package object over `okay.core` and `okay.core.std`: the core
  puts nothing at the top of the package `okay` (the okay platform's own `okay.*` would clash on one classpath).
  `TestFacade`, the guide's check (`TestGuide`, `TestGuideDoc`) are its tests: `sbt wordsJVM/test`.

## Benchmarks, commits

- `sbt "bench -prof gc"` the lanes and a table; `sbt "bench save -prof gc"` keeps the run in `bench.d`; the bar is
  docs/one.md §4 H (okay-core-zero's last run in bench.d). Never recompile while a JMH run is going, never run two
  JMH runs at once (other sessions run JMH too: wait for the lock). Compare by interleaved A/B.
- Commits: no Co-Authored-By lines; repository text in English.
