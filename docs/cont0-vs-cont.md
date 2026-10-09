# What `Cont0` does better than `Cont`

Both monads implement the same interface; a program written once runs on either and, but for one case below, gives
the same answer. They differ in what they are built on, in what they can express beyond the interface, and in
speed. This is where `Cont0` (`okay.zero`) is ahead of `Cont` (`okay.cont`) — and, to be fair, where it is not.

## 1. A more general foundation: shift0 and `$`

`Cont` is built on Danvy–Filinski's `shift` and `reset`. `Cont0` is built on Materzok–Biernacki's `shift0` and `$`,
which are more general:

- `shift` and `reset` are definable on them — `shift = shift0 ∘ reset0`, the body under a delimiter again — and that
  is how `Cont0` runs them. The converse does not hold: `shift0`, whose body runs *outside* the delimiter, is not
  expressible with one `reset` level.
- `shift0` reaches through delimiters one at a time — the CPS hierarchy without fixing its height. A `shift0` in a
  `shift0`'s body reaches the next reset out; with `shift`/`reset` that needs a second, different pair of operators.
- `$` takes any `ret`: a delimiter is a function of the body's value, not only the identity — a handler's `ret` *is*
  a `$`. A handler and a reset are one construct, a `$` whose answers are read, rather than two mechanisms.
- The kernel is four nodes (`Return`, `Bind`, `Shift0`, `Dollar`) and is open: `Cont0` is usable as a monad of its
  own, with raw `shift0` and `$`, beside the effects built on it.

## 2. The levels are in the types

Under a delimiter, `Cont0`'s answers are programs of the level outside, and that is in the types: a program's
type says how deep it is among the delimiters. Consequences, all checked by the compiler, without a cast:

- `shift0` under a handler does not compile — a handler's row is one longer than its level's.
- No raw `shift0` or `$` can catch an operation's request on its way to its handler, and no request can be made up
  or taken apart (`Eff` and `Level` are opaque).
- A `shift` that changes the answer type directly under a handler — where the handler's clauses could not apply to
  the rest — is told apart from one that keeps it (`same`).

`Cont`'s answers carry no level: simpler types, and none of these guarantees.

## 3. A `shift` under a handler: `k` holds the handler

With no `reset` between, a `shift`'s delimiter is the handler. Danvy–Filinski's `k` is the rest *with* the
delimiter, and `Cont0` keeps that: `k(x)` runs the rest under the handler — its `ret` and its clauses. An operation
in `k` is `k`'s own handler's, its clause with the rest of `k` only: a `raise` in `k` is that `k`'s `Left`, and the
shift's body goes on.

`Cont` gives `k` the handler's `ret` only, and leaves its operations to the shift body's handler: a `raise` in `k`
aborts the whole shift body. `Cont0`'s `k` is self-contained — it can be called anywhere, its effects already
handled — and answering handlers and general ones behave alike. (`TestMonads` pins the difference.)

## 4. Speed where effects are layered

`Cont` runs a handler as a fold over the program: an operation of an effect further out goes out through every
handler between, each re-folding the rest. `Cont0` runs a handler as a delimiter in a machine that knows, at run
time, the handlers in force: an operation of an answering handler is answered **in place**, across any number of
handlers and resets between.

The same programs, `MonadsBenchmark` (bench.d, 2026-10-09), ns an operation unless noted:

| | `Cont` | `Cont0` | |
|---|---|---|---|
| an operation across 1 handler | 11.7 | 5.7 | 2.0× |
| across 4 handlers | 22.8 | 6.8 | 3.4× |
| across 16 handlers | 90.4 | 17.2 | 5.3× |
| answered in place / by a general clause | 6.50 / 8.99 | 5.43 / 7.72 | 1.2× |
| state / writer / collect | 15.5 / 20.0 / 13.1 | 14.1 / 17.3 / 10.5 | 1.1–1.25× |
| 8 queens (a run) | 255 µs | 191 µs | 1.3× |
| a shift under a reset, 16 / 256 maps, `k` 8 times (a run) | 439 / 6050 ns | 323 / 4740 ns | 1.3× |
| a shift under a handler, 16 maps, `k` 8 times (a run) | 411 ns | 366 ns | 1.1× |

A resumed `k` passes its value through the captured frames in the machine's loop, with no program of the rest
rebuilt frame by frame — the deeper the rest, the larger the gain. Real programs layer effects (state under a
writer under a reader…), which is where the first rows matter.

## 5. A reference to check it against

`Cont0`'s machine is an optimization of a definition that exists as code: each delimiter is a `$` whose answers are
read. The machine falls back to it for what it does not take (`bail`), and the tests run every suite on the
definition alone (`Ref`) and on a textbook CPS of shift0 (`Cps0`) beside the machine. `Cont`'s fold has no such
executable definition beside it.

## Where `Cont` is better

- **Speed in three places**, by 3–17%: `choose` (29.2 against 34.3 ns), `throws` (13.7 against 14.4 ns), a shallow
  shift directly under a handler (122 against 139 ns a run). `Cont` folds a handler when the program is built;
  `Cont0` makes a delimiter node the machine enters — an entry, a frame stack, a resumption a little larger.
- **Simpler types.** No levels: a `Cont` program's type does not depend on how many delimiters it is under.
  Under effects still to be handled, `generate`'s consumer type can be generic over the monad for `Cont`; for
  `Cont0` a generic one would be recursive through the level, so the consumer names its own. At the top both give
  one `Stream` (`stream`).
- **A smaller implementation.** `okay.cont` is about 350 lines; `okay.zero` about 600 — the kernel, the machine,
  the levels.
- **Master's semantics** for a `shift` directly under a handler, if that is the one wanted.

## In short

`Cont0` is the more general foundation (shift0 and `$`, the CPS hierarchy, an open kernel), with stronger static
guarantees (levels in the types), a cleaner semantics for `shift` under a handler, and 2–5× faster effects where
handlers are layered. `Cont` is simpler and a little faster in a few shallow cases.
