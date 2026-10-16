# Control and audit of effects: interceptors

THE POINT OF IT ALL. A program's type says what effects it may perform; an interceptor makes each of those effects
pass a point of control at run time — told to an audit trail, checked against a policy, changed, or refused — and the
types make sure that a SENSITIVE effect cannot be performed or handled anywhere but under such a point. okay-audit
checks the boundary of effects in the bytecode at every build (`okay/docs/modules/okay-audit.md`); the interceptors
are its other half, at run time, in the kernel.

Everything here is the kernel's, `okay.one`: `okay.one.row` (`Sensitive`, `Plain`, `Audited`), `okay.one.handler`
(`Interceptor`, `Pass`, `Observer`, `Seen`), `okay.one.std.Audit` (`audit`, `handleAudited`, `intercept`, `observe`,
`record`, `trail`). The programs below are `TestIntercept`'s (src/test/scala/okay/one), run there; okay-rust
(`~/work/my/okay-rust`, `tests/intercept.rs`) has the same programs in Rust. The guide's section *Control and audit*
has a short one, checked by `TestGuideDoc`.

## Contents

1. [Three interceptors](#three-interceptors)
2. [Sensitive effects and the guarantee](#sensitive-effects-and-the-guarantee)
3. [The audit: `Interceptor`, `Pass`, `handleAudited`](#the-audit-interceptor-pass-handleaudited)
4. [A plain effect intercepted](#a-plain-effect-intercepted)
5. [The observer: every effect](#the-observer-every-effect)
6. [The trail](#the-trail)
7. [Where an interceptor stands, what it sees](#where-an-interceptor-stands-what-it-sees)
8. [The machine](#the-machine)
9. [Cost](#cost)
10. [Limits, and what is next](#limits-and-what-is-next)
11. [The same in Rust: okay-rust](#the-same-in-rust-okay-rust)

## Three interceptors

| | what it intercepts | what it can do | the row outside |
|---|---|---|---|
| `Audit.intercept(h)(body)`, `Intercept.at(in)(fa)(clause)` | one PLAIN effect, at the head of the body's row / anywhere in it | anything a `Deep` handler does: tell, pass on (its clause's own operation of the effect goes to the handler further out), change the operation or its answer, not resume | the same |
| `Audit.audit(i)(body)` | one SENSITIVE effect `S` | tell, pass on by `Pass` (the only way), change the operation or its answer, refuse | `S` turned `Audited[S]` |
| `Audit.observe(o)(body)` | every operation of every effect on its way out of `body` | tell what it is and its answer, pass it on as it is (`proceed`), refuse | every sensitive effect turned audited |

The first two are typed: the clause sees the operation as its enum's case and its answer at its type. The observer
sees any operation, so as a description (`effect`, `describe`) and a typed way on (`proceed`, its answer `X`).

## Sensitive effects and the guarantee

An effect is SENSITIVE when its author says so — a given, next to the enum:

```scala
enum Pay[X]:
  case Transfer(to: String, amount: Int) extends Pay[String]
  case Balance() extends Pay[Int]
given Sensitive[Pay] = Sensitive()
```

From then on:

- **`handle` refuses it.** `handle` asks `Plain[E]`, which exists only when no `Sensitive[E]` does; the message says
  what to do:

  ```
  a sensitive effect is handled only under its audit: audit it first (`Audit.audit` with an interceptor of it, or
  `Audit.observe`), then handle it with `handleAudited`
  ```

- **`handleAudited` takes it only from `Audited[Pay]`**, which only an audit or an observer puts in the row.
  `transfer[Pay +: Pure]("a", 1).handleAudited(bank(10))` does not compile: no `Removed[Audited[Pay], Pay +: Pure, …]`.
- **`intercept` refuses it** as `handle` does (`Plain[Pay]`): an interceptor of a sensitive effect is an audit.
- **Outside its audit no program performs it.** The row there has `Audited[Pay]`, and `Audited[E]` is OPAQUE —
  `type Audited[E[_]] = [x] =>> Audited.Op[E, x]`, `opaque type Op[E[_], X] = E[X]`: outside its object it is not
  `Pay`, so there is no `In[Pay, R]` for it, and `transfer[Outside]("a", 1)` does not compile ("No given instance of
  type In[Pay, Outside]"). Inside its object it IS `Pay`: the same operations at run time, the same paths, the same
  handler — `Audited.in` and `Audited.removed` turn its evidence into `Pay`'s, no cast.

So the only programs that run (`run` takes `Pure`) are those whose every sensitive operation passed an audit or an
observer. Each refusal is a test (`TestIntercept`, "the guarantee, by types"), checked by its message.

## The audit: `Interceptor`, `Pass`, `handleAudited`

```scala
abstract class Interceptor[S[_], T <: Row, A, B]:
  def ret(a: A): B ! T
  def apply[X](op: S[X], k: X => B ! T, pass: Pass[S, T]): B ! T

final class Pass[S[_], T <: Row]:          // made by `audit` alone
  def apply[X](op: S[X]): X ! T             // `op` performed past the audit, to the handler of `Audited[S]`

def audit[S[_], T <: Row, A, B](i: Interceptor[S, T, A, B])(body: A ! (S +: T))
    (using Sensitive[S], In[Audited[S], T]): B ! T

extension [R <: Row, A](p: A ! R)
  def handleAudited[S[_], T, B](h: Handler[S, T & Row, A, B])
      (using Removed[Audited[S], R, T & Row], Sensitive[S]): B ! (T & Row)
```

The body is over `S +: T`: its operations of `S` go to the interceptor, each with `k`, the rest of the body under the
interceptor again. `T`, the row outside, has `Audited[S]` in it. The clause runs OUTSIDE the interceptor, over `T`;
`pass(op)` performs `op` there — to the handler `handleAudited` installed — and its value is the operation's: the
clause gives it, or another, to `k`. A clause can:

- **tell**: `record[T](line)` before and after;
- **pass on as it is**: `pass(op).flatMap(k)`;
- **change the operation**: `pass(Pay.Transfer(to.trim.toUpperCase, amount))` — normalized, capped, tagged;
- **change the answer**: `pass(op).flatMap(r => k(redact(r)))`;
- **refuse**: not call `pass`, not resume `k` — raise (`Throws.raise`), or answer for the whole body (`ret`'s type);
- **resume `k` more than once**, keep it, drop it — it is a `Deep` handler's `k`, data.

The audit of payments, `TestIntercept`'s:

```scala
def masked(account: String): String = "****" + account.takeRight(4)

def guarded[T <: Row, A](limit: Int)(using In[Throws % String, T], In[Audit, T]) = new Interceptor[Pay, T, A, A]:
  def ret(a: A) = pure(a)
  def apply[X](op: Pay[X], k: X => A ! T, pass: Pass[Pay, T]): A ! T = op match
    case Pay.Transfer(to0, amount) =>
      val to = to0.trim.toUpperCase                                  // the account written one way: changed
      if amount > limit then                                         // the policy: refused, raised, told
        val why = s"refused: $amount to ${masked(to)} over the limit $limit"
        record[T](why).flatMap(_ => Throws.raise[String, T](why))
      else for
        receipt <- pass(Pay.Transfer(to, amount))                    // passed on
        _ <- record[T](s"transfer $amount to ${masked(to)} → $receipt")   // told, masked
        a <- k(receipt)
      yield a
    case Pay.Balance() =>
      for
        b <- pass(Pay.Balance())
        _ <- record[T](s"balance → $b")
        a <- k(b)
      yield a

type Outside = Audited[Pay] +: Throws % String +: Audit +: Pure

def payments[R <: Row](using In[Pay, R]): Int ! R = for
  _ <- transfer[R](" acct-1234 ", 100)
  b <- balance[R]
  _ <- transfer[R]("acct-77", 5000)
yield b

audit(guarded[Outside, Int](1000))(payments[Pay +: Outside])
  .handleAudited(bank(1000)).handle(Throws.either).handle(Audit.trail).run
// (List("transfer 100 to ****1234 → ok:ACCT-1234:100",
//       "balance → 900",
//       "refused: 5000 to ****T-77 over the limit 1000"),
//  Left("refused: 5000 to ****T-77 over the limit 1000"))
```

The program `payments` is written over any row with `Pay` in it and knows nothing of the audit; it is instantiated
at `Pay +: Outside` for it. `bank` is an ordinary `Stateful` handler of `Pay`: what makes it sensitive is
`handleAudited`, not the handler.

## A plain effect intercepted

A plain effect needs no audit to be handled; it may still be intercepted, for a part of a program:
`Audit.intercept(h)(body)` with `body` over `E +: T`, `E` in `T` too (the facade's `intercept` the same), or
`Intercept.at(in)(fa)(clause)` for `E` anywhere in `fa`'s row (`Removed.Again`, the kernel's). The clause runs
outside: its own operation of `E` reaches the handler further out.

```scala
type T = Writer % String +: Pure
def shouting[A] = new Deep[Writer % String, T, A, A]:
  def ret(a: A) = pure(a)
  def apply[X](op: Writer[String, X], k: X => A ! T) = op match
    case Writer.Tell(s) => Writer.tell[String, T](s.toUpperCase).flatMap(_ => k(()))

Writer.tell[String, T]("a").flatMap(_ => Audit.intercept(shouting[Unit])(Writer.tell[String, Writer % String +: T]("b")))
  .handle(Writer.collect).run                     // (List("a", "B"), ())
```

## The observer: every effect

```scala
abstract class Observer[T <: Row, A, B]:
  def ret(a: A): B ! T
  def apply[E[_], X](op: Seen[E, X, T], k: X => B ! T): B ! T

final class Seen[E[_], X, T <: Row]:
  val op: E[X]
  def effect: String        // the enum's name: "Pay", "State"
  def describe: String      // the operation's toString: "Transfer(acct,6)", "Put(6)"
  def proceed: X ! T        // performed outside the observer, to its handler — its value typed

def observe[R <: Row, T <: Row, A, B](o: Observer[T, A, B])(body: A ! R)(using Audited.Marks[R, T]): B ! T
```

Every operation of `body` that goes out of it — whatever its effect — stops at the observer first: its clause gets it
seen and the rest `k`. `proceed` performs it outside the observer (its place in the row is the same there: the
observer adds no effect), and the clause gives its value, or another of its type, to `k`. `T` is `R` with each
sensitive effect audited (`Audited.Marks[R, T]`, found by givens: a `Sensitive[E]` marks `E`, none leaves it) — an
observer is an audit of every effect at once, so `handleAudited` takes the sensitive ones after it.

Every operation and its answer, told:

```scala
def telling[T <: Row, A](using In[Audit, T]) = new Observer[T, A, A]:
  def ret(a: A) = pure(a)
  def apply[E[_], X](op: Seen[E, X, T], k: X => A ! T): A ! T = for
    _ <- record[T](s"${op.effect} ${op.describe}")
    x <- op.proceed
    _ <- record[T](s"  → $x")
    a <- k(x)
  yield a

type R = Reader % Int +: State % Int +: Pay +: Audit +: Pure
type T = Reader % Int +: State % Int +: Audited[Pay] +: Audit +: Pure
val p: String ! R = for
  n <- Reader.ask[Int, R]
  _ <- State.modify[Int, R](_ + n)
  s <- State.get[Int, R]
  r <- transfer[R]("acct", s)
yield r
observe(telling[T, String])(p).handle(Reader(5)).handle(State(1)).handleAudited(bank(100)).handle(Audit.trail).run
// (List("Reader Ask()", "  → 5", "State Get()", "  → 1", "State Put(6)", "  → ()",
//       "State Get()", "  → 6", "Pay Transfer(acct,6)", "  → ok:acct:6"), (6, "ok:acct:6"))
```

A policy over every effect — a read-only run, where no operation of `Pay` passes:

```scala
def readOnly[A] = new Observer[T, A, A]:
  def ret(a: A) = pure(a)
  def apply[E[_], X](op: Seen[E, X, T], k: X => A ! T): A ! T =
    if op.effect == "Pay" then
      val why = s"read-only: ${op.describe} refused"
      record[T](why).flatMap(_ => Throws.raise[String, T](why))
    else op.proceed.flatMap(k)
```

**An answer is told each time the rest gets it.** The frames after `proceed` are in the rest of whatever handler
answers the operation; a handler that resumes its `k` twice runs them twice:

```scala
type R = Choose +: Audit +: Pure
observe(telling[R, Boolean])(Choose.flip[R]).handle(Choose.all).handle(Audit.trail).run
// (List("Choose Among(List(true, false))", "  → true", "  → false"), List(true, false))
```

## The trail

`Audit` is an ordinary plain effect: `Record(line: String)`, `record[R](line)`, and `trail`, a `Stateful` handler
keeping the lines in order beside the value. It is handled OUTSIDE the interceptors — their clauses run outside
them, so `record` there reaches it — and anything can handle it instead: a handler that writes to a log, a database,
a message queue, or checks the trail against an expected one. The trail is the run's value in a test, not a side
effect.

The observer does not see the trail's own records: they are performed by its clause, outside it.

## Where an interceptor stands, what it sees

An operation goes out from where it is performed, level by level, to its handler. An interceptor sees exactly the
operations that pass it on their way:

- an operation handled by a handler INSIDE the interceptor never reaches it — put the observer innermost (applied
  first) to see every effect of the program, or around a part of it to see that part;
- a clause of a handler inside, performing an effect of the row outside, passes the interceptor — the clause runs at
  its handler's level, outside the handler;
- a rest `k` taken past an interceptor holds it: resumed, the interceptor is back, and sees the rest's operations
  again (`Rest.UnderObserve` is a level of `k`);
- an interceptor's own clause runs outside it: its operations — `record`, `pass`, `proceed` — are not seen by it,
  and reach the next interceptor out, if any. Interceptors nest: an audit inside an observer is seen by it.

A throw in a clause is the kernel's `Raise`, as anywhere (`TestRaise`): the levels unwound, guards' finalizers run.

## The machine

The audit and `intercept` are `Deep` handlers — the machine's level `Under`, nothing new. The observer is a level of
its own, `Meta.UnderObserve[T, V, B, N, …]` — the row the same in it (no effect of its own, as a guard's), no
answer (`N`, `none: N =:= Nothing`: it takes the rest, as `Under` does), its body a program of effects:

- `level(in, m)`, the look-up of an operation's handler by its path, does not pass it (its `passed` is `null`): an
  operation meeting it is not answered in place, it goes out by `outE`;
- `outE` puts each level it passes around the rest, as for any handler taking `k`; at `UnderObserve` it stops and
  calls the clause (`observing`) with the operation seen (`Seen(op, in)` — `in` its place in the row, the same
  outside) and `k` (`Rest.UnderObserve`, the level put back when `k` is resumed);
- `unwind`, `bottom` (its `ret`), `resumed`, `failIn`, `masked`, `shifted` know it (a shift0 cannot pass it: no
  answer there, as under `Under`);
- `eval`, the loop, is untouched: the node `Observed` is entered in `rare`, the operation found in `outE`.

The path cache (`lookUp`, `Found`) is unaffected: an observer between an operation and its handler makes `level`
return `null`, never cached.

## Cost

`Meta.UnderObserve` costs nothing where there is no observer: `eval` is the same bytecode, the cases are on cold
paths. A/B, okay-core master 7ace816/97b55b1 against the branch with the interceptors, two interleaved rounds (µs):

| | before | after | |
|---|---|---|---|
| `MixedBenchmark.orders` (state) | 221.4 | 223.6 | +1.0% |
| `KernelBenchmark.answered` | 5.33 | 5.21 | −2.2% |
| `forwarded16` | 5.77 | 5.73 | −0.6% |
| `state` | 12.12 | 12.22 | +0.8% |
| `choose` | 12.94 | 13.00 | +0.5% |
| `general` | 9.36 | 9.41 | +0.5% |
| `throwing` | 20.38 | 20.26 | −0.6% |
| `capture` | 0.937 | 0.933 | −0.4% |

All within the noise. Where there IS an interceptor, each operation it sees costs what a `Deep` handler's does: the
rest taken as `k`, a level a time from the operation to the interceptor, and put back when resumed.

## Limits, and what is next

- The API is the kernel's (`okay.one.std.Audit`, over `A ! R` / `Eff[R, A]`); the facade's `A ! Es` has no `audit`
  or `observe` of its own yet.
- `Seen.effect` is the operation enum's simple name, `describe` its `toString`: enough for a trail, not a schema.
  A structured record (the effect, the case, its fields, the answer) is a natural next step.
- The observer is typed by the answer, not by the case: it can pass on, refuse, or give another answer of the same
  type; to rewrite an operation, use a typed interceptor of its effect.
- An observer takes the rest, so it is around a program of effects (`!`) only — not under a `$` whose answer a
  shift0 inside moves.
- `Sensitive[E]` is given by the effect's author; an effect defined elsewhere is made sensitive by a given in scope
  where it is handled — the guarantee holds where that given is.

## The same in Rust: okay-rust

okay-rust (`~/work/my/okay-rust`, its `tests/intercept.rs`) has the same five programs and the same refusals, typed the
Rust way:

| okay-core | okay-rust |
|---|---|
| `given Sensitive[Pay]` | `impl Effect for Pay { type Kind = Sensitive; }` |
| `Plain[E]`, its message | `E::Kind: PlainKind`, `#[diagnostic::on_unimplemented]` |
| `Audited[E]`, opaque | `Audited<E>`, no `Effect` impl, `Mark<E, Ix>` puts it in the row |
| `audit(i)(body)`, `Pass` | `p.audit::<S, _, _>(i)`, `Pass<S>` |
| `handleAudited` | `handle_audited` |
| `observe(o)(body)`, `Seen` | `p.observe(o)`, `AnyOp` (`effect`, `describe` by `Debug`, `proceed`, `proceed_then`) |
| `Audited.Marks` | `AuditAll` |
| `Audit`, `record`, `trail` | `okay::audit::{Audit, record, trail}` |
