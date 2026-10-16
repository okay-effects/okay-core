## one-interceptors - control and audit of effects: a sensitive effect only under its audit, an observer of all

- `Sensitive[E]`, given by an effect's author; `handle` asks `Plain[E]` (none for a sensitive effect, its message
  says what to do); `Audited[E]`, opaque — the same operations at run time, no `In[E, …]` outside its object, so no
  program outside an audit performs `E`; `handleAudited` takes `Audited[E]` off. (`okay.one.row`)
- `Audit.audit(i)(body)`: an `Interceptor` of a sensitive effect, its clauses given `Pass`, the only way on — each
  operation told, passed on as it is or changed, refused; its answer changed before `k` gets it.
  `Audit.intercept(h)(body)` for a plain effect at the head (`Intercept.at` anywhere, unchanged).
- `Audit.observe(o)(body)`: an `Observer` of every operation of every effect on its way out (`Seen`: `effect`,
  `describe`, `proceed` typed), its answer told each time the rest gets it, or refused; outside, the row with its
  sensitive effects audited (`Audited.Marks`). The machine: `Meta.UnderObserve`, a level of no effect not passed by
  `level`, so the operation goes out by `outE` and stops there (`Rest.UnderObserve` its `k`); `eval` untouched.
- The trail an effect: `Audit` (`record`, `trail`), handled outside the interceptors.
- Tests: `TestIntercept` — the audit of payments (a limit refused, an account normalized, masked in the trail), a
  plain intercept, every operation of a reader, a state and payments told with its answers, a read-only run, an
  answer told twice under a choice; five programs refused by the compiler. okay-rust's `tests/intercept.rs` the same.
- Measured: A/B against master, two rounds — the mixed program +1.0 %, the kernel's lanes −2.2 … +0.8 %: noise.
- Docs: `docs/intercept.md` (the account), the guide's *Control and audit* (checked by `TestGuideDoc`).
