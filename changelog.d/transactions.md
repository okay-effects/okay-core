## transactions - a transaction is a subtree: Tx, the bracket in the data

- A MODULE OF ITS OWN, `tx/` (`okay-core-tx`, `okay.tx`, cross-built as the core): transactions are not the kernel's
  std. `Tx[C, T, X]`: `Atomic(body)` a bracket that holds its body — brackets cannot mismatch, the closing
  one is the return from the node, no commit to forget; `Rollback(reason)` a leaf of no value. Inside a bracket the
  body has `Txs[C, T]` again and the resource `C` to read (`Reader % C`); `T` the row outside, the owner's.
- `Tx.Transactor[C, T]`: begin, commit, rollback as programs over `T`; `retry(e, attempt)` for deadlocks. The
  handlers `closed` and `open`, both `Deep`: the first bracket begins, its subtree runs under `open` where an inner
  bracket is its body and nothing more; the return commits, then the rest; a `Rollback` or any failure inside rolls
  back and fails the rest where the bracket was — `k.failed(e)`, new in `Cont`: a failure raised inside `k`'s levels,
  so a `recover` around the bracket sees it (`Tx.Rolled` for a `Rollback`, never retried); an abort or a cancel
  crossing the bracket rolls back (the guard's finalizer).
- The words, in the module: `atomic(body)`, `rollback(reason)`, `transactions(owner)(body)`.
- `TestTx` (7) and `TestTxWords`: brackets inside brackets one BEGIN one COMMIT, the resource read at any
  depth; a rollback in the inner bracket; a failure; an abort crossing; the owner's retry; a recover around the
  bracket; a rollback with none open.
- The JVM's own, `Jdbc[T](connect, isolation, readOnly, retries)`: a `Transactor` over a JDBC connection from a pool's
  `connect` — auto-commit off at the isolation, the driver's commit and rollback, the connection closed after either,
  a serialization failure or a deadlock (40001, 40P01) running the bracket again; `Jdbc.execute`, `Jdbc.queryInt`
  on the bracket's connection. `TestJdbc` on H2: nested brackets one connection one commit, a rollback leaves
  nothing, a 40001 runs the bracket again on a fresh connection, the isolation asked is the connection's. The
  platform's `Typed.transact` (okay-sql) is on okay-freer's words still: its owner on this module comes with the
  platform's move to the core's words.
