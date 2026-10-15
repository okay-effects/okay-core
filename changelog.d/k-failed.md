## k-failed - a clause answering an operation with a failure

- `k.failed(e)` (`Cont`): the rest failed — `e` raised where the operation was, inside the levels `k` holds, so the
  program's own `recover` around the operation sees it, as a step's `fail`. What a handler answers when the
  operation has no value to be answered with: a transaction rolled back (okay's `okay-tx`, where transactions live
  — not the kernel's), a request refused. One type test on a `k`, `Machine.Fails`, of the class; a function (the
  reference's) raises where the clause is.
