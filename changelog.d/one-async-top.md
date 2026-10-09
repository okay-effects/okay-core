## one-async-top - one Async for every monad; Top, a monad's re-entry

- `Top`: a monad's context at the top (over `Pure`, at `Unit`) and a run of a program of it — `okay.cont.top`,
  `okay.zero.top`, the references' in the tests. `Async.run` written once in okay.std, on the monad of the `Top`
  given; `okay.cont.Async`, `okay.zero.Async` gone. TestAsyncOn on Cont, Cont0, Ref and Cps0.
