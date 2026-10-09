## zero-without-own-api - okay.zero only an instance of the interface, shift0 its addition

- okay.zero's own API gone — its Ctx, At, InReset, handle, reset, shift, perform, Needs and std: a program is written
  once over `Effects` and `okay.std` and run by `okay.cont.run` or `okay.zero.run`. `shift0` on a reset's context of
  `okay.zero.At` — under a handler it does not compile (a handler's row is one longer than its level's).
- A master's handler is okay.zero's as it is (`Handler`/`Answering` one trait): no adapter at its install.
- Tests: the copies of master's tests gone; the suites written once (`TestEffectsOn`, `TestContextOn`,
  `TestControlOn`, `TestTogetherOn`, `TestDynamicOn`, `TestPStateOn`, `TestAsyncOn`) take a `Backend` and run on
  each monad.
- Measured: through the interface Cont0 as fast as through its own API before (throws 14.4 µs, the adapter gone).
