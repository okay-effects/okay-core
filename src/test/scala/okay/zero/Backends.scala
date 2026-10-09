package okay.zero

import okay.core.{Backend, Effects}

/** `Cont0`: `Fx` the context */
object ZeroBackend extends Backend:
  def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A =
    Fx.run(body(using summon[Fx[Pure, A, A]]))

/** `Cont0` by the kernel alone, the reference: `FxRef` the context */
object RefBackend extends Backend:
  def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A =
    FxRef.run(body(using summon[FxRef[Pure, A, A]]))

/** master's tests of the effects, on `Cont0` */
class TestEffectsOnZero extends okay.core.TestEffectsOn(ZeroBackend)

/** master's tests of the context — reset, shift, `A ! Es` — on `Cont0` */
class TestContextOnZero extends okay.core.TestContextOn(ZeroBackend)

/** master's tests of `PState` on `Cont0` */
class TestPStateOnZero extends okay.core.TestPStateOn(ZeroBackend)

/** master's tests on the reference */
class TestEffectsOnRef extends okay.core.TestEffectsOn(RefBackend)
class TestContextOnRef extends okay.core.TestContextOn(RefBackend)
class TestPStateOnRef extends okay.core.TestPStateOn(RefBackend)

class TestTogetherOnZero extends okay.core.TestTogetherOn(ZeroBackend)
class TestTogetherOnRef extends okay.core.TestTogetherOn(RefBackend)

class TestDynamicOnZero extends okay.core.TestDynamicOn(ZeroBackend)
class TestDynamicOnRef extends okay.core.TestDynamicOn(RefBackend)

class TestControlOnZero extends okay.core.TestControlOn(ZeroBackend)
class TestControlOnRef extends okay.core.TestControlOn(RefBackend)

/** master's tests of `Async` on `Cont0`: `okay.zero.Async` its run */
class TestAsyncOnZero extends okay.core.TestAsyncOn:
  def runIO[A](body: (c: Effects { type Effs = okay.std.Async +: okay.std.Async.Fails; type S = Unit }) ?=> c.Prog[A]): Either[Throwable, A] =
    Fx.run(okay.std.throws[Throwable, A](Async.run(body(using summon[Async.Body]))))
