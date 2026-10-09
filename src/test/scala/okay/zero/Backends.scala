package okay.zero

import okay.core.{Backend, Delimited}

/** `Cont0`: `Fx` the context */
object ZeroBackend extends Backend:
  def run[A](body: (c: Delimited { type R = Pure; type S = A }) ?=> c.M[Pure, A, A, A]): A =
    Fx.run(body(using summon[Fx[Pure, A, A]]))

/** `Cont0` by the kernel alone, the reference: `FxRef` the context */
object RefBackend extends Backend:
  def run[A](body: (c: Delimited { type R = Pure; type S = A }) ?=> c.M[Pure, A, A, A]): A =
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
