package okay.zero

// the rows and handlers, okay.core's: imported, not exported — Scala 2's TASTy reader fails on an export of a sealed trait
import okay.core.{Row, +:, Pure, In, :?, %, Sub, Req, Paths, Handler, Answering, Value}

import okay.core.{Backend, Effects}

/** `Cont0`: `At` the context */
object ZeroBackend extends Backend:
  def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A =
    okay.zero.run(body(using summon[At[Pure, A, A]]))

/** `Cont0` by the kernel alone, the reference: `Ref` the context */
object RefBackend extends Backend:
  def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A =
    Ref.run(body(using summon[Ref[Pure, A, A]]))

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

class TestInterceptOnZero extends okay.core.TestInterceptOn(ZeroBackend)
class TestInterceptOnRef extends okay.core.TestInterceptOn(RefBackend)
class TestNestingOnZero extends okay.core.TestNestingOn(ZeroBackend, deep = true)
class TestNestingOnRef extends okay.core.TestNestingOn(RefBackend, deep = true)
