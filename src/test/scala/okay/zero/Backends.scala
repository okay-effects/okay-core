package okay.zero

// the rows and handlers, okay.core's: imported, not exported — Scala 2's TASTy reader fails on an export of a sealed trait
import okay.core.{Row, +:, Pure, In, :?, %, Sub, Req, Paths, Handler, Answering, Value}

import okay.core.{Backend, Effects}

/** `Cont0`: `At` the context */
object ZeroBackend extends Backend:
  def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A =
    okay.zero.run(body(using summon[At[Pure, A, A]]))

/** `Cont0` by the kernel alone, the reference: `Spec` the context */
object SpecBackend extends Backend:
  def run[A](body: (c: Effects { type Effs = Pure; type S = A }) ?=> c.Prog[A]): A =
    Spec.run(body(using summon[Spec[Pure, A, A]]))

/** master's tests of the effects, on `Cont0` */
class TestEffectsOnZero extends okay.core.TestEffectsOn(ZeroBackend)

/** master's tests of the context — reset, shift, `A ! Es` — on `Cont0` */
class TestContextOnZero extends okay.core.TestContextOn(ZeroBackend)

/** master's tests of `PState` on `Cont0` */
class TestPStateOnZero extends okay.core.TestPStateOn(ZeroBackend)

/** master's tests on the reference */
class TestEffectsOnSpec extends okay.core.TestEffectsOn(SpecBackend)
class TestContextOnSpec extends okay.core.TestContextOn(SpecBackend)
class TestPStateOnSpec extends okay.core.TestPStateOn(SpecBackend)

class TestTogetherOnZero extends okay.core.TestTogetherOn(ZeroBackend)
class TestTogetherOnSpec extends okay.core.TestTogetherOn(SpecBackend)

class TestDynamicOnZero extends okay.core.TestDynamicOn(ZeroBackend)
class TestDynamicOnSpec extends okay.core.TestDynamicOn(SpecBackend)

class TestControlOnZero extends okay.core.TestControlOn(ZeroBackend)
class TestControlOnSpec extends okay.core.TestControlOn(SpecBackend)

class TestInterceptOnZero extends okay.core.TestInterceptOn(ZeroBackend)
class TestInterceptOnSpec extends okay.core.TestInterceptOn(SpecBackend)
class TestNestingOnZero extends okay.core.TestNestingOn(ZeroBackend, deep = true)
class TestNestingOnSpec extends okay.core.TestNestingOn(SpecBackend, deep = true)
