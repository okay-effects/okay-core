package okay.cont

import okay.core.{Pure, +:}
import okay.core.kernel.{Dyn, KHandler, Machine}
import Cont.*

/** CONT AS THE KERNEL'S MACHINE (okay.core.kernel's `OnCore.cont`): its nodes made untyped — the kernel's programs
 * carry no row or answer types, okay-core's are phantom at run time: THE TRUSTED STEP of this object — and run on
 * Cont's top */
private[okay] object ContMachine extends Machine:
  private type C = Cont[Pure, Any, Any, Any]
  private def c(p: AnyRef): C = p.asInstanceOf[C]
  def pure(a: Any): AnyRef = Return[Pure, Any, Any](a)
  def bind(p: AnyRef, f: Any => AnyRef): AnyRef = Bind[Pure, Any, Any, Any, Any, Any](c(p), f.asInstanceOf[Any => C])
  def delay(p: () => AnyRef): AnyRef = Delay[Pure, Any, Any, Any](p.asInstanceOf[() => C])
  def perform(op: okay.kernel.Op[?]): AnyRef = KInject[Pure, Any, Any](op)
  def performThen(op: okay.kernel.Op[?], k: Any => AnyRef): AnyRef = KSuspend[Pure, Any, Any, Any](op, false, k.asInstanceOf[Any => C])
  def handle(h: KHandler, body: () => AnyRef): AnyRef =
    val ch = h.asInstanceOf[okay.core.Handler[[Z] =>> Cont[Pure, Any, Any, Z], Dyn, Any, Any]]
    Fold[Dyn, Pure, Any, Any, Any, Any](body.asInstanceOf[() => Cont[Dyn +: Pure, Any, Any, Any]], a => h.ret(a), ch,
      h.asInstanceOf[okay.core.Value[Dyn]])
  def run(p: AnyRef): Any = c(p).value
  def owns(p: Any): Boolean = p.isInstanceOf[Cont[?, ?, ?, ?]]
