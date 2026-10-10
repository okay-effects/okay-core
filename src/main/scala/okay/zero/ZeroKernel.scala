package okay.zero

import okay.core.{Pure, +:}
import okay.core.kernel.{Dyn, KHandler, Machine}
import Cont0.*

/** CONT0 AS THE KERNEL'S MACHINE (okay.core.kernel's `OnCore.zero`): its nodes made untyped — the kernel's programs
 * carry no row or answer types, okay-core's are phantom at run time: THE TRUSTED STEP of this object — and run on
 * Cont0's top */
private[okay] object ZeroMachine extends Machine:
  private type C = Cont0[Any, Any, Any]
  private def c(p: AnyRef): C = p.asInstanceOf[C]
  def pure(a: Any): AnyRef = Return[Any, Any](a)
  def bind(p: AnyRef, f: Any => AnyRef): AnyRef = Bind[Any, Any, Any, Any, Any](c(p), f.asInstanceOf[Any => C])
  def delay(p: () => AnyRef): AnyRef = Delay[Any, Any, Any](p.asInstanceOf[() => C])
  def perform(op: okay.kernel.Op[?]): AnyRef = KOp[Any, Any, Any](op)
  def performThen(op: okay.kernel.Op[?], k: Any => AnyRef): AnyRef = KOpK[Any, Any, Any](op, false, k.asInstanceOf[Any => C])
  def handle(h: KHandler, body: () => AnyRef): AnyRef =
    Handle[Dyn, Pure, Any, Any, Any, Any](body.asInstanceOf[() => Cont0[Cont0[Cont0[Any, Any, Res[Pure, Any, Any]], Cont0[Any, Any, Res[Pure, Any, Any]], Res[Dyn +: Pure, Cont0[Any, Any, Res[Pure, Any, Any]], Any]], Cont0[Cont0[Any, Any, Res[Pure, Any, Any]], Cont0[Any, Any, Res[Pure, Any, Any]], Res[Dyn +: Pure, Cont0[Any, Any, Res[Pure, Any, Any]], Any]], Any]],
      h.asInstanceOf[okay.core.Handler[[X] =>> Cont0[Cont0[Any, Any, Res[Pure, Any, Any]], Cont0[Any, Any, Res[Pure, Any, Any]], X], Dyn, Any, Any]])
  def run(p: AnyRef): Any = runTop(p.asInstanceOf[Eff[Pure, Any, Any, Any, Any]])
  def owns(p: Any): Boolean = p.isInstanceOf[Cont0[?, ?, ?]]
