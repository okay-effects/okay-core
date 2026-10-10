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
  def pure(a: Any): okay.kernel.Prog[Any] = Return[Any, Any](a)
  def bind(p: AnyRef, f: Any => AnyRef): okay.kernel.Prog[Any] = Bind[Any, Any, Any, Any, Any](c(p), f.asInstanceOf[Any => C])
  // a bind, not Cont0's `Map`: measured, the kernel's mixed program 250 µs so, 337 with `Map` (its `Mapped` frames)
  def map(p: AnyRef, f: Any => Any): okay.kernel.Prog[Any] = Bind[Any, Any, Any, Any, Any](c(p), (a: Any) => Return[Any, Any](f(a)))
  def delay(p: () => AnyRef): okay.kernel.Prog[Any] = Delay[Any, Any, Any](p.asInstanceOf[() => C])
  def perform(op: okay.kernel.Op[?]): okay.kernel.Prog[Any] = KOp[Any, Any, Any](op)
  def performThen(op: okay.kernel.Op[?], k: Any => AnyRef): okay.kernel.Prog[Any] = KOpK[Any, Any, Any](op, false, k.asInstanceOf[Any => C])
  def handle(h: KHandler, body: () => AnyRef): okay.kernel.Prog[Any] =
    Handle[Dyn, Pure, Any, Any, Any, Any](body.asInstanceOf[() => Cont0[Cont0[Cont0[Any, Any, Res[Pure, Any, Any]], Cont0[Any, Any, Res[Pure, Any, Any]], Res[Dyn +: Pure, Cont0[Any, Any, Res[Pure, Any, Any]], Any]], Cont0[Cont0[Any, Any, Res[Pure, Any, Any]], Cont0[Any, Any, Res[Pure, Any, Any]], Res[Dyn +: Pure, Cont0[Any, Any, Res[Pure, Any, Any]], Any]], Any]],
      h.asInstanceOf[okay.core.Handler[[X] =>> Cont0[Cont0[Any, Any, Res[Pure, Any, Any]], Cont0[Any, Any, Res[Pure, Any, Any]], X], Dyn, Any, Any]])
  def run(p: AnyRef): Any = runTop(p.asInstanceOf[Eff[Pure, Any, Any, Any, Any]])
  def owns(p: Any): Boolean = p.isInstanceOf[Cont0[?, ?, ?]]
  def node(p: AnyRef): okay.kernel.Prog[Any] = c(p)
  def fuse(p: AnyRef, k: Any => AnyRef): okay.kernel.Prog[Any] | Null = p match
    case o: KOp[?, ?, ?] => KOpK[Any, Any, Any](o.op, false, k.asInstanceOf[Any => C], true)
    case _ => null
